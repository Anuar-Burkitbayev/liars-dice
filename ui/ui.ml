open! Core
open Logic_library
open Hw2
open Hw4
open Virtual_dom
open Js_of_ocaml
open Async_kernel
open! Bonsai.Let_syntax
module Node = Vdom.Node
module Attr = Vdom.Attr
module Effect = Vdom.Effect

module Multiplayer = struct
  let firebase_api_key = "AIzaSyDzpJvn3EOxsm9EZPpNwaRDIrEXdsEGp7I"
  let project_id = "liars-dice-3fd4e"

  let base_url =
    Printf.sprintf
      "https://firestore.googleapis.com/v1/projects/%s/databases/(default)/documents"
      project_id
  ;;

  let game_document_path game_id = Printf.sprintf "games/%s" game_id

  let game_document_url game_id =
    Printf.sprintf "%s/%s?key=%s" base_url (game_document_path game_id) firebase_api_key
  ;;

  let state_update_url game_id =
    Printf.sprintf
      "%s/%s?updateMask.fieldPaths=state&key=%s"
      base_url
      (game_document_path game_id)
      firebase_api_key
  ;;

  let queue_document_path = "matchmaking/waiting"

  let queue_document_url () =
    Printf.sprintf "%s/%s?key=%s" base_url queue_document_path firebase_api_key
  ;;

  let generate_game_id () =
    Random.self_init ();
    let timestamp = int_of_float (Js.to_float (new%js Js.date_now)##getTime) in
    let random_suffix = Random.int 10_000 in
    Printf.sprintf "%d-%04d" timestamp random_suffix
  ;;

  let generate_client_id () =
    Random.self_init ();
    let alphabet = "abcdefghijklmnopqrstuvwxyz0123456789" in
    String.init 16 ~f:(fun _ -> alphabet.[Random.int (String.length alphabet)])
  ;;

  let parse_document response_text =
    try
      let json = Js.Unsafe.global##._JSON##parse response_text in
      let fields = Js.Unsafe.get json "fields" in
      if not (Js.Optdef.test (Js.Optdef.return fields))
      then Ok None
      else (
        let state_field = Js.Unsafe.get fields "state" in
        let state_res =
          if Js.Optdef.test (Js.Optdef.return state_field)
          then (
            let sv = Js.Unsafe.get state_field "stringValue" in
            if Js.Optdef.test (Js.Optdef.return sv)
            then (
              let s = Js.to_string sv in
              try Ok (Some (Sexp.of_string s |> Game.t_of_sexp)) with
              | exn ->
                Error (Printf.sprintf "Parse game sexp failed: %s" (Exn.to_string exn)))
            else Ok None)
          else Ok None
        in
        match state_res with
        | Error _ as e -> e
        | Ok maybe_state ->
          let starter =
            try
              let starter_field = Js.Unsafe.get fields "starter" in
              if Js.Optdef.test (Js.Optdef.return starter_field)
              then (
                let iv = Js.Unsafe.get starter_field "integerValue" in
                if Js.Optdef.test (Js.Optdef.return iv)
                then (
                  let s = Js.to_string iv in
                  match Int.of_string_opt s with
                  | Some i -> Some i
                  | None -> None)
                else None)
              else None
            with
            | _ -> None
          in
          let player1 =
            try
              let p1_field = Js.Unsafe.get fields "player1" in
              if Js.Optdef.test (Js.Optdef.return p1_field)
              then (
                let sv = Js.Unsafe.get p1_field "stringValue" in
                if Js.Optdef.test (Js.Optdef.return sv)
                then Some (Js.to_string sv)
                else None)
              else None
            with
            | _ -> None
          in
          let player2 =
            try
              let p2_field = Js.Unsafe.get fields "player2" in
              if Js.Optdef.test (Js.Optdef.return p2_field)
              then (
                let sv = Js.Unsafe.get p2_field "stringValue" in
                if Js.Optdef.test (Js.Optdef.return sv)
                then Some (Js.to_string sv)
                else None)
              else None
            with
            | _ -> None
          in
          Ok (Some (maybe_state, starter, player1, player2)))
    with
    | exn -> Error (Printf.sprintf "Parse error: %s" (Exn.to_string exn))
  ;;

  let fetch_document_async ~game_id
    : ( (Game.t option * int option * string option * string option) option
        , string )
        Result.t
        Deferred.t
    =
    let ivar = Ivar.create () in
    let xhr = XmlHttpRequest.create () in
    xhr##_open (Js.string "GET") (Js.string (game_document_url game_id)) Js._true;
    xhr##.onreadystatechange
    := Js.wrap_callback (fun _ ->
         match xhr##.readyState with
         | XmlHttpRequest.DONE ->
           let status = xhr##.status in
           if status = 404
           then Ivar.fill ivar (Ok None)
           else if status >= 200 && status < 300
           then (
             let resp = Js.Opt.case xhr##.responseText (fun () -> "{}") Js.to_string in
             match parse_document (Js.string resp) with
             | Ok parsed -> Ivar.fill ivar (Ok parsed)
             | Error msg -> Ivar.fill ivar (Error msg))
           else Ivar.fill ivar (Error (Printf.sprintf "Failed to fetch game: %d" status))
         | _ -> ());
    ignore (xhr##send Js.null);
    Ivar.read ivar
  ;;

  let fetch_document_effect ~game_id =
    Bonsai_web.Effect.of_deferred_fun (fun () -> fetch_document_async ~game_id) ()
  ;;

  let save_game_state_async ~game_id ~game_state : (unit, string) Result.t Deferred.t =
    let ivar = Ivar.create () in
    let xhr = XmlHttpRequest.create () in
    xhr##_open (Js.string "PATCH") (Js.string (state_update_url game_id)) Js._true;
    xhr##setRequestHeader (Js.string "Content-Type") (Js.string "application/json");
    let game_state_sexp = Game.sexp_of_t game_state |> Sexp.to_string |> String.escaped in
    let body =
      Printf.sprintf {|{"fields":{"state":{"stringValue":"%s"}}}|} game_state_sexp
    in
    xhr##.onreadystatechange
    := Js.wrap_callback (fun _ ->
         match xhr##.readyState with
         | XmlHttpRequest.DONE ->
           let status = xhr##.status in
           if status >= 200 && status < 300
           then Ivar.fill ivar (Ok ())
           else Ivar.fill ivar (Error (Printf.sprintf "Failed to save state: %d" status))
         | _ -> ());
    ignore (xhr##send (Js.Opt.return (Js.string body)));
    Ivar.read ivar
  ;;

  let save_game_state_effect ~game_id ~game_state =
    Bonsai_web.Effect.of_deferred_fun
      (fun () -> save_game_state_async ~game_id ~game_state)
      ()
  ;;

  let create_game_async ~game_id ~game_state ~starter ~player1_id ~player2_id
    : (unit, string) Result.t Deferred.t
    =
    let ivar = Ivar.create () in
    let xhr = XmlHttpRequest.create () in
    let url =
      Printf.sprintf "%s/games?documentId=%s&key=%s" base_url game_id firebase_api_key
    in
    xhr##_open (Js.string "POST") (Js.string url) Js._true;
    xhr##setRequestHeader (Js.string "Content-Type") (Js.string "application/json");
    let game_state_sexp = Game.sexp_of_t game_state |> Sexp.to_string |> String.escaped in
    let body =
      Printf.sprintf
        "{\"fields\":{\"state\":{\"stringValue\":\"%s\"},\"starter\":{\"integerValue\":\"%d\"},\"player1\":{\"stringValue\":\"%s\"},\"player2\":{\"stringValue\":\"%s\"}}}"
        game_state_sexp
        starter
        (String.escaped player1_id)
        (String.escaped player2_id)
    in
    xhr##.onreadystatechange
    := Js.wrap_callback (fun _ ->
         match xhr##.readyState with
         | XmlHttpRequest.DONE ->
           let status = xhr##.status in
           let response_text =
             Js.Opt.case xhr##.responseText (fun () -> "") Js.to_string
           in
           if status >= 200 && status < 300
           then Ivar.fill ivar (Ok ())
           else
             Ivar.fill
               ivar
               (Error
                  (Printf.sprintf
                     "Failed to create game: %d. Resp: %s"
                     status
                     response_text))
         | _ -> ());
    ignore (xhr##send (Js.Opt.return (Js.string body)));
    Ivar.read ivar
  ;;

  type queue_entry =
    { client_id : string
    ; ts : int
    }

  let parse_queue_response response_text : (queue_entry option, string) result =
    try
      let json = Js.Unsafe.global##._JSON##parse response_text in
      let fields = Js.Unsafe.get json "fields" in
      if not (Js.Optdef.test (Js.Optdef.return fields))
      then Ok None
      else (
        let waiter = Js.Unsafe.get fields "waiter" in
        if not (Js.Optdef.test (Js.Optdef.return waiter))
        then Ok None
        else (
          let mapv = Js.Unsafe.get waiter "mapValue" in
          let wf = Js.Unsafe.get mapv "fields" in
          let cid = Js.Unsafe.get wf "client_id" in
          let ts = Js.Unsafe.get wf "ts" in
          if not (Js.Optdef.test (Js.Optdef.return cid))
          then Ok None
          else if not (Js.Optdef.test (Js.Optdef.return ts))
          then Ok None
          else (
            let client = Js.to_string (Js.Unsafe.get cid "stringValue") in
            let ts_s = Js.to_string (Js.Unsafe.get ts "integerValue") in
            match Int.of_string_opt ts_s with
            | None -> Ok None
            | Some i -> Ok (Some { client_id = client; ts = i }))))
    with
    | exn -> Error (Printf.sprintf "Parse queue error: %s" (Exn.to_string exn))
  ;;

  let fetch_queue_async () : (queue_entry option, string) Result.t Deferred.t =
    let ivar = Ivar.create () in
    let xhr = XmlHttpRequest.create () in
    xhr##_open (Js.string "GET") (Js.string (queue_document_url ())) Js._true;
    xhr##.onreadystatechange
    := Js.wrap_callback (fun _ ->
         match xhr##.readyState with
         | XmlHttpRequest.DONE ->
           let status = xhr##.status in
           if status = 404
           then Ivar.fill ivar (Ok None)
           else if status >= 200 && status < 300
           then (
             let resp = Js.Opt.case xhr##.responseText (fun () -> "{}") Js.to_string in
             match parse_queue_response (Js.string resp) with
             | Ok q -> Ivar.fill ivar (Ok q)
             | Error msg -> Ivar.fill ivar (Error msg))
           else Ivar.fill ivar (Error (Printf.sprintf "Failed to fetch queue: %d" status))
         | _ -> ());
    ignore (xhr##send Js.null);
    Ivar.read ivar
  ;;

  let fetch_queue_effect () =
    Bonsai_web.Effect.of_deferred_fun (fun () -> fetch_queue_async ()) ()
  ;;

  let set_queue_waiter_async ~client_id : (unit, string) Result.t Deferred.t =
    let ivar = Ivar.create () in
    let xhr = XmlHttpRequest.create () in
    xhr##_open (Js.string "PATCH") (Js.string (queue_document_url ())) Js._true;
    xhr##setRequestHeader (Js.string "Content-Type") (Js.string "application/json");
    let ts = int_of_float (Js.to_float (new%js Js.date_now)##getTime) in
    let body =
      Printf.sprintf
        {|{"fields":{"waiter":{"mapValue":{"fields":{"client_id":{"stringValue":"%s"},"ts":{"integerValue":"%d"}}}}}}|}
        (String.escaped client_id)
        ts
    in
    xhr##.onreadystatechange
    := Js.wrap_callback (fun _ ->
         match xhr##.readyState with
         | XmlHttpRequest.DONE ->
           let status = xhr##.status in
           if status >= 200 && status < 300
           then Ivar.fill ivar (Ok ())
           else
             Ivar.fill
               ivar
               (Error (Printf.sprintf "Failed to set queue waiter: %d" status))
         | _ -> ());
    ignore (xhr##send (Js.Opt.return (Js.string body)));
    Ivar.read ivar
  ;;

  let set_queue_waiter_effect ~client_id =
    Bonsai_web.Effect.of_deferred_fun (fun () -> set_queue_waiter_async ~client_id) ()
  ;;

  let clear_queue_async () : (unit, string) Result.t Deferred.t =
    let ivar = Ivar.create () in
    let xhr = XmlHttpRequest.create () in
    xhr##_open (Js.string "PATCH") (Js.string (queue_document_url ())) Js._true;
    xhr##setRequestHeader (Js.string "Content-Type") (Js.string "application/json");
    let body = {|{"fields":{}}|} in
    xhr##.onreadystatechange
    := Js.wrap_callback (fun _ ->
         match xhr##.readyState with
         | XmlHttpRequest.DONE ->
           let status = xhr##.status in
           if status >= 200 && status < 300
           then Ivar.fill ivar (Ok ())
           else Ivar.fill ivar (Error (Printf.sprintf "Failed to clear queue: %d" status))
         | _ -> ());
    ignore (xhr##send (Js.Opt.return (Js.string body)));
    Ivar.read ivar
  ;;

  let clear_queue_effect () =
    Bonsai_web.Effect.of_deferred_fun (fun () -> clear_queue_async ()) ()
  ;;

  let find_game_for_client_async ~client_id : (string option, string) Result.t Deferred.t =
    let ivar = Ivar.create () in
    let xhr = XmlHttpRequest.create () in
    let list_url =
      Printf.sprintf "%s/games?key=%s&pageSize=50" base_url firebase_api_key
    in
    xhr##_open (Js.string "GET") (Js.string list_url) Js._true;
    xhr##.onreadystatechange
    := Js.wrap_callback (fun _ ->
         match xhr##.readyState with
         | XmlHttpRequest.DONE ->
           let status = xhr##.status in
           if status >= 200 && status < 300
           then (
             try
               let resp = Js.Opt.case xhr##.responseText (fun () -> "{}") Js.to_string in
               let json = Js.Unsafe.global##._JSON##parse (Js.string resp) in
               let documents = Js.Unsafe.get json "documents" in
               if Js.Optdef.test (Js.Optdef.return documents)
               then (
                 let docs_array = Js.to_array documents in
                 let found_game = ref None in
                 Array.iter docs_array ~f:(fun doc ->
                   try
                     let fields = Js.Unsafe.get doc "fields" in
                     let p1_field = Js.Unsafe.get fields "player1" in
                     let p2_field = Js.Unsafe.get fields "player2" in
                     let p1_match =
                       if Js.Optdef.test (Js.Optdef.return p1_field)
                       then (
                         let sv = Js.Unsafe.get p1_field "stringValue" in
                         String.equal (Js.to_string sv) client_id)
                       else false
                     in
                     let p2_match =
                       if Js.Optdef.test (Js.Optdef.return p2_field)
                       then (
                         let sv = Js.Unsafe.get p2_field "stringValue" in
                         String.equal (Js.to_string sv) client_id)
                       else false
                     in
                     if p1_match || p2_match
                     then (
                       let name = Js.Unsafe.get doc "name" in
                       let name_str = Js.to_string name in
                       let game_id =
                         match String.rsplit2 name_str ~on:'/' with
                         | Some (_, id) -> id
                         | None -> ""
                       in
                       if String.length game_id > 0 then found_game := Some game_id)
                   with
                   | _ -> ());
                 Ivar.fill ivar (Ok !found_game))
               else Ivar.fill ivar (Ok None)
             with
             | exn ->
               Ivar.fill
                 ivar
                 (Error (Printf.sprintf "Parse error: %s" (Exn.to_string exn))))
           else Ivar.fill ivar (Error (Printf.sprintf "Failed to list games: %d" status))
         | _ -> ());
    ignore (xhr##send Js.null);
    Ivar.read ivar
  ;;

  let find_game_for_client_effect ~client_id =
    Bonsai_web.Effect.of_deferred_fun (fun () -> find_game_for_client_async ~client_id) ()
  ;;

  let attempt_match_and_create_game_async ~client_id ~(maybe_waiter : queue_entry option)
    : (string option, string) Result.t Deferred.t
    =
    let ivar = Ivar.create () in
    match maybe_waiter with
    | None ->
      Ivar.fill ivar (Ok None);
      Ivar.read ivar
    | Some { client_id = waiter_id; ts = _ } ->
      if String.equal waiter_id client_id
      then (
        Ivar.fill ivar (Ok None);
        Ivar.read ivar)
      else (
        let created_game_id = generate_game_id () in
        (* The waiter is Player1, the matcher (current client) is Player2 *)
        let starter = 1 in
        let initial_game = Game.init ~dice_per_player:5 in
        let open Async_kernel.Deferred.Let_syntax in
        let%bind result =
          create_game_async
            ~game_id:created_game_id
            ~game_state:initial_game
            ~starter
            ~player1_id:waiter_id
            ~player2_id:client_id
        in
        Ivar.fill
          ivar
          (match result with
           | Ok () -> Ok (Some created_game_id)
           | Error msg -> Error msg);
        Ivar.read ivar)
  ;;

  let attempt_match_and_create_game_effect ~client_id ~maybe_waiter =
    Bonsai_web.Effect.of_deferred_fun
      (fun () -> attempt_match_and_create_game_async ~client_id ~maybe_waiter)
      ()
  ;;
end

let dice_face value =
  match value with
  | 1 -> "⚀"
  | 2 -> "⚁"
  | 3 -> "⚂"
  | 4 -> "⚃"
  | 5 -> "⚄"
  | 6 -> "⚅"
  | _ -> "?"
;;

let render_die ?(hidden = false) value =
  if hidden
  then Node.div ~attrs:[ Attr.class_ "die hidden" ] [ Node.text "?" ]
  else
    Node.div
      ~attrs:[ Attr.class_ "die"; Attr.create "data-face" (Int.to_string value) ]
      [ Node.span ~attrs:[ Attr.class_ "face" ] [ Node.text (dice_face value) ] ]
;;

type game_mode =
  | TitleScreen
  | AIMode
  | OnlineMode
[@@deriving sexp, equal]

type model =
  { game : Game.t
  ; round_message : string option
  ; selected_move_index : int
  ; ai_thinking : bool
  ; round_end_ticks : int
  ; game_mode : game_mode
  ; title_screen_ticks : int
  ; client_id : string
  ; waiting_in_queue : bool
  ; current_game_id : string option
  ; starter : int option
  ; my_player_number : int option
  ; processing_move : bool
  ; last_error : string option
  }
[@@deriving sexp]

let model_init () : model =
  { game = Game.init ~dice_per_player:5
  ; round_message = None
  ; selected_move_index = 0
  ; ai_thinking = false
  ; round_end_ticks = 0
  ; game_mode = TitleScreen
  ; title_screen_ticks = 0
  ; client_id = Multiplayer.generate_client_id ()
  ; waiting_in_queue = false
  ; current_game_id = None
  ; starter = None
  ; my_player_number = None
  ; processing_move = false
  ; last_error = None
  }
;;

let bid_to_string = function
  | None -> "No bid yet"
  | Some (bid : Bid.t) -> sprintf "%d x %s" bid.count (dice_face bid.value)
;;

let move_to_string = function
  | `Bid (bid : Bid.t) -> sprintf "%d x %s" bid.count (dice_face bid.value)
  | `CallLiar -> "Call Liar"
;;

let is_local_player_turn (m : model) : bool =
  match Game.get_winner m.game, Game.current_round m.game with
  | Some _, _ -> false
  | _, None -> false
  | None, Some round ->
    (match Round.get_current_player round with
     | Player.Player1 -> true
     | Player.Player2 -> false)
;;

let execute_ai_move (m : model) : model =
  match Game.current_round m.game with
  | None -> m
  | Some round ->
    (match get_logical_move round with
     | `Bid b ->
       (match Round.make_bid round b with
        | Ok new_round ->
          { m with
            game = { m.game with current_round = Some new_round }
          ; ai_thinking = false
          }
        | Error _ ->
          (match Round.call_liar round with
           | Ok (winner, _) ->
             let game' = Game.apply_round_result m.game winner in
             let game_with_round_cleared = { game' with current_round = None } in
             { m with
               game = game_with_round_cleared
             ; round_message =
                 Some
                   (if Player.equal winner Player.Player1
                    then "You won the round!"
                    else "You lost the round!")
             ; ai_thinking = false
             ; round_end_ticks = 1
             }
           | Error _ -> { m with ai_thinking = false }))
     | `CallLiar ->
       (match Round.call_liar round with
        | Ok (winner, _) ->
          let game' = Game.apply_round_result m.game winner in
          let game_with_round_cleared = { game' with current_round = None } in
          { m with
            game = game_with_round_cleared
          ; round_message =
              Some
                (if Player.equal winner Player.Player1
                 then "You won the round!"
                 else "You lost the round!")
          ; ai_thinking = false
          ; round_end_ticks = 1
          }
        | Error _ -> { m with ai_thinking = false }))
;;

let swap_players_in_game (game : Game.t) : Game.t =
  match game.current_round with
  | None -> game
  | Some round ->
    let swapped_hands =
      List.map round.hands ~f:(fun (player, hand) -> Player.opposite player, hand)
    in
    let swapped_current_player = Player.opposite round.current_player in
    let swapped_round =
      { round with hands = swapped_hands; current_player = swapped_current_player }
    in
    let swapped_rounds_won =
      List.map game.rounds_won ~f:(fun (player, wins) -> Player.opposite player, wins)
    in
    let swapped_game_winner = Option.map game.game_winner ~f:Player.opposite in
    { game with
      current_round = Some swapped_round
    ; rounds_won = swapped_rounds_won
    ; game_winner = swapped_game_winner
    }
;;

let maybe_rotate_game_for_starter (game : Game.t) (my_player_number : int option) : Game.t
  =
  match my_player_number with
  | None -> game
  | Some player_num ->
    (* If we are Player2, we need to swap the game perspective *)
    if player_num = 2 then swap_players_in_game game else game
;;

let get_canonical_game_state (game : Game.t) (my_player_number : int option) : Game.t =
  match my_player_number with
  | None -> game
  | Some player_num ->
    (* If we are Player2, unswap to get canonical form (Player1 perspective) *)
    if player_num = 2 then swap_players_in_game game else game
;;

let render_title_screen model set_model =
  let round = Game.current_round model.game in
  let top_dice =
    Option.value_map round ~default:[] ~f:(fun r -> Round.hand_of r Player.Player1)
  in
  let bottom_dice =
    Option.value_map round ~default:[] ~f:(fun r -> Round.hand_of r Player.Player2)
  in
  Node.div
    ~attrs:[ Attr.class_ "title-screen" ]
    [ Node.div
        ~attrs:[ Attr.class_ "game-container" ]
        [ Node.div
            ~attrs:[ Attr.class_ "hand" ]
            [ Node.div
                ~attrs:[ Attr.class_ "dice-container" ]
                (List.map top_dice ~f:render_die)
            ]
        ; Node.div
            ~attrs:[ Attr.class_ "game-info title-info" ]
            [ Node.h1 [ Node.text "Liar's Dice" ]
            ; Node.div
                ~attrs:[ Attr.class_ "mode-buttons" ]
                [ Node.button
                    ~attrs:
                      [ Attr.class_ "btn btn-mode"
                      ; Attr.on_click (fun _ev ->
                          set_model
                            { model with
                              game_mode = AIMode
                            ; game = Game.init ~dice_per_player:5
                            })
                      ]
                    [ Node.text "AI" ]
                ; Node.button
                    ~attrs:
                      [ Attr.class_ "btn btn-mode"
                      ; Attr.on_click (fun _ev ->
                          let new_model =
                            { model with game_mode = OnlineMode; waiting_in_queue = true }
                          in
                          set_model
                            { new_model with
                              current_game_id = None
                            ; starter = None
                            ; last_error = None
                            })
                      ]
                    [ Node.text "Online" ]
                ]
            ]
        ; Node.div
            ~attrs:[ Attr.class_ "hand" ]
            [ Node.div
                ~attrs:[ Attr.class_ "dice-container" ]
                (List.map bottom_dice ~f:render_die)
            ]
        ]
    ]
;;

let component =
  let%sub model, set_model =
    let module M = struct
      type t = model [@@deriving sexp]

      let equal a b = Sexp.equal (sexp_of_t a) (sexp_of_t b)
    end
    in
    Bonsai.state (module M) ~default_model:(model_init ())
  in
  let%sub () =
    let callback =
      let%map model = model
      and set_model = set_model in
      if (not model.waiting_in_queue) || not (equal_game_mode model.game_mode OnlineMode)
      then Effect.Ignore
      else
        let open Vdom.Effect.Let_syntax in
        let%bind queue_res = Multiplayer.fetch_queue_effect () in
        match queue_res with
        | Error msg ->
          set_model { model with last_error = Some ("Matchmaking fetch failed: " ^ msg) }
        | Ok None ->
          let%bind set_res =
            Multiplayer.set_queue_waiter_effect ~client_id:model.client_id
          in
          (match set_res with
           | Ok () -> Vdom.Effect.Ignore
           | Error msg ->
             set_model { model with last_error = Some ("Failed to join queue: " ^ msg) })
        | Ok (Some waiter) ->
          if String.equal waiter.client_id model.client_id
          then (
            (* We are the waiter - check if a game was created for us *)
            let%bind game_search_res =
              Multiplayer.find_game_for_client_effect ~client_id:model.client_id
            in
            match game_search_res with
            | Error _ -> Vdom.Effect.Ignore
            | Ok None -> Vdom.Effect.Ignore
            | Ok (Some found_game_id) ->
              (* Found a game! Fetch it and start playing *)
              let%bind fetch_res =
                Multiplayer.fetch_document_effect ~game_id:found_game_id
              in
              (match fetch_res with
               | Error _ -> Vdom.Effect.Ignore
               | Ok None -> Vdom.Effect.Ignore
               | Ok (Some (maybe_state, starter, player1, player2)) ->
                 let game_state = Option.value_exn maybe_state in
                 let my_player_number =
                   match player1, player2 with
                   | Some p1, _ when String.equal p1 model.client_id -> Some 1
                   | _, Some p2 when String.equal p2 model.client_id -> Some 2
                   | _ -> None
                 in
                 let rotated =
                   maybe_rotate_game_for_starter game_state my_player_number
                 in
                 set_model
                   { model with
                     game = rotated
                   ; waiting_in_queue = false
                   ; current_game_id = Some found_game_id
                   ; starter
                   ; my_player_number
                   ; last_error = None
                   }))
          else (
            let%bind match_res =
              Multiplayer.attempt_match_and_create_game_effect
                ~client_id:model.client_id
                ~maybe_waiter:(Some waiter)
            in
            match match_res with
            | Error msg ->
              set_model { model with last_error = Some ("Match create failed: " ^ msg) }
            | Ok None -> Vdom.Effect.Ignore
            | Ok (Some created_game_id) ->
              let%bind _ = Multiplayer.clear_queue_effect () in
              let%bind fetch_res =
                Multiplayer.fetch_document_effect ~game_id:created_game_id
              in
              (match fetch_res with
               | Error msg ->
                 set_model
                   { model with last_error = Some ("Fetch created game failed: " ^ msg) }
               | Ok None ->
                 set_model { model with last_error = Some "Created game not found" }
               | Ok (Some (maybe_state, starter, player1, player2)) ->
                 let game_state = Option.value_exn maybe_state in
                 (* Determine which player we are based on player1/player2 *)
                 let my_player_number =
                   match player1, player2 with
                   | Some p1, _ when String.equal p1 model.client_id -> Some 1
                   | _, Some p2 when String.equal p2 model.client_id -> Some 2
                   | _ -> None
                 in
                 let rotated =
                   maybe_rotate_game_for_starter game_state my_player_number
                 in
                 set_model
                   { model with
                     game = rotated
                   ; waiting_in_queue = false
                   ; current_game_id = Some created_game_id
                   ; starter
                   ; my_player_number
                   ; last_error = None
                   }))
    in
    Bonsai.Clock.every
      ~when_to_start_next_effect:`Every_multiple_of_period_blocking
      (Time_ns.Span.of_sec 1.0)
      callback
  in
  let%sub () =
    let callback =
      let%map model = model
      and set_model = set_model in
      match model.current_game_id with
      | None -> Effect.Ignore
      | Some gid ->
        let open Vdom.Effect.Let_syntax in
        let%bind fetch_res = Multiplayer.fetch_document_effect ~game_id:gid in
        (match fetch_res with
         | Error msg -> set_model { model with last_error = Some ("Sync failed: " ^ msg) }
         | Ok None -> set_model { model with last_error = Some "Game not found" }
         | Ok (Some (maybe_state, starter, player1, player2)) ->
           let game_state = Option.value_exn maybe_state in
           (* Determine which player we are based on player1/player2 *)
           let my_player_number =
             match player1, player2 with
             | Some p1, _ when String.equal p1 model.client_id -> Some 1
             | _, Some p2 when String.equal p2 model.client_id -> Some 2
             | _ -> model.my_player_number
           in
           let rotated = maybe_rotate_game_for_starter game_state my_player_number in
           (* Check if round just ended (game has no current_round but previous did) *)
           let should_show_round_end =
             Option.is_none (Game.current_round rotated)
             && Option.is_some (Game.current_round model.game)
             && model.round_end_ticks = 0
           in
           let round_message =
             if should_show_round_end
             then (
               (* Compare scores in canonical (unrotated) game state to determine actual winner *)
               let canonical_old =
                 get_canonical_game_state model.game model.my_player_number
               in
               let canonical_new =
                 get_canonical_game_state rotated model.my_player_number
               in
               let p1_old = Game.rounds_won_by canonical_old Player.Player1 in
               let p2_old = Game.rounds_won_by canonical_old Player.Player2 in
               let p1_new = Game.rounds_won_by canonical_new Player.Player1 in
               let p2_new = Game.rounds_won_by canonical_new Player.Player2 in
               let canonical_winner =
                 if p1_new > p1_old
                 then Some Player.Player1
                 else if p2_new > p2_old
                 then Some Player.Player2
                 else None
               in
               (* Check if I (based on my_player_number) won *)
               match canonical_winner, model.my_player_number with
               | Some Player.Player1, Some 1 -> Some "You won the round!"
               | Some Player.Player2, Some 2 -> Some "You won the round!"
               | Some Player.Player1, Some 2 -> Some "You lost the round!"
               | Some Player.Player2, Some 1 -> Some "You lost the round!"
               | _ -> model.round_message)
             else model.round_message
           in
           let round_end_ticks =
             if should_show_round_end then 1 else model.round_end_ticks
           in
           set_model
             { model with
               game = rotated
             ; starter
             ; my_player_number
             ; round_message
             ; round_end_ticks
             ; last_error = None
             })
    in
    Bonsai.Clock.every
      ~when_to_start_next_effect:`Every_multiple_of_period_blocking
      (Time_ns.Span.of_sec 1.5)
      callback
  in
  let%sub () =
    let callback =
      let%map model = model
      and set_model = set_model in
      if equal_game_mode model.game_mode TitleScreen
      then (
        let new_ticks = model.title_screen_ticks + 1 in
        if new_ticks mod 2 = 0
        then
          set_model
            { model with
              title_screen_ticks = new_ticks
            ; game = Game.init ~dice_per_player:5
            }
        else set_model { model with title_screen_ticks = new_ticks })
      else if model.round_end_ticks > 0 && Option.is_none (Game.get_winner model.game)
      then
        if model.round_end_ticks >= 8
        then (
          let game' = Game.next_round_if_possible model.game in
          match model.current_game_id with
          | Some gid when equal_game_mode model.game_mode OnlineMode ->
            (* Sync new round to Firebase *)
            let open Vdom.Effect.Let_syntax in
            (* Save canonical game state (Player1 perspective) to Firebase *)
            let canonical_game = get_canonical_game_state game' model.my_player_number in
            let%bind res =
              Multiplayer.save_game_state_effect ~game_id:gid ~game_state:canonical_game
            in
            (match res with
             | Ok () ->
               set_model
                 { model with game = game'; round_message = None; round_end_ticks = 0 }
             | Error err ->
               set_model
                 { model with last_error = Some ("Sync new round failed: " ^ err) })
          | _ ->
            set_model
              { model with game = game'; round_message = None; round_end_ticks = 0 })
        else set_model { model with round_end_ticks = model.round_end_ticks + 1 }
      else if
        equal_game_mode model.game_mode AIMode
        && (not (is_local_player_turn model))
        && Option.is_some (Game.current_round model.game)
        && not model.ai_thinking
      then set_model { model with ai_thinking = true }
      else if equal_game_mode model.game_mode AIMode && model.ai_thinking
      then (
        let new_model = execute_ai_move model in
        set_model new_model)
      else Effect.Ignore
    in
    Bonsai.Clock.every
      ~when_to_start_next_effect:`Every_multiple_of_period_blocking
      (Time_ns.Span.of_sec 0.5)
      callback
  in
  let%arr model = model
  and set_model = set_model in
  let game = model.game in
  match model.game_mode with
  | TitleScreen -> render_title_screen model set_model
  | OnlineMode ->
    if model.waiting_in_queue
    then
      Node.div
        ~attrs:[ Attr.class_ "setup-screen" ]
        [ Node.h2 [ Node.text "Searching for opponent..." ]
        ; Node.p [ Node.text (Printf.sprintf "Client id: %s" model.client_id) ]
        ; Node.div
            [ Node.button
                ~attrs:
                  [ Attr.class_ "btn"
                  ; Attr.on_click (fun _ ->
                      let _ = Multiplayer.clear_queue_effect () in
                      set_model { model with waiting_in_queue = false; last_error = None })
                  ]
                [ Node.text "Cancel Matchmaking" ]
            ]
        ; (match model.last_error with
           | None -> Node.none
           | Some e -> Node.div ~attrs:[ Attr.class_ "move-error-banner" ] [ Node.text e ])
        ]
    else (
      let round = Game.current_round game in
      let current_bid = Option.bind round ~f:Round.get_current_bid in
      let current_player = Option.map round ~f:Round.get_current_player in
      let p1_score = Game.rounds_won_by game Player.Player1 in
      let p2_score = Game.rounds_won_by game Player.Player2 in
      let p1_hand =
        Option.value_map round ~default:[] ~f:(fun r -> Round.hand_of r Player.Player1)
      in
      let p2_hand =
        Option.value_map round ~default:[] ~f:(fun r -> Round.hand_of r Player.Player2)
      in
      let turn_text =
        match current_player with
        | None -> "Game over"
        | Some Player.Player1 -> "Your Turn"
        | Some Player.Player2 -> "Opponent's Turn"
      in
      let is_player_turn = is_local_player_turn model in
      let game_in_progress =
        Option.is_none (Game.get_winner game) && Option.is_some round
      in
      let buttons_enabled =
        game_in_progress
        && is_player_turn
        && (not model.processing_move)
        && model.round_end_ticks = 0
      in
      let valid_moves =
        Option.value_map round ~default:[] ~f:(fun r -> Round.get_all_moves r)
      in
      let selected_move_index =
        if List.length valid_moves = 0
        then 0
        else Int.min model.selected_move_index (List.length valid_moves - 1)
      in
      let on_move_select =
        Attr.on_input (fun _ev str ->
          let new_index = Option.value (Int.of_string_opt str) ~default:0 in
          set_model { model with selected_move_index = new_index })
      in
      let make_move_handler =
        Attr.on_click (fun _ev ->
          match Game.current_round game with
          | None -> Effect.Ignore
          | Some round ->
            if not is_player_turn
            then Effect.Ignore
            else (
              let selected_move = List.nth valid_moves selected_move_index in
              match selected_move with
              | None -> Effect.Ignore
              | Some (`Bid bid) ->
                if model.processing_move
                then Effect.Ignore
                else (
                  match Round.make_bid round bid with
                  | Error _ -> Effect.Ignore
                  | Ok new_round ->
                    let new_game = { game with current_round = Some new_round } in
                    (match model.current_game_id with
                     | Some gid ->
                       let open Vdom.Effect.Let_syntax in
                       let%bind _ = set_model { model with processing_move = true } in
                       (* Save canonical game state (Player1 perspective) to Firebase *)
                       let canonical_game =
                         get_canonical_game_state new_game model.my_player_number
                       in
                       let%bind res =
                         Multiplayer.save_game_state_effect
                           ~game_id:gid
                           ~game_state:canonical_game
                       in
                       (match res with
                        | Ok () ->
                          set_model
                            { model with game = new_game; processing_move = false }
                        | Error err ->
                          set_model
                            { model with
                              last_error = Some ("Sync failed: " ^ err)
                            ; processing_move = false
                            })
                     | None -> set_model { model with game = new_game }))
              | Some `CallLiar ->
                if model.processing_move
                then Effect.Ignore
                else (
                  match Round.call_liar round with
                  | Error _ -> Effect.Ignore
                  | Ok (winner, _msg) ->
                    let game' = Game.apply_round_result game winner in
                    (* Clear the current round so both players can detect round end *)
                    let game_with_round_cleared = { game' with current_round = None } in
                    let round_message =
                      if Player.equal winner Player.Player1
                      then "You won the round!"
                      else "You lost the round!"
                    in
                    (match model.current_game_id with
                     | Some gid ->
                       let open Vdom.Effect.Let_syntax in
                       let%bind _ = set_model { model with processing_move = true } in
                       (* Save canonical game state (Player1 perspective) to Firebase *)
                       let canonical_game =
                         get_canonical_game_state
                           game_with_round_cleared
                           model.my_player_number
                       in
                       let%bind res =
                         Multiplayer.save_game_state_effect
                           ~game_id:gid
                           ~game_state:canonical_game
                       in
                       (match res with
                        | Ok () ->
                          set_model
                            { model with
                              game = game_with_round_cleared
                            ; round_message = Some round_message
                            ; round_end_ticks = 1
                            ; processing_move = false
                            }
                        | Error err ->
                          set_model
                            { model with
                              last_error = Some ("Sync failed: " ^ err)
                            ; processing_move = false
                            })
                     | None ->
                       set_model
                         { model with
                           game = game_with_round_cleared
                         ; round_message = Some round_message
                         ; round_end_ticks = 1
                         }))))
      in
      let move_controls =
        Node.div
          ~attrs:[ Attr.class_ "action-bar" ]
          [ Node.select
              ~attrs:
                [ (if buttons_enabled
                   then Attr.empty
                   else Attr.bool_property "disabled" true)
                ; on_move_select
                ]
              (List.mapi valid_moves ~f:(fun i move ->
                 Node.option
                   ~attrs:
                     [ Attr.value (Int.to_string i)
                     ; (if i = selected_move_index then Attr.selected else Attr.empty)
                     ]
                   [ Node.text (move_to_string move) ]))
          ; Node.button
              ~attrs:
                [ Attr.class_ "btn make-move"
                ; (if buttons_enabled
                   then Attr.empty
                   else Attr.bool_property "disabled" true)
                ; make_move_handler
                ]
              [ Node.text "Make Move" ]
          ]
      in
      let show_opponent_dice = Option.is_some model.round_message in
      Node.div
        [ Node.div
            ~attrs:[ Attr.class_ "game-container" ]
            [ Node.div
                ~attrs:[ Attr.class_ "hand" ]
                [ Node.div
                    ~attrs:[ Attr.class_ "player-label" ]
                    [ Node.text "Opponent (Player 2)" ]
                ; Node.div
                    ~attrs:[ Attr.class_ "dice-container" ]
                    (List.map p2_hand ~f:(fun v ->
                       render_die ~hidden:(not show_opponent_dice) v))
                ]
            ; Node.div
                ~attrs:[ Attr.class_ "game-info" ]
                [ Node.h1 [ Node.text "Liar's Dice" ]
                ; Node.div
                    ~attrs:[ Attr.class_ "scoreboard" ]
                    [ Node.span
                        ~attrs:[ Attr.class_ "score player1-score" ]
                        [ Node.text (sprintf "Player 1: %d" p1_score) ]
                    ; Node.span
                        ~attrs:[ Attr.class_ "score player2-score" ]
                        [ Node.text (sprintf "Player 2: %d" p2_score) ]
                    ]
                ; Node.p ~attrs:[ Attr.id "turn-info" ] [ Node.text turn_text ]
                ; Node.p
                    ~attrs:[ Attr.id "bid-info" ]
                    [ Node.text (bid_to_string current_bid) ]
                ]
            ; Node.div
                ~attrs:[ Attr.class_ "hand player-hand" ]
                [ Node.div
                    ~attrs:[ Attr.class_ "dice-container" ]
                    (List.map p1_hand ~f:(render_die ~hidden:false))
                ; Node.div
                    ~attrs:[ Attr.class_ "player-label" ]
                    [ Node.text "You (Player 1)" ]
                ; move_controls
                ]
            ]
        ; (match model.round_message, Game.get_winner game with
           | None, None -> Node.none
           | Some msg, None ->
             Node.div
               ~attrs:[ Attr.id "round-message"; Attr.class_ "system-message" ]
               [ Node.div ~attrs:[ Attr.class_ "message-content" ] [ Node.text msg ] ]
           | _, Some winner ->
             let msg =
               if Player.equal winner Player.Player1
               then "You won the game!"
               else "You lost the game!"
             in
             Node.div
               ~attrs:[ Attr.id "round-message"; Attr.class_ "system-message" ]
               [ Node.div
                   ~attrs:[ Attr.class_ "message-content" ]
                   [ Node.text msg
                   ; Node.p
                       ~attrs:[ Attr.style (Css_gen.font_size (`Rem 1.2)) ]
                       [ Node.text "Play another?" ]
                   ; Node.div
                       ~attrs:[ Attr.class_ "mode-buttons" ]
                       [ Node.button
                           ~attrs:
                             [ Attr.class_ "btn btn-mode"
                             ; Attr.on_click (fun _ev ->
                                 set_model
                                   { (model_init ()) with
                                     game_mode = AIMode
                                   ; game = Game.init ~dice_per_player:5
                                   })
                             ]
                           [ Node.text "AI" ]
                       ; Node.button
                           ~attrs:
                             [ Attr.class_ "btn btn-mode"
                             ; Attr.on_click (fun _ev ->
                                 set_model
                                   { (model_init ()) with
                                     game_mode = OnlineMode
                                   ; waiting_in_queue = true
                                   })
                             ]
                           [ Node.text "Online" ]
                       ]
                   ]
               ])
        ])
  | AIMode ->
    let round = Game.current_round model.game in
    let current_bid = Option.bind round ~f:Round.get_current_bid in
    let current_player = Option.map round ~f:Round.get_current_player in
    let p1_score = Game.rounds_won_by model.game Player.Player1 in
    let p2_score = Game.rounds_won_by model.game Player.Player2 in
    let p1_hand =
      Option.value_map round ~default:[] ~f:(fun r -> Round.hand_of r Player.Player1)
    in
    let p2_hand =
      Option.value_map round ~default:[] ~f:(fun r -> Round.hand_of r Player.Player2)
    in
    let turn_text =
      match current_player with
      | None -> "Game over"
      | Some Player.Player1 -> "Player 1's Turn"
      | Some Player.Player2 -> "Player 2's Turn"
    in
    let is_player_turn =
      Option.value_map current_player ~default:false ~f:(Player.equal Player.Player1)
    in
    let game_in_progress =
      Option.is_none (Game.get_winner model.game) && Option.is_some round
    in
    let buttons_enabled =
      game_in_progress
      && is_player_turn
      && (not model.processing_move)
      && model.round_end_ticks = 0
    in
    let valid_moves =
      Option.value_map round ~default:[] ~f:(fun r -> Round.get_all_moves r)
    in
    let selected_move_index =
      if List.length valid_moves = 0
      then 0
      else Int.min model.selected_move_index (List.length valid_moves - 1)
    in
    let on_move_select =
      Attr.on_input (fun _ev str ->
        let new_index = Option.value (Int.of_string_opt str) ~default:0 in
        set_model { model with selected_move_index = new_index })
    in
    let make_move_handler =
      Attr.on_click (fun _ev ->
        match Game.current_round model.game with
        | None -> Effect.Ignore
        | Some round ->
          if not is_player_turn
          then Effect.Ignore
          else (
            let selected_move = List.nth valid_moves selected_move_index in
            match selected_move with
            | None -> Effect.Ignore
            | Some (`Bid bid) ->
              (match Round.make_bid round bid with
               | Error _ -> Effect.Ignore
               | Ok new_round ->
                 set_model
                   { model with
                     game = { model.game with current_round = Some new_round }
                   })
            | Some `CallLiar ->
              (match Round.call_liar round with
               | Error _ -> Effect.Ignore
               | Ok (winner, _msg) ->
                 let game' = Game.apply_round_result model.game winner in
                 let game_with_round_cleared = { game' with current_round = None } in
                 let round_message =
                   if Player.equal winner Player.Player1
                   then "You won the round!"
                   else "You lost the round!"
                 in
                 set_model
                   { model with
                     game = game_with_round_cleared
                   ; round_message = Some round_message
                   ; round_end_ticks = 1
                   })))
    in
    let move_controls =
      Node.div
        ~attrs:[ Attr.class_ "action-bar" ]
        [ Node.select
            ~attrs:
              [ (if buttons_enabled
                 then Attr.empty
                 else Attr.bool_property "disabled" true)
              ; on_move_select
              ]
            (List.mapi valid_moves ~f:(fun i move ->
               Node.option
                 ~attrs:
                   [ Attr.value (Int.to_string i)
                   ; (if i = selected_move_index then Attr.selected else Attr.empty)
                   ]
                 [ Node.text (move_to_string move) ]))
        ; Node.button
            ~attrs:
              [ Attr.class_ "btn make-move"
              ; (if buttons_enabled
                 then Attr.empty
                 else Attr.bool_property "disabled" true)
              ; make_move_handler
              ]
            [ Node.text "Make Move" ]
        ]
    in
    let show_opponent_dice = Option.is_some model.round_message in
    Node.div
      [ Node.div
          ~attrs:[ Attr.class_ "game-container" ]
          [ Node.div
              ~attrs:[ Attr.class_ "hand" ]
              [ Node.div
                  ~attrs:[ Attr.class_ "player-label" ]
                  [ Node.text "Opponent (Player 2)" ]
              ; Node.div
                  ~attrs:[ Attr.class_ "dice-container" ]
                  (List.map p2_hand ~f:(fun v ->
                     render_die ~hidden:(not show_opponent_dice) v))
              ]
          ; Node.div
              ~attrs:[ Attr.class_ "game-info" ]
              [ Node.h1 [ Node.text "Liar's Dice" ]
              ; Node.div
                  ~attrs:[ Attr.class_ "scoreboard" ]
                  [ Node.span
                      ~attrs:[ Attr.class_ "score player1-score" ]
                      [ Node.text (sprintf "Player 1: %d" p1_score) ]
                  ; Node.span
                      ~attrs:[ Attr.class_ "score player2-score" ]
                      [ Node.text (sprintf "Player 2: %d" p2_score) ]
                  ]
              ; Node.p ~attrs:[ Attr.id "turn-info" ] [ Node.text turn_text ]
              ; Node.p
                  ~attrs:[ Attr.id "bid-info" ]
                  [ Node.text (bid_to_string current_bid) ]
              ]
          ; Node.div
              ~attrs:[ Attr.class_ "hand player-hand" ]
              [ Node.div
                  ~attrs:[ Attr.class_ "dice-container" ]
                  (List.map p1_hand ~f:(render_die ~hidden:false))
              ; Node.div
                  ~attrs:[ Attr.class_ "player-label" ]
                  [ Node.text "You (Player 1)" ]
              ; move_controls
              ]
          ]
      ; (match model.round_message, Game.get_winner model.game with
         | None, None -> Node.none
         | Some msg, None ->
           Node.div
             ~attrs:[ Attr.id "round-message"; Attr.class_ "system-message" ]
             [ Node.div ~attrs:[ Attr.class_ "message-content" ] [ Node.text msg ] ]
         | _, Some winner ->
           let msg =
             if Player.equal winner Player.Player1
             then "You won the game!"
             else "You lost the game!"
           in
           Node.div
             ~attrs:[ Attr.id "round-message"; Attr.class_ "system-message" ]
             [ Node.div
                 ~attrs:[ Attr.class_ "message-content" ]
                 [ Node.text msg
                 ; Node.p
                     ~attrs:[ Attr.style (Css_gen.font_size (`Rem 1.2)) ]
                     [ Node.text "Play another?" ]
                 ; Node.div
                     ~attrs:[ Attr.class_ "mode-buttons" ]
                     [ Node.button
                         ~attrs:
                           [ Attr.class_ "btn btn-mode"
                           ; Attr.on_click (fun _ev ->
                               set_model
                                 { (model_init ()) with
                                   game_mode = AIMode
                                 ; game = Game.init ~dice_per_player:5
                                 })
                           ]
                         [ Node.text "AI" ]
                     ; Node.button
                         ~attrs:
                           [ Attr.class_ "btn btn-mode"
                           ; Attr.on_click (fun _ev ->
                               set_model
                                 { (model_init ()) with
                                   game_mode = OnlineMode
                                 ; waiting_in_queue = true
                                 })
                           ]
                         [ Node.text "Online" ]
                     ]
                 ]
             ])
      ]
;;

let () =
  Random.self_init ();
  Bonsai_web.Start.start ~bind_to_element_with_id:"app" component
;;

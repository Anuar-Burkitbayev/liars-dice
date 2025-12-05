open! Core
open Logic_library
open Hw2
open Hw4
open Virtual_dom
open! Bonsai.Let_syntax
module Node = Vdom.Node
module Attr = Vdom.Attr
module Effect = Vdom.Effect

(* Helper function to get dice face Unicode *)
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

(* Render a single die *)
let render_die ?(hidden = false) value =
  if hidden
  then Node.div ~attrs:[ Attr.class_ "die hidden" ] [ Node.text "?" ]
  else
    Node.div
      ~attrs:[ Attr.class_ "die"; Attr.create "data-face" (Int.to_string value) ]
      [ Node.span ~attrs:[ Attr.class_ "face" ] [ Node.text (dice_face value) ] ]
;;

(* --- Model and helpers --- *)
type model =
  { game : Game.t
  ; round_message : string option
  ; selected_move_index : int
  ; ai_thinking : bool (* Track if AI is about to move *)
  ; round_end_ticks : int (* Count ticks since round ended (0 = not ended) *)
  }
[@@deriving sexp]

let model_init () : model =
  { game = Game.init ~dice_per_player:5
  ; round_message = None
  ; selected_move_index = 0
  ; ai_thinking = false
  ; round_end_ticks = 0
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

(* Check if it's AI's turn *)
let is_ai_turn (m : model) : bool =
  match Game.get_winner m.game, Game.current_round m.game with
  | Some _, _ -> false
  | _, None -> false
  | None, Some round ->
    (match Round.get_current_player round with
     | Player.Player1 -> false
     | Player.Player2 -> true)
;;

(* Execute a single AI move *)
let execute_ai_move (m : model) : model =
  match Game.current_round m.game with
  | None -> m
  | Some round ->
    (match get_logical_move round with
     | `Bid b ->
       (match Round.make_bid round b with
        | Ok new_round ->
          { m with game = { m.game with current_round = Some new_round }; ai_thinking = false }
        | Error _ ->
          (* If AI produced invalid bid (shouldn't happen), call liar *)
          (match Round.call_liar round with
           | Ok (winner, _) ->
             let game' = Game.apply_round_result m.game winner in
             { m with
               game = game'
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
          { m with
            game = game'
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

(* Create the main UI component *)
let component =
  (* State: model with custom equality via sexp *)
  let%sub model, set_model =
    let module M = struct
      type t = model [@@deriving sexp]

      let equal a b = Sexp.equal (sexp_of_t a) (sexp_of_t b)
    end
    in
    Bonsai.state (module M) ~default_model:(model_init ())
  in
  (* Set up a clock to check for AI moves and auto-progression *)
  let%sub () =
    let callback =
      let%map model = model
      and set_model = set_model in
      (* Check for automatic round progression *)
      if model.round_end_ticks > 0 && Option.is_none (Game.get_winner model.game)
      then
        if model.round_end_ticks >= 8 (* 8 ticks * 0.5s = 4 seconds *)
        then
          (* Auto-progress to next round *)
          let game' = Game.next_round_if_possible model.game in
          set_model { model with game = game'; round_message = None; round_end_ticks = 0 }
        else
          (* Increment tick counter *)
          set_model { model with round_end_ticks = model.round_end_ticks + 1 }
      else if is_ai_turn model && not model.ai_thinking
      then
        (* Mark AI as thinking and schedule the move *)
        set_model { model with ai_thinking = true }
      else if model.ai_thinking
      then
        (* Execute the AI move after delay *)
        let new_model = execute_ai_move model in
        set_model new_model
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
    | Some Player.Player1 -> "Player 1's Turn"
    | Some Player.Player2 -> "Player 2's Turn"
  in
  let is_player_turn =
    Option.value_map current_player ~default:false ~f:(Player.equal Player.Player1)
  in
  let game_in_progress = Option.is_none (Game.get_winner game) && Option.is_some round in
  let buttons_enabled = game_in_progress && is_player_turn in
  (* Get valid moves for current state *)
  let valid_moves =
    Option.value_map round ~default:[] ~f:(fun r -> Round.get_all_moves r)
  in
  (* Ensure selected index is in bounds *)
  let selected_move_index =
    if List.length valid_moves = 0
    then 0
    else Int.min model.selected_move_index (List.length valid_moves - 1)
  in
  (* Handlers *)
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
            (match Round.make_bid round bid with
             | Error _ -> Effect.Ignore
             | Ok new_round ->
               (* Update game state, AI will move on next clock tick *)
               set_model
                 { model with game = { game with current_round = Some new_round } })
           | Some `CallLiar ->
             (match Round.call_liar round with
              | Error _ -> Effect.Ignore
              | Ok (winner, _msg) ->
                let game' = Game.apply_round_result game winner in
                let round_message =
                  if Player.equal winner Player.Player1
                  then "You won the round!"
                  else "You lost the round!"
                in
                set_model { model with game = game'; round_message = Some round_message; round_end_ticks = 1 })))
  in
  let new_round_handler =
    Attr.on_click (fun _ev ->
      match Game.get_winner game with
      | Some _ -> set_model (model_init ())
      | None ->
        (* Start a new round *)
        let game' = Game.next_round_if_possible game in
        (* AI will move on next clock tick if needed *)
        set_model { model with game = game'; round_message = None })
  in
  (* Dropdown for valid moves *)
  let move_controls =
    Node.div
      ~attrs:[ Attr.class_ "action-bar" ]
      [ Node.select
          ~attrs:
            [ (if buttons_enabled then Attr.empty else Attr.bool_property "disabled" true)
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
            ; (if buttons_enabled then Attr.empty else Attr.bool_property "disabled" true)
            ; make_move_handler
            ]
          [ Node.text "Make Move" ]
      ]
  in
  (* The view *)
  (* Show opponent's dice when round has ended *)
  let show_opponent_dice = Option.is_some model.round_message in
  Node.div
    [ Node.div
        ~attrs:[ Attr.class_ "game-container" ]
        [ (* Opponent's hand (hidden during play, visible after round ends) *)
          Node.div
            ~attrs:[ Attr.class_ "hand" ]
            [ Node.div
                ~attrs:[ Attr.class_ "player-label" ]
                [ Node.text "Opponent (Player 2)" ]
            ; Node.div
                ~attrs:[ Attr.class_ "dice-container" ]
                (List.map p2_hand ~f:(render_die ~hidden:(not show_opponent_dice)))
            ]
        ; (* Game info section *)
          Node.div
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
        ; (* Player's hand and actions *)
          Node.div
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
    ; (* Popup overlay for round/game message *)
      (match model.round_message, Game.get_winner game with
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
               ; Node.button
                   ~attrs:[ Attr.class_ "btn btn-new-game"; new_round_handler ]
                   [ Node.text "New Game" ]
               ]
           ])
    ]
;;

(* Start the app *)
let () =
  Random.self_init ();
  Bonsai_web.Start.start ~bind_to_element_with_id:"app" component
;;

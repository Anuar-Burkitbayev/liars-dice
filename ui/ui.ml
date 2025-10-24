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
  ; round : Round.t option
  ; round_message : string option
  ; final_winner : Player.t option
  ; bid_count : int
  ; bid_value : int
  }
[@@deriving sexp]

let model_init () : model =
  { game = Game.init ~dice_per_player:5
  ; round = Some (Round.init ~p1_dice:5 ~p2_dice:5)
  ; round_message = None
  ; final_winner = None
  ; bid_count = 1
  ; bid_value = 1
  }
;;

let bid_to_string = function
  | None -> "No bid yet"
  | Some (bid : Bid.t) -> sprintf "%d %ds" bid.count bid.value
;;

let run_ai_until_human (m : model) : model =
  let rec loop m =
    match Game.get_winner m.game, m.round with
    | Some _, _ -> m
    | _, None -> m
    | None, Some round ->
      (match Round.get_current_player round with
       | Player.Player1 -> m
       | Player.Player2 ->
         (match get_logical_move round with
          | `Bid b ->
            (match Round.make_bid round b with
             | Ok new_round -> loop { m with round = Some new_round }
             | Error _ ->
               (* If AI produced invalid bid (shouldn't happen), call liar *)
               (match Round.call_liar round with
                | Ok (winner, _) ->
                  let game' = Game.apply_round_result m.game winner in
                  let final = Game.get_winner game' in
                  { m with
                    game = game'
                  ; round = None
                  ; round_message =
                      Some
                        (if Player.equal winner Player.Player1
                         then "You won the round!"
                         else "You lost the round!")
                  ; final_winner = final
                  }
                | Error _ -> m))
          | `CallLiar ->
            (match Round.call_liar round with
             | Ok (winner, _) ->
               let game' = Game.apply_round_result m.game winner in
               let final = Game.get_winner game' in
               { m with
                 game = game'
               ; round = None
               ; round_message =
                   Some
                     (if Player.equal winner Player.Player1
                      then "You won the round!"
                      else "You lost the round!")
               ; final_winner = final
               }
             | Error _ -> m)))
  in
  loop m
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
  let%arr model = model
  and set_model = set_model in
  let game = model.game in
  let round = model.round in
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
  let game_in_progress = Option.is_none model.final_winner && Option.is_some round in
  let buttons_enabled = game_in_progress && is_player_turn in
  let can_call_liar = buttons_enabled && Option.is_some current_bid in
  (* Handlers *)
  let on_bid_count_input =
    Attr.on_input (fun _ev str ->
      let new_count = Option.value (Int.of_string_opt str) ~default:1 in
      set_model { model with bid_count = Int.max 1 new_count })
  in
  let on_bid_value_input =
    Attr.on_input (fun _ev str ->
      let v = Option.value (Int.of_string_opt str) ~default:1 in
      let v = Int.clamp_exn v ~min:1 ~max:6 in
      set_model { model with bid_value = v })
  in
  let place_bid_handler =
    Attr.on_click (fun _ev ->
      match model.round with
      | None -> Effect.Ignore
      | Some round ->
        if not is_player_turn
        then Effect.Ignore
        else (
          let bid = { Bid.count = model.bid_count; value = model.bid_value } in
          match Round.make_bid round bid with
          | Error _ -> Effect.Ignore
          | Ok new_round ->
            let m' = { model with round = Some new_round } in
            let m'' = run_ai_until_human m' in
            set_model m''))
  in
  let call_liar_handler =
    Attr.on_click (fun _ev ->
      match model.round with
      | None -> Effect.Ignore
      | Some round ->
        if not (is_player_turn && Option.is_some (Round.get_current_bid round))
        then Effect.Ignore
        else (
          match Round.call_liar round with
          | Error _ -> Effect.Ignore
          | Ok (winner, _msg) ->
            let game' = Game.apply_round_result game winner in
            let final = Game.get_winner game' in
            let round_message =
              if Player.equal winner Player.Player1
              then "You won the round!"
              else "You lost the round!"
            in
            set_model
              { model with
                game = game'
              ; round = None
              ; round_message = Some round_message
              ; final_winner = final
              }))
  in
  let overlay_dismiss_handler =
    Attr.on_click (fun _ev ->
      match model.final_winner with
      | Some _ -> set_model (model_init ())
      | None ->
        (* Start a new round *)
        let m' =
          { model with
            round_message = None
          ; round = Some (Round.init ~p1_dice:5 ~p2_dice:5)
          }
        in
        (* If AI somehow starts, let it move *)
        let m'' = run_ai_until_human m' in
        set_model m'')
  in
  (* Inputs for bid (kept minimal to match mockup footprint) *)
  let bid_controls =
    Node.div
      ~attrs:[ Attr.class_ "action-bar" ]
      [ Node.input
          ~attrs:
            [ Attr.type_ "number"
            ; Attr.create "min" "1"
            ; Attr.create "max" "10"
            ; Attr.value (Int.to_string model.bid_count)
            ; (if buttons_enabled then Attr.empty else Attr.bool_property "disabled" true)
            ; on_bid_count_input
            ]
          ()
      ; Node.input
          ~attrs:
            [ Attr.type_ "number"
            ; Attr.create "min" "1"
            ; Attr.create "max" "6"
            ; Attr.value (Int.to_string model.bid_value)
            ; (if buttons_enabled then Attr.empty else Attr.bool_property "disabled" true)
            ; on_bid_value_input
            ]
          ()
      ; Node.button
          ~attrs:
            [ Attr.class_ "btn make-bid"
            ; (if buttons_enabled then Attr.empty else Attr.bool_property "disabled" true)
            ; place_bid_handler
            ]
          [ Node.text "Make Bid" ]
      ; Node.button
          ~attrs:
            [ Attr.class_ "btn call-liar"
            ; (if can_call_liar then Attr.empty else Attr.bool_property "disabled" true)
            ; call_liar_handler
            ]
          [ Node.text "Call Liar" ]
      ]
  in
  (* The view *)
  Node.div
    [ Node.div
        ~attrs:[ Attr.class_ "game-container" ]
        [ (* Opponent's hand (hidden) *)
          Node.div
            ~attrs:[ Attr.class_ "hand" ]
            [ Node.div
                ~attrs:[ Attr.class_ "player-label" ]
                [ Node.text "Opponent (Player 2)" ]
            ; Node.div
                ~attrs:[ Attr.class_ "dice-container" ]
                (List.map p2_hand ~f:(render_die ~hidden:true))
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
            ; bid_controls
            ]
        ]
    ; (* Overlay for round/game message *)
      (match model.round_message, model.final_winner with
       | None, None -> Node.none
       | Some msg, None ->
         Node.div
           ~attrs:
             [ Attr.id "round-message"
             ; Attr.class_ "system-message"
             ; overlay_dismiss_handler
             ]
           [ Node.div ~attrs:[ Attr.class_ "message-content" ] [ Node.text msg ] ]
       | _, Some winner ->
         let msg =
           if Player.equal winner Player.Player1
           then "You won the game!"
           else "You lost the game!"
         in
         Node.div
           ~attrs:
             [ Attr.id "round-message"
             ; Attr.class_ "system-message"
             ; overlay_dismiss_handler
             ]
           [ Node.div ~attrs:[ Attr.class_ "message-content" ] [ Node.text msg ] ])
    ]
;;

(* Start the app *)
let () =
  (* Initialize randomness for dice rolls in JS runtime *)
  Random.self_init ();
  Bonsai_web.Start.start ~bind_to_element_with_id:"app" component
;;

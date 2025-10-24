open! Core
open Logic_library
open Hw2
open Hw4
open Virtual_dom
open! Bonsai.Let_syntax
module Node = Virtual_dom.Vdom.Node
module Attr = Virtual_dom.Vdom.Attr

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

(* Create the main UI component *)
let component =
  (* Create a demo game state *)
  let game = Game.init ~dice_per_player:5 in
  let round = Game.current_round game |> Option.value_exn in
  let p1_hand = Round.hand_of round Player.Player1 in
  let p2_hand = Round.hand_of round Player.Player2 in
  let current_player = Round.get_current_player round in
  let current_bid = Round.get_current_bid round in
  let p1_score = Game.rounds_won_by game Player.Player1 in
  let p2_score = Game.rounds_won_by game Player.Player2 in
  let turn_text =
    match current_player with
    | Player.Player1 -> "Player 1's Turn"
    | Player.Player2 -> "Player 2's Turn"
  in
  let bid_text =
    match current_bid with
    | None -> "No bid yet"
    | Some bid -> sprintf "%d %ds" bid.Bid.count bid.Bid.value
  in
  let view =
    Node.div
      ~attrs:[]
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
              [ Node.h1 ~attrs:[] [ Node.text "Liar's Dice" ]
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
              ; Node.p ~attrs:[ Attr.id "bid-info" ] [ Node.text bid_text ]
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
              ; Node.div
                  ~attrs:[ Attr.class_ "action-bar" ]
                  [ Node.button
                      ~attrs:[ Attr.class_ "btn make-bid" ]
                      [ Node.text "Make Bid" ]
                  ; Node.button
                      ~attrs:
                        [ Attr.class_ "btn call-liar"
                        ; (if Option.is_none current_bid
                           then Attr.create "disabled" "true"
                           else Attr.empty)
                        ]
                      [ Node.text "Call Liar" ]
                  ]
              ]
          ]
      ]
  in
  Bonsai.const view
;;

(* Start the app *)
let () = Bonsai_web.Start.start component

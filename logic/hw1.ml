open! Core

(* ----- Types ----- *)

type player_kind =
  | Player1
  | Player2

type die = int
type hand = die list

type bid =
  { quantity : int
  ; face : die
  }

type decision =
  | In_progress of
      { whose_turn : player_kind
      ; current_bid : bid option
      }
  | Reveal of
      { last_bid : bid
      ; loser : player_kind
      }
  | Winner of player_kind

type game_state =
  { p1_hand : hand
  ; p2_hand : hand
  ; decision : decision
  }

type move =
  | Make_bid of bid
  | Call

(* ----- Example States ----- *)

(* Initial state: both players have 5 dice, Player1 starts *)
let initial_state : game_state =
  { p1_hand = [ 1; 3; 4; 6; 2 ]
  ; p2_hand = [ 2; 5; 3; 3; 6 ]
  ; decision = In_progress { whose_turn = Player1; current_bid = None }
  }
;;

(* Player1 makes the first bid *)
let move_1 : move = Make_bid { quantity = 2; face = 3 }

let state_after_move_1 : game_state =
  { p1_hand = [ 1; 3; 4; 6; 2 ]
  ; p2_hand = [ 2; 5; 3; 3; 6 ]
  ; decision =
      In_progress { whose_turn = Player2; current_bid = Some { quantity = 2; face = 3 } }
  }
;;

(* Player2 raises the bid *)
let move_2 : move = Make_bid { quantity = 3; face = 3 }

let state_after_move_2 : game_state =
  { p1_hand = [ 1; 3; 4; 6; 2 ]
  ; p2_hand = [ 2; 5; 3; 3; 6 ]
  ; decision =
      In_progress { whose_turn = Player1; current_bid = Some { quantity = 3; face = 3 } }
  }
;;

(* Player1 calls — transition to terminal reveal state *)
let move_to_terminal_state : move = Call

let terminal_state : game_state =
  { p1_hand = [ 1; 3; 4; 6; 2 ]
  ; p2_hand = [ 2; 5; 3; 3; 6 ]
  ; decision = Reveal { last_bid = { quantity = 3; face = 3 }; loser = Player2 }
  }
;;

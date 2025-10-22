open! Core

(** Two-player Liar's Dice game (1s count as themselves, not wild). *)

(** Represents which player is acting. *)
type player_kind =
  | Player1
  | Player2

(** Each die shows a number 1–6. *)
type die = int

(** A hand is a list of dice values. *)
type hand = die list

(** A bid claims there are quantity dice showing face across all players. *)
type bid =
  { quantity : int
  ; face : die
  }

(** Overall game decision state. *)
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

(** Full game state. *)
type game_state =
  { p1_hand : hand
  ; p2_hand : hand
  ; decision : decision
  }

(** A move can either make a bid or call the previous one. *)
type move =
  | Make_bid of bid
  | Call

(** Initial game state: both players have fixed hands and Player1 starts. *)
val initial_state : game_state

(** First bid made by Player1. *)
val move_1 : move

(** Game state after move_1. *)
val state_after_move_1 : game_state

(** Second bid made by Player2. *)
val move_2 : move

(** Game state after move_2. *)
val state_after_move_2 : game_state

(** Player1 calls to end the round. *)
val move_to_terminal_state : move

(** Reveal state showing the losing player. *)
val terminal_state : game_state

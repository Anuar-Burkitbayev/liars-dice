open! Core

module Player : sig
  type t =
    | Player1
    | Player2
  [@@deriving sexp, equal, compare]

  val opposite : t -> t
end

module Die : sig
  type t = int [@@deriving sexp, equal]

  val roll : unit -> t
end

module Hand : sig
  type t = Die.t list [@@deriving sexp, equal]

  val roll : int -> t
  val count_value : t -> int -> int
end

module Bid : sig
  type t =
    { count : int
    ; value : int
    }
  [@@deriving sexp, equal, compare]

  val is_higher : previous:t option -> next:t -> bool
end

module Round : sig
  type t =
    { hands : (Player.t * Hand.t) list
    ; current_player : Player.t
    ; current_bid : Bid.t option
    }
  [@@deriving sexp]

  val init : p1_dice:int -> p2_dice:int -> t
  val hand_of : t -> Player.t -> Hand.t
  val total_count_of_value : t -> int -> int
  val make_bid : t -> Bid.t -> t Or_error.t
  val call_liar : t -> (Player.t * string) Or_error.t

  (* Test helper functions *)
  val get_current_bid : t -> Bid.t option
  val get_current_player : t -> Player.t
  val create_with_hands : p1_hand:Hand.t -> p2_hand:Hand.t -> t
  val get_all_moves : t -> [ `Bid of Bid.t | `CallLiar ] list
end

module Game : sig
  type t =
    { current_round : Round.t option
    ; game_winner : Player.t option
    ; rounds_won : (Player.t * int) list
    ; dice_per_player : int
    }
  [@@deriving sexp]

  val init : dice_per_player:int -> t
  val rounds_won_by : t -> Player.t -> int
  val update_rounds_won : t -> Player.t -> t
  val check_game_winner : t -> t
  val apply_round_result : t -> Player.t -> t
  val start_new_round : t -> t
  val next_round_if_possible : t -> t
  val get_winner : t -> Player.t option
  val current_round : t -> Round.t option
end

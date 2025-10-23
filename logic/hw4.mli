open! Core
open Hw2

(** [get_random_move round] returns a random valid move from all available moves
    in the current round. This AI has no strategy and picks moves uniformly at random. *)
val get_random_move : Round.t -> [ `Bid of Bid.t | `CallLiar ]

(** [get_logical_move round] returns a move based on simple logical heuristics.
    The AI will call liar if the bid count is significantly larger than what it has
    in its own hand. Otherwise, it makes conservative bids based on its dice. *)
val get_logical_move : Round.t -> [ `Bid of Bid.t | `CallLiar ]

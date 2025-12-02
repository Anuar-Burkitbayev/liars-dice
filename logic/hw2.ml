open! Core

module Player = struct
  type t =
    | Player1
    | Player2
  [@@deriving sexp, equal, compare]

  let opposite = function
    | Player1 -> Player2
    | Player2 -> Player1
  ;;
end

module Die = struct
  type t = int [@@deriving sexp, equal]

  let roll () = Random.int 6 + 1
end

module Hand = struct
  type t = Die.t list [@@deriving sexp, equal]

  let roll count = List.init count ~f:(fun _ -> Die.roll ())
  let count_value t value = List.count t ~f:(fun die -> Int.equal die value)
end

module Bid = struct
  type t =
    { count : int
    ; value : int
    }
  [@@deriving sexp, equal, compare]

  let is_higher ~previous ~next =
    match previous with
    | None -> true
    | Some prev ->
      next.count > prev.count || (next.count = prev.count && next.value > prev.value)
  ;;
end

module Round = struct
  type t =
    { hands : (Player.t * Hand.t) list
    ; current_player : Player.t
    ; current_bid : Bid.t option
    }
  [@@deriving sexp]

  let init ~p1_dice ~p2_dice =
    { hands = [ Player.Player1, Hand.roll p1_dice; Player.Player2, Hand.roll p2_dice ]
    ; current_player = Player.Player1
    ; current_bid = None
    }
  ;;

  let hand_of t player = List.Assoc.find_exn t.hands player ~equal:Player.equal

  let total_count_of_value t value =
    List.fold t.hands ~init:0 ~f:(fun acc (_, hand) -> acc + Hand.count_value hand value)
  ;;

  let make_bid t (bid : Bid.t) : t Or_error.t =
    if
      bid.count < 1
      || bid.count
         > List.fold t.hands ~init:0 ~f:(fun acc (_, hand) -> acc + List.length hand)
    then Or_error.error_string "Bid count must be between 1 and the total number of dice"
    else if bid.value < 1 || bid.value > 6
    then Or_error.error_string "Bid value must be between 1 and 6"
    else if not (Bid.is_higher ~previous:t.current_bid ~next:bid)
    then Or_error.error_string "Bid must be higher than the previous bid"
    else (
      let next_player = Player.opposite t.current_player in
      Ok { t with current_player = next_player; current_bid = Some bid })
  ;;

  let call_liar t =
    match t.current_bid with
    | None -> Or_error.error_string "No bid to challenge"
    | Some bid ->
      let actual_count = total_count_of_value t bid.value in
      let prev_player = Player.opposite t.current_player in
      let winner, message =
        if actual_count >= bid.count
        then
          ( prev_player
          , sprintf
              "Not a lie! There %s %d %d%s"
              (if actual_count = 1 then "is" else "are")
              actual_count
              bid.value
              (if actual_count = 1 then "" else "s") )
        else
          ( t.current_player
          , sprintf
              "Caught in a lie! There %s only %d %d%s"
              (if actual_count = 1 then "is" else "are")
              actual_count
              bid.value
              (if actual_count = 1 then "" else "s") )
      in
      Ok (winner, message)
  ;;

  let get_all_moves t =
    let valid_bids =
      let max_dice =
        List.fold t.hands ~init:0 ~f:(fun acc (_, hand) -> acc + List.length hand)
      in
      List.concat_map
        (List.range 1 (max_dice + 1))
        ~f:(fun count ->
          List.map (List.range 1 7) ~f:(fun value ->
            let bid = { Bid.count; value } in
            if Bid.is_higher ~previous:t.current_bid ~next:bid
            then Some (`Bid bid)
            else None))
      |> List.filter_opt
    in
    match t.current_bid with
    | None -> valid_bids
    | Some _ -> `CallLiar :: valid_bids
  ;;

  (* Test helper functions *)
  let get_current_bid t = t.current_bid
  let get_current_player t = t.current_player

  let create_with_hands ~p1_hand ~p2_hand =
    { hands = [ Player.Player1, p1_hand; Player.Player2, p2_hand ]
    ; current_player = Player.Player1
    ; current_bid = None
    }
  ;;
end

module Game = struct
  type t =
    { current_round : Round.t option
    ; game_winner : Player.t option
    ; rounds_won : (Player.t * int) list
    ; dice_per_player : int
    }
  [@@deriving sexp]

  let init ~dice_per_player =
    { current_round = Some (Round.init ~p1_dice:dice_per_player ~p2_dice:dice_per_player)
    ; game_winner = None
    ; rounds_won = [ Player.Player1, 0; Player.Player2, 0 ]
    ; dice_per_player
    }
  ;;

  let rounds_won_by t player = List.Assoc.find_exn t.rounds_won player ~equal:Player.equal

  let update_rounds_won t winner =
    let updated_rounds_won =
      List.map t.rounds_won ~f:(fun (player, wins) ->
        if Player.equal player winner then player, wins + 1 else player, wins)
    in
    { t with rounds_won = updated_rounds_won }
  ;;

  let check_game_winner t =
    let winner =
      List.find t.rounds_won ~f:(fun (_, wins) -> wins >= 2) |> Option.map ~f:fst
    in
    { t with game_winner = winner }
  ;;

  let apply_round_result t round_winner =
    let updated_game = update_rounds_won t round_winner in
    check_game_winner updated_game
  ;;

  let start_new_round t =
    { t with
      current_round =
        Some (Round.init ~p1_dice:t.dice_per_player ~p2_dice:t.dice_per_player)
    }
  ;;

  let next_round_if_possible t =
    match t.game_winner with
    | Some _ -> t (* Game is over *)
    | None -> start_new_round t
  ;;

  let get_winner t = t.game_winner
  let current_round t = t.current_round
end

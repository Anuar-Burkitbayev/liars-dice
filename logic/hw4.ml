open! Core
open Hw2

(*
   Due to Liar's Dice being vastly unobservable and stochastic, most common AI algos will struggle
Players can also bluff, so I think reinforcement learning is the only way to go here
Implementing and training such an AI would be very difficult, so I went with a simpler approach
One chooses randomly while another uses basic logic
*)

let get_random_move (round : Round.t) : [ `Bid of Bid.t | `CallLiar ] =
  let valid_moves = Round.get_all_moves round in
  let num_moves = List.length valid_moves in
  let random_index = Random.int num_moves in
  List.nth_exn valid_moves random_index
;;

let get_logical_move (round : Round.t) : [ `Bid of Bid.t | `CallLiar ] =
  let my_hand = Round.hand_of round (Round.get_current_player round) in
  let current_bid = Round.get_current_bid round in
  match current_bid with
  | None ->
    (* No current bid, make a safe initial bid based on what we have *)
    let die_counts =
      List.init 6 ~f:(fun i ->
        let value = i + 1 in
        value, Hand.count_value my_hand value)
    in
    let best_value, best_count =
      List.fold die_counts ~init:(1, 0) ~f:(fun (best_val, best_cnt) (value, count) ->
        if count > best_cnt then value, count else best_val, best_cnt)
    in
    let count = max 1 best_count in
    `Bid { Bid.count; value = best_value }
  | Some bid ->
    (* Check if we should call liar *)
    let my_count = Hand.count_value my_hand bid.value in
    if bid.count > my_count + 2
    then `CallLiar
    else (
      (* Find the closest safest bid *)
      let valid_moves = Round.get_all_moves round in
      let bid_moves =
        List.filter_map valid_moves ~f:(function
          | `Bid b -> Some b
          | `CallLiar -> None)
      in
      (* Find bids we can make based on our hand *)
      let safe_bids =
        List.filter bid_moves ~f:(fun b ->
          let my_count_of_value = Hand.count_value my_hand b.value in
          b.count <= my_count_of_value + 2)
      in
      match safe_bids with
      | [] ->
        (* No safe bids, make  valid bid *)
        (match List.hd bid_moves with
         | Some b -> `Bid b
         | None -> `CallLiar)
      | _ ->
        (* Pick the smallest safe bid (closest to current) *)
        let smallest_safe_bid =
          List.min_elt safe_bids ~compare:Bid.compare |> Option.value_exn
        in
        `Bid smallest_safe_bid)
;;

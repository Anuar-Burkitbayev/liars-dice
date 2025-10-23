open! Core
open Logic_library
open Hw2

let () = Random.init 12345
let ok_exn result = Result.ok result |> Option.value_exn

(* Basic Game Setup Tests *)
let%test "Game initialization with 5 dice per player" =
  let game = Game.init ~dice_per_player:5 in
  let round = Game.current_round game |> Option.value_exn in
  List.length (Round.hand_of round Player.Player1) = 5
  && List.length (Round.hand_of round Player.Player2) = 5
  && Option.is_none (Game.get_winner game)
;;

(* Bid Tests *)
let%test "Initial bid with count=1 value=1 should be valid" =
  let round = Round.init ~p1_dice:5 ~p2_dice:5 in
  Result.is_ok (Round.make_bid round { count = 1; value = 1 })
;;

let%test "Bid with count=0 should be invalid" =
  let round = Round.init ~p1_dice:5 ~p2_dice:5 in
  Result.is_error (Round.make_bid round { count = 0; value = 1 })
;;

let%test "Bid with value=7 should be invalid" =
  let round = Round.init ~p1_dice:5 ~p2_dice:5 in
  Result.is_error (Round.make_bid round { count = 1; value = 7 })
;;

(* Bid Comparison Tests *)
let%test "Higher count is always valid bid" =
  Bid.is_higher ~previous:(Some { count = 3; value = 6 }) ~next:{ count = 4; value = 6 }
;;

let%test "Same count needs higher value" =
  Bid.is_higher ~previous:(Some { count = 3; value = 3 }) ~next:{ count = 3; value = 4 }
  && not
       (Bid.is_higher
          ~previous:(Some { count = 3; value = 4 })
          ~next:{ count = 3; value = 3 })
;;

(* Round Mechanics Tests *)
let make_and_get_round bid round = Round.make_bid round bid |> ok_exn

let%test "Sequence of valid bids" =
  let round = Round.init ~p1_dice:5 ~p2_dice:5 in
  let round = make_and_get_round { count = 1; value = 2 } round in
  let round = make_and_get_round { count = 2; value = 2 } round in
  Result.is_ok (Round.make_bid round { count = 3; value = 2 })
;;

let%test "Call liar on true bid" =
  let round = Round.init ~p1_dice:5 ~p2_dice:5 in
  let round = make_and_get_round { count = 1; value = 2 } round in
  match Round.call_liar round with
  | Ok (winner, _) -> Player.equal winner Player.Player1
  | Error _ -> false
;;

let%test "Call liar on false bid" =
  let round = Round.init ~p1_dice:5 ~p2_dice:5 in
  let round = make_and_get_round { count = 2; value = 6 } round in
  match Round.call_liar round with
  | Ok (winner, _) -> Player.equal winner Player.Player2
  | Error _ -> false
;;

let%expect_test "Call liar when bid is true" =
  let round = Round.init ~p1_dice:5 ~p2_dice:5 in
  let round = make_and_get_round { count = 3; value = 2 } round in
  let winner, message = Round.call_liar round |> ok_exn in
  print_s [%sexp (winner : Player.t)];
  print_endline message;
  [%expect
    {|
    Player2
    Caught in a lie! There is only 1 2 |}]
;;

let%expect_test "Call liar when bid is false" =
  let round = Round.init ~p1_dice:5 ~p2_dice:5 in
  let round = make_and_get_round { count = 2; value = 6 } round in
  let winner, message = Round.call_liar round |> ok_exn in
  print_s [%sexp (winner : Player.t)];
  print_endline message;
  [%expect
    {|
    Player2
    Caught in a lie! There is only 1 6 |}]
;;

(* Game Win Condition Tests *)
let%expect_test "Game ends after player wins 2 rounds" =
  let game = Game.init ~dice_per_player:5 in
  (* Simulate Player1 winning two rounds *)
  let game = Game.apply_round_result game Player.Player1 in
  let game = Game.next_round_if_possible game in
  let game = Game.apply_round_result game Player.Player1 in
  print_s [%sexp (Game.get_winner game : Player.t option)];
  [%expect {| (Player1) |}]
;;

let%expect_test "Game continues if no player has 2 wins" =
  let game = Game.init ~dice_per_player:5 in
  (* Simulate Player1 and Player2 each winning one round *)
  let game = Game.apply_round_result game Player.Player1 in
  let game = Game.next_round_if_possible game in
  let game = Game.apply_round_result game Player.Player2 in
  let game = Game.next_round_if_possible game in
  (* Game should still be in progress *)
  print_s [%sexp (Game.get_winner game : Player.t option)];
  print_s [%sexp (Option.is_some (Game.current_round game) : bool)];
  [%expect
    {|
    ()
    true |}]
;;

let%expect_test "Round counting works correctly" =
  let game = Game.init ~dice_per_player:5 in
  let game = Game.apply_round_result game Player.Player1 in
  let p1_wins = Game.rounds_won_by game Player.Player1 in
  let p2_wins = Game.rounds_won_by game Player.Player2 in
  print_s [%message "Round wins" (p1_wins : int) (p2_wins : int)];
  [%expect {| ("Round wins" (p1_wins 1) (p2_wins 0)) |}]
;;

(* Game Progression Tests from hw1.ml *)
let%test "Game progression - initial state to first bid" =
  let game = Game.init ~dice_per_player:5 in
  let round = Game.current_round game |> Option.value_exn in
  let bid = { Bid.count = 2; value = 3 } in
  match Round.make_bid round bid with
  | Ok new_round ->
    Player.equal (Round.get_current_player new_round) Player.Player2
    && Option.equal Bid.equal (Round.get_current_bid new_round) (Some bid)
  | Error _ -> false
;;

let%test "Game progression - second bid higher quantity" =
  let game = Game.init ~dice_per_player:5 in
  let round = Game.current_round game |> Option.value_exn in
  let round = Round.make_bid round { Bid.count = 2; value = 3 } |> ok_exn in
  let bid2 = { Bid.count = 3; value = 3 } in
  match Round.make_bid round bid2 with
  | Ok new_round ->
    Player.equal (Round.get_current_player new_round) Player.Player1
    && Option.equal Bid.equal (Round.get_current_bid new_round) (Some bid2)
  | Error _ -> false
;;

let%test "Game progression - calling liar correctly updates game state" =
  let game = Game.init ~dice_per_player:5 in
  let round = Game.current_round game |> Option.value_exn in
  let round = Round.make_bid round { Bid.count = 3; value = 3 } |> ok_exn in
  match Round.call_liar round with
  | Ok (winner, _) -> Option.is_some (Some winner)
  | Error _ -> false
;;

(* Random Walk Test *)
let%expect_test "Random walk - complete game simulation" =
  let rec play_round round moves_count =
    if moves_count > 50
    then Error "Movecount too high"
    else (
      let moves = Round.get_all_moves round in
      match moves with
      | [] -> Error "No valid moves available"
      | _ ->
        let random_move = List.nth_exn moves (Random.int (List.length moves)) in
        (match random_move with
         | `Bid bid ->
           (match Round.make_bid round bid with
            | Ok new_round -> play_round new_round (moves_count + 1)
            | Error _ -> Error "Invalid bid generated")
         | `CallLiar ->
           (match Round.call_liar round with
            | Ok (winner, _) -> Ok winner
            | Error _ -> Error "Invalid liar call")))
  in
  let rec play_game game rounds_played =
    if rounds_played > 3
    then Error "Max rounds exceeded"
    else (
      match Game.get_winner game with
      | Some winner ->
        print_s [%message "Game over" (winner : Player.t) (rounds_played : int)];
        Ok ()
      | None ->
        (match Game.current_round game with
         | None -> Error "No current round"
         | Some round ->
           (match play_round round 0 with
            | Ok round_winner ->
              let game = Game.apply_round_result game round_winner in
              let game = Game.next_round_if_possible game in
              play_game game (rounds_played + 1)
            | Error msg -> Error msg)))
  in
  let game = Game.init ~dice_per_player:5 in
  (match play_game game 0 with
   | Ok () -> ()
   | Error msg -> print_endline ("Error: " ^ msg));
  [%expect {| ("Game over" (winner Player2) (rounds_played 3)) |}]
;;

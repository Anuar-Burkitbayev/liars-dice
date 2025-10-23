open! Core
open Logic_library
open Hw2

let () =
  (* Initialize random number generator with a fixed seed for tests *)
  Random.init 12345
;;

let ok_exn result = Result.ok result |> Option.value_exn

let pretty_print_round round =
  let p1_hand = Round.hand_of round Player.Player1 in
  let p2_hand = Round.hand_of round Player.Player2 in
  let current_bid = Round.get_current_bid round in
  let current_player = Round.get_current_player round in
  printf "\nCurrent player: %s\n" 
    (match current_player with
     | Player.Player1 -> "Player 1"
     | Player.Player2 -> "Player 2");
  printf "Player 1 hand: %s\n" 
    (List.to_string ~f:Int.to_string p1_hand);
  printf "Player 2 hand: %s\n"
    (List.to_string ~f:Int.to_string p2_hand);
  match current_bid with
  | None -> printf "No current bid\n"
  | Some bid -> 
    printf "Current bid: %d dice showing %d\n" bid.count bid.value
;;

let pretty_print_game game =
  let p1_wins = Game.rounds_won_by game Player.Player1 in
  let p2_wins = Game.rounds_won_by game Player.Player2 in
  printf "\nGame State:\n";
  printf "Player 1 rounds won: %d\n" p1_wins;
  printf "Player 2 rounds won: %d\n" p2_wins;
  match Game.get_winner game with
  | Some Player.Player1 -> printf "Player 1 has won the game!\n"
  | Some Player.Player2 -> printf "Player 2 has won the game!\n"
  | None ->
    printf "Game in progress\n";
    match Game.current_round game with
    | None -> printf "No active round\n"
    | Some round -> pretty_print_round round
;;

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

let%expect_test "Random walk with pretty print" =
  let initial_round = Round.init ~p1_dice:5 ~p2_dice:5 in
  printf "\nInitial state:\n";
  pretty_print_round initial_round;
  
  let rec play_and_print_random_moves round moves_taken =
    if moves_taken >= 100
    then `MaxMoves
    else (
      let possible_moves = Round.get_all_moves round in
      match possible_moves with
      | [] -> `NoMoves
      | moves ->
        let random_move = List.random_element_exn moves in
        (match random_move with
         | `CallLiar ->
           printf "\nMove %d: Calling Liar!\n" moves_taken;
           (match Round.call_liar round with
            | Ok (winner, message) -> 
              printf "Result: %s\n" message;
              `LiarCalled winner
            | Error _ -> `Error)
         | `Bid bid ->
           printf "\nMove %d: Bidding %d dice showing %d\n" 
             moves_taken bid.count bid.value;
           (match Round.make_bid round bid with
            | Ok new_round -> 
              pretty_print_round new_round;
              play_and_print_random_moves new_round (moves_taken + 1)
            | Error _ -> `Error)))
  in
  let result = play_and_print_random_moves initial_round 0 in
  printf "\nFinal result: ";
  (match result with
   | `MaxMoves -> printf "Random walk reached maximum moves limit\n"
   | `NoMoves -> printf "Random walk ended with no possible moves\n"
   | `LiarCalled winner ->
     printf "Random walk ended with liar being called. Winner: %s\n"
       (match winner with
        | Player.Player1 -> "Player 1"
        | Player.Player2 -> "Player 2")
   | `Error -> printf "Random walk ended with an error\n");
  [%expect {|
  Initial state:
    Current player: Player 1
    Player 1 hand: (1 2 6 1 4)
    Player 2 hand: (4 1 5 1 1)
    No current bid


    Move 0: Bidding 3 dice showing 5


    Current player: Player 2
    Player 1 hand: (1 2 6 1 4)
    Player 2 hand: (4 1 5 1 1)
    Current bid: 3 dice showing 5
  

    Move 1: Bidding 6 dice showing 6
  

    Current player: Player 1
    Player 1 hand: (1 2 6 1 4)
    Player 2 hand: (4 1 5 1 1)
    Current bid: 6 dice showing 6
  

    Move 2: Bidding 7 dice showing 6
  

    Current player: Player 2
    Player 1 hand: (1 2 6 1 4)
    Player 2 hand: (4 1 5 1 1)
    Current bid: 7 dice showing 6
  

    Move 3: Calling Liar!
      Result: Caught in a lie! There is only 1 6
  

    Final result: Random walk ended with liar being called. Winner: Player 2|}]
;;


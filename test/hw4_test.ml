open! Core
open Logic_library.Hw2
open Logic_library.Hw4

let play_single_round ~ai1 ~ai2 round =
  let rec play_round_loop r =
    let current_player = Round.get_current_player r in
    let ai_move =
      match current_player with
      | Player.Player1 -> ai1 r
      | Player.Player2 -> ai2 r
    in
    match ai_move with
    | `CallLiar ->
      (match Round.call_liar r with
       | Ok (winner, _) -> winner
       | Error _ -> failwith "Invalid call_liar")
    | `Bid bid ->
      (match Round.make_bid r bid with
       | Ok new_round -> play_round_loop new_round
       | Error _ -> failwith "Invalid bid")
  in
  play_round_loop round
;;

let play_single_game ~ai1 ~ai2 ~dice_per_player =
  let rec play_game_loop game =
    match Game.get_winner game with
    | Some winner -> winner
    | None ->
      (match Game.current_round game with
       | None -> failwith "No current round"
       | Some round ->
         let round_winner = play_single_round ~ai1 ~ai2 round in
         let updated_game = Game.apply_round_result game round_winner in
         let next_game = Game.next_round_if_possible updated_game in
         play_game_loop next_game)
  in
  let initial_game = Game.init ~dice_per_player in
  play_game_loop initial_game
;;

let play_multiple_games ~ai1 ~ai2 ~num_games ~dice_per_player =
  let results =
    List.init num_games ~f:(fun _ -> play_single_game ~ai1 ~ai2 ~dice_per_player)
  in
  let player1_wins =
    List.count results ~f:(fun winner -> Player.equal winner Player.Player1)
  in
  let player2_wins =
    List.count results ~f:(fun winner -> Player.equal winner Player.Player2)
  in
  player1_wins, player2_wins
;;

(* Random AI vs Logical AI -- Logical should win most games *)

let%expect_test "Random AI vs Logical AI - 10000 games" =
  Random.init 1;
  let player1_wins, player2_wins =
    play_multiple_games
      ~ai1:get_random_move
      ~ai2:get_logical_move
      ~num_games:10000
      ~dice_per_player:5
  in
  printf "Random AI (Player 1) wins: %d\n" player1_wins;
  printf "Logical AI (Player 2) wins: %d\n" player2_wins;
  printf "Total games: %d\n" (player1_wins + player2_wins);
  [%expect
    {|
    Random AI (Player 1) wins: 6
    Logical AI (Player 2) wins: 9994
    Total games: 10000 |}]
;;

(* Putting the AIs against themselves -- same AI matchups should result in close outcomes *)

let%expect_test "Random AI vs Random AI - 10000 games" =
  Random.init 1;
  let player1_wins, player2_wins =
    play_multiple_games
      ~ai1:get_random_move
      ~ai2:get_random_move
      ~num_games:10000
      ~dice_per_player:5
  in
  printf "Random AI (Player 1) wins: %d\n" player1_wins;
  printf "Random AI (Player 2) wins: %d\n" player2_wins;
  printf "Total games: %d\n" (player1_wins + player2_wins);
  [%expect
    {|
    Random AI (Player 1) wins: 4930
    Random AI (Player 2) wins: 5070
    Total games: 10000 |}]
;;

let%expect_test "Logical AI vs Logical AI - 10000 games" =
  Random.init 1;
  let player1_wins, player2_wins =
    play_multiple_games
      ~ai1:get_logical_move
      ~ai2:get_logical_move
      ~num_games:10000
      ~dice_per_player:5
  in
  printf "Logical AI (Player 1) wins: %d\n" player1_wins;
  printf "Logical AI (Player 2) wins: %d\n" player2_wins;
  printf "Total games: %d\n" (player1_wins + player2_wins);
  [%expect
    {|
    Logical AI (Player 1) wins: 5732
    Logical AI (Player 2) wins: 4268
    Total games: 10000 |}]
;;

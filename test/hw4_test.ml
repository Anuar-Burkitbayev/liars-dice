open! Core
open Logic_library
open Hw2
open Hw4
open Hw3

type player_kind_or_empty =
  | E
  | O
  | X

let print_computer_move (board_as_lists : player_kind_or_empty list list) max_depth =
  let board : Player_kind.t Cell_position.Map.t =
    List.mapi board_as_lists ~f:(fun row row_as_list ->
      List.filter_mapi row_as_list ~f:(fun col player_kind_or_empty ->
        let player_kind : Player_kind.t option =
          match player_kind_or_empty with
          | E -> None
          | O -> Some O
          | X -> Some X
        in
        Option.map player_kind ~f:(fun player_kind : (Cell_position.t * Player_kind.t) ->
          { row; column = col }, player_kind)))
    |> List.concat
    |> Cell_position.Map.of_alist_exn
  in
  let whose_turn : Player_kind.t = if Map.length board mod 2 = 0 then X else O in
  let state : Game_state.t =
    { board
    ; rows = 3
    ; columns = 3
    ; winning_sequence_length = 3
    ; decision = In_progress { whose_turn }
    ; last_move = None
    }
  in
  let move = alpha_beta state ~depth:max_depth |> Option.value_exn in
  let next_state = Game_state.make_move state move |> ok_exn in
  print_s [%message "Computer chooses this move" (move : Move.t)];
  print_endline "\nThis transitions the game from this state:";
  pretty_print_board state;
  print_endline "\nTo this state:";
  pretty_print_board next_state
;;

let%expect_test "returns exactly one cell" =
  print_computer_move [ [ O; O; X ]; [ X; X; O ]; [ O; X; E ] ] 1;
  [%expect
    {|
    ("Computer chooses this move" (move ((row 2) (column 2))))

    This transitions the game from this state:
    O|O|X
    -----
    X|X|O
    -----
    O|X|
    (In_progress (whose_turn X))

    To this state:
    O|O|X
    -----
    X|X|O
    -----
    O|X|X
    Stalemate
    |}]
;;

let%expect_test "X finds an immediate winning move" =
  print_computer_move [ [ E; E; O ]; [ O; X; X ]; [ E; X; O ] ] 1;
  [%expect
    {|
    ("Computer chooses this move" (move ((row 0) (column 1))))

    This transitions the game from this state:
     | |O
    -----
    O|X|X
    -----
     |X|O
    (In_progress (whose_turn X))

    To this state:
     |X|O
    -----
    O|X|X
    -----
     |X|O
    (Winner X)
    |}]
;;

let%expect_test "O finds an immediate winning move" =
  print_computer_move [ [ E; E; O ]; [ O; X; X ]; [ O; X; O ] ] 1;
  [%expect
    {|
    ("Computer chooses this move" (move ((row 0) (column 0))))

    This transitions the game from this state:
     | |O
    -----
    O|X|X
    -----
    O|X|O
    (In_progress (whose_turn O))

    To this state:
    O| |O
    -----
    O|X|X
    -----
    O|X|O
    (Winner O)
    |}]
;;

let%expect_test "X prevents an immediate win" =
  print_computer_move [ [ X; E; E ]; [ O; O; E ]; [ X; E; E ] ] 2;
  [%expect
    {|
    ("Computer chooses this move" (move ((row 1) (column 2))))

    This transitions the game from this state:
    X| |
    -----
    O|O|
    -----
    X| |
    (In_progress (whose_turn X))

    To this state:
    X| |
    -----
    O|O|X
    -----
    X| |
    (In_progress (whose_turn O))
    |}]
;;

let%expect_test "O prevents an immediate win" =
  print_computer_move [ [ X; X; E ]; [ O; E; E ]; [ E; E; E ] ] 2;
  [%expect
    {|
    ("Computer chooses this move" (move ((row 0) (column 2))))

    This transitions the game from this state:
    X|X|
    -----
    O| |
    -----
     | |
    (In_progress (whose_turn O))

    To this state:
    X|X|O
    -----
    O| |
    -----
     | |
    (In_progress (whose_turn X))
    |}]
;;

let%expect_test "O prevents another immediate win" =
  print_computer_move [ [ X; O; E ]; [ X; O; E ]; [ E; X; E ] ] 2;
  [%expect
    {|
    ("Computer chooses this move" (move ((row 2) (column 0))))

    This transitions the game from this state:
    X|O|
    -----
    X|O|
    -----
     |X|
    (In_progress (whose_turn O))

    To this state:
    X|O|
    -----
    X|O|
    -----
    O|X|
    (In_progress (whose_turn X))
    |}]
;;

let%expect_test "X finds a winning move that will lead to winning in 2 steps" =
  print_computer_move [ [ X; E; E ]; [ O; X; E ]; [ E; E; O ] ] 3;
  [%expect
    {|
    ("Computer chooses this move" (move ((row 0) (column 1))))

    This transitions the game from this state:
    X| |
    -----
    O|X|
    -----
     | |O
    (In_progress (whose_turn X))

    To this state:
    X|X|
    -----
    O|X|
    -----
     | |O
    (In_progress (whose_turn O))
    |}]
;;

let%expect_test "O finds a winning move that will lead to winning in 2 steps" =
  print_computer_move [ [ E; X; E ]; [ X; X; O ]; [ E; O; E ] ] 3;
  [%expect
    {|
    ("Computer chooses this move" (move ((row 2) (column 2))))

    This transitions the game from this state:
     |X|
    -----
    X|X|O
    -----
     |O|
    (In_progress (whose_turn O))

    To this state:
     |X|
    -----
    X|X|O
    -----
     |O|O
    (In_progress (whose_turn X))
    |}]
;;

let%expect_test "O finds a cool winning move that will lead to winning in 2 steps" =
  print_computer_move [ [ X; O; X ]; [ X; E; E ]; [ O; E; E ] ] 3;
  [%expect
    {|
    ("Computer chooses this move" (move ((row 2) (column 1))))

    This transitions the game from this state:
    X|O|X
    -----
    X| |
    -----
    O| |
    (In_progress (whose_turn O))

    To this state:
    X|O|X
    -----
    X| |
    -----
    O|O|
    (In_progress (whose_turn X))
    |}]
;;

let%expect_test "O finds the wrong move due to small depth" =
  print_computer_move [ [ X; E; E ]; [ E; E; E ]; [ E; E; E ] ] 3;
  [%expect
    {|
    ("Computer chooses this move" (move ((row 0) (column 1))))

    This transitions the game from this state:
    X| |
    -----
     | |
    -----
     | |
    (In_progress (whose_turn O))

    To this state:
    X|O|
    -----
     | |
    -----
     | |
    (In_progress (whose_turn X))
    |}]
;;

let%expect_test "O finds the correct move when depth is big enough" =
  print_computer_move [ [ X; E; E ]; [ E; E; E ]; [ E; E; E ] ] 6;
  [%expect
    {|
    ("Computer chooses this move" (move ((row 1) (column 1))))

    This transitions the game from this state:
    X| |
    -----
     | |
    -----
     | |
    (In_progress (whose_turn O))

    To this state:
    X| |
    -----
     |O|
    -----
     | |
    (In_progress (whose_turn X))
    |}]
;;

let%expect_test "X finds a winning move that will lead to winning in 2 steps" =
  print_computer_move [ [ E; E; E ]; [ O; X; E ]; [ E; E; E ] ] 5;
  [%expect
    {|
    ("Computer chooses this move" (move ((row 0) (column 0))))

    This transitions the game from this state:
     | |
    -----
    O|X|
    -----
     | |
    (In_progress (whose_turn X))

    To this state:
    X| |
    -----
    O|X|
    -----
     | |
    (In_progress (whose_turn O))
    |}]
;;

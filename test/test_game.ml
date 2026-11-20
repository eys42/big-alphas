(** Game tests: test the rules/API *)

open OUnit2
open Big_alphas

let new_game () = Game.create ~seed:42 Strategy.Easy

let assert_invalid_argument f =
  try
    ignore (f ());
    assert_failure "Expected Invalid_argument, but no exception was raised"
  with Invalid_argument _ -> ()

(** Find a coordinate whose *actual* board cell satisfies [predicate]. This
    avoids relying on ship locations or a fixed board size. *)
let find_cell board predicate =
  let size = Board.dim board in
  let rec search x y =
    if x >= size then assert_failure "No cell matched the test condition"
    else if y >= size then search (x + 1) 0
    else if predicate (Board.cell_at_own board x y) then (x, y)
    else search x (y + 1)
  in
  search 0 0

let is_empty = function
  | Board.Empty -> true
  | _ -> false

let is_unhit_ship = function
  | Board.Occupied (_, false) -> true
  | _ -> false

let test_initial_state _ =
  let game = new_game () in
  assert_equal Game.Human (Game.current_player game);
  assert_equal None (Game.winner game);
  assert_equal
    (Board.dim (Game.human_board game))
    (Board.dim (Game.opponent_board game))

let test_human_miss _ =
  let game = new_game () in
  let board = Game.opponent_board game in
  let x, y = find_cell board is_empty in
  assert_equal Board.Miss (Game.human_strike game x y);
  assert_equal Board.Miss (Board.cell_at_own board x y);
  assert_equal Game.Computer (Game.current_player game);
  assert_equal None (Game.winner game)

let test_human_hit _ =
  let game = new_game () in
  let board = Game.opponent_board game in
  let x, y = find_cell board is_unhit_ship in
  (match Game.human_strike game x y with
  | Board.Occupied (_, true) | Board.Sunk _ -> ()
  | _ -> assert_failure "Attacking a ship must register a hit or sink");
  assert_bool "Attacked ship square should no longer be strikeable"
    (not (Strategy.strikeable board x y));
  assert_equal Game.Computer (Game.current_player game)

let test_out_of_bounds_preserves_turn _ =
  let game = new_game () in
  let size = Board.dim (Game.opponent_board game) in
  assert_invalid_argument (fun () -> Game.human_strike game (-1) 0);
  assert_invalid_argument (fun () -> Game.human_strike game size 0);
  assert_equal Game.Human (Game.current_player game);
  assert_equal None (Game.winner game)

let test_wrong_turn_preserves_board _ =
  let game = new_game () in
  let board = Game.opponent_board game in
  let x, y = find_cell board is_empty in
  ignore (Game.human_strike game x y);
  let x2, y2 = find_cell board is_empty in
  assert_invalid_argument (fun () -> Game.human_strike game x2 y2);
  assert_equal Board.Empty (Board.cell_at_own board x2 y2);
  assert_equal Game.Computer (Game.current_player game)

let test_computer_turn _ =
  let game = new_game () in
  let board = Game.opponent_board game in
  let x, y = find_cell board is_empty in
  ignore (Game.human_strike game x y);
  let human_board = Game.human_board game in
  let (mx, my), _result = Game.machine_strike game in
  let size = Board.dim human_board in
  assert_bool "Computer x in bounds" (mx >= 0 && mx < size);
  assert_bool "Computer y in bounds" (my >= 0 && my < size);
  assert_bool "Computer's shot is now marked as used"
    (not (Strategy.strikeable human_board mx my));
  assert_equal Game.Human (Game.current_player game)

let test_repeated_attack_preserves_turn _ =
  let game = new_game () in
  let board = Game.opponent_board game in
  let x, y = find_cell board is_empty in
  ignore (Game.human_strike game x y);
  ignore (Game.machine_strike game);
  assert_invalid_argument (fun () -> Game.human_strike game x y);
  assert_equal Game.Human (Game.current_player game);
  assert_equal Board.Miss (Board.cell_at_own board x y)

let test_win_and_game_over _ =
  let game = new_game () in
  let size = Board.dim (Game.opponent_board game) in
  let rec play fuel =
    if fuel = 0 then assert_failure "The game never reached a winner"
    else
      match Game.winner game with
      | Some _ -> ()
      | None ->
          (match Game.current_player game with
          | Game.Human ->
              (* Inspect the board only to pick a winning move in the test.
                 Actual gameplay still goes through Game.human_strike. *)
              let x, y = find_cell (Game.opponent_board game) is_unhit_ship in
              ignore (Game.human_strike game x y)
          | Game.Computer -> ignore (Game.machine_strike game));
          play (fuel - 1)
  in
  play ((2 * size * size) + 2);
  assert_equal (Some Game.Human) (Game.winner game);
  assert_equal Game.Human (Game.current_player game);
  assert_invalid_argument (fun () -> Game.human_strike game 0 0);
  assert_invalid_argument (fun () -> Game.machine_strike game)

let suite =
  "Game rules"
  >::: [
         "initial state" >:: test_initial_state;
         "human miss updates board and turn" >:: test_human_miss;
         "human hit records attack" >:: test_human_hit;
         "out of bounds does not use turn" >:: test_out_of_bounds_preserves_turn;
         "wrong player cannot attack" >:: test_wrong_turn_preserves_board;
         "computer attack is valid" >:: test_computer_turn;
         "cannot attack the same square twice"
         >:: test_repeated_attack_preserves_turn;
         "winning ends the game" >:: test_win_and_game_over;
       ]

let () = run_test_tt_main suite

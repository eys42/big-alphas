open OUnit2
open Big_alphas

let ship orientation anchor length = { Board.anchor; orientation; length }

(** [all_cells board] returns a list of all cells on the board in row-major
    order. *)
let all_cells board =
  let dim = Board.dim board in
  List.init (dim * dim) (fun index ->
      Board.cell_at board (index / dim) (index mod dim))

(** [count_cells predicate board] counts the number of cells in [board] that
    satisfy [predicate]. *)
let count_cells predicate board =
  List.length (List.filter predicate (all_cells board))

(** [is_empty cell] checks if [cell] is empty. *)
let is_empty = function
  | Board.Empty -> true
  | _ -> false

(** [is_treasure cell] checks if [cell] contains a treasure chest. *)
let is_treasure = function
  | Board.Treasure_chest _ -> true
  | _ -> false

(** [is_occupied cell] checks if [cell] is occupied by a ship. *)
let is_occupied = function
  | Board.Occupied _ -> true
  | _ -> false

(** [assert_boards_equal expected actual] asserts that the two boards are equal.
    If assertion fails, message is printed with the coordinates of the first
    differing cell. *)
let assert_boards_equal expected actual =
  let dim = Board.dim expected in
  assert_equal dim (Board.dim actual);
  for y = 0 to dim - 1 do
    for x = 0 to dim - 1 do
      assert_equal
        ~msg:(Printf.sprintf "boards differ at (%d, %d)" x y)
        (Board.cell_at expected x y)
        (Board.cell_at actual x y)
    done
  done

let test_empty_small_board _ =
  let board = Board.init 3 [] 17 in
  assert_equal 7 (count_cells is_empty board);
  assert_equal 2 (count_cells is_treasure board);
  assert_equal 0 (count_cells is_occupied board)

let test_small_board_with_ship _ =
  let carrier = ship Board.East (0, 0) 3 in
  let board = Board.init 3 [ carrier ] 23 in
  assert_equal 3 (count_cells is_occupied board);
  assert_equal 2 (count_cells is_treasure board);
  assert_equal (Board.Occupied carrier) (Board.cell_at board 0 0);
  assert_equal (Board.Occupied carrier) (Board.cell_at board 0 1);
  assert_equal (Board.Occupied carrier) (Board.cell_at board 0 2)

let test_normal_board _ =
  let ships =
    [
      ship Board.East (0, 0) 5;
      ship Board.South (6, 0) 4;
      ship Board.South (2, 2) 3;
      ship Board.East (4, 5) 3;
      ship Board.West (9, 9) 2;
    ]
  in
  let board = Board.init 10 ships 31 in
  assert_equal 17 (count_cells is_occupied board);
  assert_equal 2 (count_cells is_treasure board);
  assert_equal 81 (count_cells is_empty board)

let test_board_dimension _ =
  assert_equal 3 (Board.dim (Board.init 3 [] 17));
  assert_equal 10 (Board.dim (Board.init 10 [] 17))

let test_rejects_nonpositive_dimension _ =
  assert_raises (Invalid_argument "Dimension must be positive") (fun () ->
      Board.init 0 [] 0);
  assert_raises (Invalid_argument "Dimension must be positive") (fun () ->
      Board.init (-1) [] 0)

let test_rejects_excessive_coverage _ =
  let ships = List.init 4 (fun index -> ship Board.East (index, 0) 1) in
  assert_raises (Invalid_argument "Ships cover more than 1/3 of the board")
    (fun () -> Board.init 3 ships 0)

let test_rejects_out_of_bounds_ships _ =
  let cases =
    [
      ship Board.East (-1, 0) 1;
      ship Board.East (0, 9) 2;
      ship Board.South (9, 0) 2;
      ship Board.East (10, 0) 1;
    ]
  in
  List.iter
    (fun invalid_ship ->
      assert_raises (Invalid_argument "Ship out of bounds") (fun () ->
          Board.init 10 [ invalid_ship ] 0))
    cases

let test_rejects_overlapping_ships _ =
  let ships = [ ship Board.East (0, 0) 2; ship Board.East (0, 1) 2 ] in
  assert_raises (Invalid_argument "Ship placement overlaps with another ship")
    (fun () -> Board.init 10 ships 0)

let test_same_seed_is_deterministic _ =
  let ships = [ ship Board.East (0, 0) 3; ship Board.South (5, 5) 2 ] in
  let first = Board.init 10 ships 99 in
  let second = Board.init 10 ships 99 in
  assert_boards_equal first second

let suite =
  "Board.init"
  >::: [
         "empty small board" >:: test_empty_small_board;
         "small board with ship" >:: test_small_board_with_ship;
         "normal board" >:: test_normal_board;
         "board dimension" >:: test_board_dimension;
         "rejects nonpositive dimension" >:: test_rejects_nonpositive_dimension;
         "rejects excessive coverage" >:: test_rejects_excessive_coverage;
         "rejects out of bounds ships" >:: test_rejects_out_of_bounds_ships;
         "rejects overlapping ships" >:: test_rejects_overlapping_ships;
         "same seed is deterministic" >:: test_same_seed_is_deterministic;
       ]

let () = run_test_tt_main suite

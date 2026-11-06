open OUnit2
open Big_alphas

(** [all_cells board] returns a list of all cells on the board in row-major
    order. *)
let all_cells board =
  let dim = Board.dim board in
  List.init (dim * dim) (fun index ->
      Board.cell_at_own board (index / dim) (index mod dim))

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
        (Board.cell_at_own expected x y)
        (Board.cell_at_own actual x y)
    done
  done

(** [list_coordinates board] returns a list of all coordinates on the board in
    row-major order. *)
let list_coordinates board =
  let dim = Board.dim board in
  List.init (dim * dim) (fun index -> (index / dim, index mod dim))

(** [find_coordinate predicate board] returns the first coordinate on the board
    that satisfies the given predicate. Uses visibility of player's own board,
    not the opponent's. *)
let find_coordinate predicate board =
  List.find
    (fun (x, y) -> predicate (Board.cell_at_own board x y))
    (list_coordinates board)

let test_empty_small_board _ =
  let board = Board.init 3 [] 17 in
  assert_equal 7 (count_cells is_empty board);
  assert_equal 2 (count_cells is_treasure board);
  assert_equal 0 (count_cells is_occupied board)

let test_small_board_with_ship _ =
  let carrier = Board.Ship.create (0, 0) Board.East 3 in
  let board = Board.init 3 [ carrier ] 23 in
  assert_equal 3 (count_cells is_occupied board);
  assert_equal 2 (count_cells is_treasure board);
  assert_equal (Board.Occupied (carrier, false)) (Board.cell_at_own board 0 0);
  assert_equal (Board.Occupied (carrier, false)) (Board.cell_at_own board 0 1);
  assert_equal (Board.Occupied (carrier, false)) (Board.cell_at_own board 0 2)

let test_normal_board _ =
  let ships =
    [
      Board.Ship.create (0, 0) Board.East 5;
      Board.Ship.create (6, 0) Board.South 4;
      Board.Ship.create (2, 2) Board.South 3;
      Board.Ship.create (4, 5) Board.East 3;
      Board.Ship.create (9, 9) Board.West 2;
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
  let ships =
    List.init 4 (fun index -> Board.Ship.create (index, 0) Board.East 1)
  in
  assert_raises (Invalid_argument "Ships cover more than 1/3 of the board")
    (fun () -> Board.init 3 ships 0)

let test_rejects_out_of_bounds_ships _ =
  let cases =
    [
      Board.Ship.create (-1, 0) Board.East 1;
      Board.Ship.create (0, 9) Board.East 2;
      Board.Ship.create (9, 0) Board.South 2;
      Board.Ship.create (0, 3) Board.North 4;
      Board.Ship.create (10, 0) Board.East 1;
    ]
  in
  List.iter
    (fun invalid_ship ->
      assert_raises (Invalid_argument "Ship out of bounds") (fun () ->
          Board.init 10 [ invalid_ship ] 0))
    cases

let test_rejects_overlapping_ships _ =
  let ships =
    [
      Board.Ship.create (0, 0) Board.East 2;
      Board.Ship.create (0, 1) Board.East 2;
    ]
  in
  assert_raises (Invalid_argument "Ship placement overlaps with another ship")
    (fun () -> Board.init 10 ships 0)

let test_same_seed_is_deterministic _ =
  let ships =
    [
      Board.Ship.create (0, 0) Board.East 3;
      Board.Ship.create (5, 5) Board.South 2;
    ]
  in
  let first = Board.init 10 ships 99 in
  let second = Board.init 10 ships 99 in
  assert_boards_equal first second

let board_init =
  "Board.init and Board.cell_at_own"
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

let test_cell_at_opponent_rejects_out_of_bounds _ =
  let board = Board.init 5 [] 17 in
  let cases = [ (-1, 0); (5, 0); (0, -1); (0, 5) ] in
  List.iter
    (fun (x, y) ->
      assert_raises (Invalid_argument "Coordinates out of bounds") (fun () ->
          Board.cell_at_opponent board x y))
    cases

let test_strike_rejects_out_of_bounds _ =
  let board = Board.init 5 [] 17 in
  let cases = [ (-1, 0); (5, 0); (0, -1); (0, 5) ] in
  List.iter
    (fun (x, y) ->
      assert_raises (Invalid_argument "Coordinates out of bounds") (fun () ->
          Board.strike board x y))
    cases

let test_cell_at_opponent_initial_state _ =
  let board = Board.init 5 [ Board.Ship.create (0, 0) Board.East 2 ] 17 in
  List.iter
    (fun (x, y) ->
      match Board.cell_at_opponent board x y with
      | Board.Hidden -> ()
      | _ ->
          assert_failure
            (Printf.sprintf
               "unexpected opponent cell at (%d, %d) immediately after init" x y))
    (list_coordinates board)

let test_strike_updates_opponent_cells _ =
  let carrier = Board.Ship.create (0, 0) Board.East 2 in
  let board = Board.init 5 [ carrier ] 17 in
  let empty_x, empty_y =
    find_coordinate
      (function
        | Board.Empty -> true
        | _ -> false)
      board
  in
  assert_equal Board.Miss (Board.strike board empty_x empty_y);
  assert_equal Board.Miss (Board.cell_at_opponent board empty_x empty_y);
  assert_equal (Board.Occupied (carrier, true)) (Board.strike board 0 0);
  assert_equal
    (Board.Occupied (carrier, true))
    (Board.cell_at_opponent board 0 0);
  assert_equal (Board.Sunk carrier) (Board.strike board 0 1);
  assert_equal (Board.Sunk carrier) (Board.cell_at_opponent board 0 1);
  assert_equal (Board.Sunk carrier) (Board.cell_at_opponent board 0 0)

let test_strike_collects_treasure _ =
  let board = Board.init 5 [] 17 in
  let x, y =
    find_coordinate
      (function
        | Board.Treasure_chest ((Board.Nuke | Board.Radar), _) -> true
        | _ -> false)
      board
  in
  let powerup =
    match Board.cell_at_own board x y with
    | Board.Treasure_chest (powerup, _) -> powerup
    | _ ->
        assert_failure
          (Printf.sprintf "expected a treasure chest at (%d, %d)" x y)
  in
  assert_equal (Board.Treasure_chest (powerup, true)) (Board.strike board x y);
  assert_equal
    (Board.Treasure_chest (Board.Collected, true))
    (Board.cell_at_opponent board x y)

let test_strike_rejects_already_hit_cells _ =
  let carrier = Board.Ship.create (0, 0) Board.East 2 in
  let board = Board.init 5 [ carrier ] 17 in
  assert_equal (Board.Occupied (carrier, true)) (Board.strike board 0 0);
  assert_raises (Invalid_argument "Can't hit the same cell twice") (fun () ->
      Board.strike board 0 0);
  assert_equal (Board.Sunk carrier) (Board.strike board 0 1);
  assert_raises (Invalid_argument "Can't hit the same cell twice") (fun () ->
      Board.strike board 0 1);
  let empty_x, empty_y =
    find_coordinate
      (function
        | Board.Empty -> true
        | _ -> false)
      board
  in
  assert_equal Board.Miss (Board.strike board empty_x empty_y);
  assert_raises (Invalid_argument "Can't hit the same cell twice") (fun () ->
      Board.strike board empty_x empty_y);
  let treasure_board = Board.init 5 [] 17 in
  let treasure_x, treasure_y =
    find_coordinate
      (function
        | Board.Treasure_chest ((Board.Nuke | Board.Radar), _) -> true
        | _ -> false)
      treasure_board
  in
  ignore (Board.strike treasure_board treasure_x treasure_y);
  assert_raises (Invalid_argument "Can't hit a collected treasure chest")
    (fun () -> Board.strike treasure_board treasure_x treasure_y)

let board_opponent =
  "Board.cell_at_opponent and Board.strike"
  >::: [
         "cell_at_opponent rejects out of bounds"
         >:: test_cell_at_opponent_rejects_out_of_bounds;
         "strike rejects out of bounds" >:: test_strike_rejects_out_of_bounds;
         "cell_at_opponent initial state"
         >:: test_cell_at_opponent_initial_state;
         "strike updates opponent cells" >:: test_strike_updates_opponent_cells;
         "strike collects treasure" >:: test_strike_collects_treasure;
         "strike rejects already hit cells"
         >:: test_strike_rejects_already_hit_cells;
       ]

let test_reveal_rejects_invalid_arguments _ =
  let board = Board.init 5 [] 17 in
  let out_of_bounds = [ (-1, 0); (5, 0); (0, -1); (0, 5) ] in
  List.iter
    (fun (x, y) ->
      assert_raises (Invalid_argument "Coordinates out of bounds") (fun () ->
          Board.reveal board x y))
    out_of_bounds;
  let x, y =
    find_coordinate
      (function
        | Board.Treasure_chest (_, _) -> true
        | _ -> false)
      board
  in
  ignore (Board.reveal board x y);
  assert_raises
    (Invalid_argument "Can't reveal a cell that has already been revealed")
    (fun () -> Board.reveal board x y)

let test_reveal_rejects_already_revealed_cells _ =
  let board = Board.init 5 [] 17 in
  let empty_x, empty_y =
    find_coordinate
      (function
        | Board.Empty -> true
        | _ -> false)
      board
  in
  ignore (Board.strike board empty_x empty_y);
  assert_raises
    (Invalid_argument "Can't reveal a cell that has already been revealed")
    (fun () -> Board.reveal board empty_x empty_y);
  let ship = Board.Ship.create (0, 0) Board.East 1 in
  let ship_board = Board.init 5 [ ship ] 17 in
  assert_raises
    (Invalid_argument "Can't reveal a cell that has already been revealed")
    (fun () ->
      ignore (Board.strike ship_board 0 0);
      Board.reveal ship_board 0 0)

let test_reveal_empty_cell _ =
  let board = Board.init 5 [] 17 in
  let x, y =
    find_coordinate
      (function
        | Board.Empty -> true
        | _ -> false)
      board
  in
  assert_equal (Board.Revealed None) (Board.reveal board x y);
  assert_equal (Board.Revealed None) (Board.cell_at_own board x y);
  assert_equal (Board.Revealed None) (Board.cell_at_opponent board x y);
  assert_raises
    (Invalid_argument "Can't reveal a cell that has already been revealed")
    (fun () -> Board.reveal board x y)

let test_reveal_occupied_cell _ =
  let carrier = Board.Ship.create (0, 0) Board.East 1 in
  let board = Board.init 5 [ carrier ] 17 in
  assert_equal (Board.Revealed (Some carrier)) (Board.reveal board 0 0);
  assert_equal (Board.Revealed (Some carrier)) (Board.cell_at_own board 0 0);
  assert_equal (Board.Revealed (Some carrier))
    (Board.cell_at_opponent board 0 0);
  assert_raises
    (Invalid_argument "Can't reveal a cell that has already been revealed")
    (fun () -> Board.reveal board 0 0)

let test_reveal_treasure_chest _ =
  let board = Board.init 5 [] 17 in
  let x, y =
    find_coordinate
      (function
        | Board.Treasure_chest ((Board.Nuke | Board.Radar), false) -> true
        | _ -> false)
      board
  in
  let powerup =
    match Board.cell_at_own board x y with
    | Board.Treasure_chest (powerup, false) -> powerup
    | _ -> assert_failure "expected an unrevealed treasure chest"
  in
  assert_equal (Board.Treasure_chest (powerup, true)) (Board.reveal board x y);
  assert_equal
    (Board.Treasure_chest (powerup, true))
    (Board.cell_at_own board x y);
  assert_equal
    (Board.Treasure_chest (powerup, true))
    (Board.cell_at_opponent board x y);
  assert_raises
    (Invalid_argument "Can't reveal a cell that has already been revealed")
    (fun () -> Board.reveal board x y)

let board_reveal =
  "Board.reveal"
  >::: [
         "rejects invalid arguments" >:: test_reveal_rejects_invalid_arguments;
         "rejects already-revealed cells"
         >:: test_reveal_rejects_already_revealed_cells;
         "reveals empty cell" >:: test_reveal_empty_cell;
         "reveals occupied cell" >:: test_reveal_occupied_cell;
         "reveals treasure chest" >:: test_reveal_treasure_chest;
       ]

let suite = "Board" >::: [ board_init; board_opponent; board_reveal ]
let () = run_test_tt_main suite

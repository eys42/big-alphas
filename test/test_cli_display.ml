open OUnit2
open Big_alphas

(** [capture_output f] captures anything printed to stdout by [f]. *)
let capture_output f =
  let filename = Filename.temp_file "display_test" ".txt" in
  let original_stdout = Unix.dup Unix.stdout in
  let output_fd =
    Unix.openfile filename [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600
  in
  flush stdout;
  Unix.dup2 output_fd Unix.stdout;
  Unix.close output_fd;
  (try
     f ();
     flush stdout;
     Unix.dup2 original_stdout Unix.stdout;
     Unix.close original_stdout
   with e ->
     flush stdout;
     Unix.dup2 original_stdout Unix.stdout;
     Unix.close original_stdout;
     raise e);
  let input = open_in filename in
  let result = really_input_string input (in_channel_length input) in
  close_in input;
  Sys.remove filename;
  result

(** [make_board ()] creates a small board with one ship. *)
let make_board () =
  let ship = Board.Ship.create (0, 0) Board.East 2 in
  (Board.init 5 [ ship ] 17, ship)

let test_prints_coordinates _ =
  let board = Board.init 3 [] 17 in
  let output = capture_output (fun () -> Display.print_board board true) in
  assert_bool "should print column coordinates" (String.length output > 0);
  assert_bool "should print row coordinates"
    (try
       ignore (Str.search_forward (Str.regexp "0") output 0);
       true
     with Not_found -> false)

let test_own_board_shows_unhit_ship _ =
  let board, _ = make_board () in
  let output = capture_output (fun () -> Display.print_board board true) in
  assert_bool "own board should show unhit ships as S"
    (try
       ignore (Str.search_forward (Str.regexp_string "S") output 0);
       true
     with Not_found -> false)

let test_west_ship_displays_on_same_row _ =
  let ship = Board.Ship.create (1, 1) Board.West 2 in
  let board = Board.init 5 [ ship ] 17 in
  let output = capture_output (fun () -> Display.print_board board true) in
  let rows = String.split_on_char '\n' output in
  let row = List.nth rows 2 in
  assert_equal 'S' row.[6];
  assert_equal 'S' row.[9]

let test_opponent_board_hides_unhit_ship _ =
  let board, _ = make_board () in
  let output = capture_output (fun () -> Display.print_board board false) in
  assert_bool "opponent board should hide unhit ships"
    (not (String.contains output 'S'))

let test_hit_ship _ =
  let board, _ = make_board () in
  ignore (Board.strike board 0 0);
  let output = capture_output (fun () -> Display.print_board board true) in
  assert_bool "hit ship should be displayed as *" (String.contains output '*')

let test_miss _ =
  let board = Board.init 5 [] 17 in
  let x, y =
    let rec find x y =
      match Board.cell_at_own board x y with
      | Board.Empty -> (x, y)
      | _ when x < 4 -> find (x + 1) y
      | _ -> find 0 (y + 1)
    in
    find 0 0
  in
  ignore (Board.strike board x y);
  let output = capture_output (fun () -> Display.print_board board false) in
  assert_bool "miss should be displayed as X" (String.contains output 'X')

let test_revealed_empty_cell _ =
  let board = Board.init 5 [] 17 in
  let rec find x y =
    match Board.cell_at_own board x y with
    | Board.Empty -> (x, y)
    | _ when x < 4 -> find (x + 1) y
    | _ -> find 0 (y + 1)
  in
  let x, y = find 0 0 in
  ignore (Board.reveal board x y);
  let output = capture_output (fun () -> Display.print_board board false) in
  assert_bool "revealed empty cell should be displayed as ~"
    (String.contains output '~')

let test_sunk_regular_ship _ =
  let ship = Board.Ship.create (0, 0) Board.East 2 in
  let board = Board.init 5 [ ship ] 17 in
  ignore (Board.strike board 0 0);
  ignore (Board.strike board 0 1);
  let output = capture_output (fun () -> Display.print_board board true) in
  assert_bool "sunk regular ship should be displayed as #"
    (String.contains output '#')

let test_sunk_golden_ship _ =
  let ship = Board.Ship.create ~is_golden:true (0, 0) Board.East 2 in
  let board = Board.init 5 [ ship ] 17 in
  ignore (Board.strike board 0 0);
  ignore (Board.strike board 0 1);
  let output = capture_output (fun () -> Display.print_board board true) in
  assert_bool "sunk golden ship should be displayed as G"
    (String.contains output 'G')

let test_collected_treasure _ =
  let board = Board.init 5 [] 17 in
  let rec find x y =
    match Board.cell_at_own board x y with
    | Board.Treasure_chest _ -> (x, y)
    | _ when x < 4 -> find (x + 1) y
    | _ -> find 0 (y + 1)
  in
  let x, y = find 0 0 in
  ignore (Board.strike board x y);
  let output = capture_output (fun () -> Display.print_board board true) in
  assert_bool "collected treasure should be displayed as C"
    (String.contains output 'C')

let display_tests =
  "Display.print_board"
  >::: [
         "prints coordinates" >:: test_prints_coordinates;
         "own board shows unhit ship" >:: test_own_board_shows_unhit_ship;
         "west ship displays on same row" >:: test_west_ship_displays_on_same_row;
         "opponent board hides unhit ship"
         >:: test_opponent_board_hides_unhit_ship;
         "hit ship" >:: test_hit_ship;
         "miss" >:: test_miss;
         "revealed empty cell" >:: test_revealed_empty_cell;
         "sunk regular ship" >:: test_sunk_regular_ship;
         "sunk golden ship" >:: test_sunk_golden_ship;
         "collected treasure" >:: test_collected_treasure;
       ]

(** Tests for CLI gameplay command parsing. *)

let test_attack_command _ =
  assert_equal (Cli.Attack (3, 5)) (Cli.parse_command "3 5");
  assert_equal (Cli.Attack (3, 5)) (Cli.parse_command "s 3 5");
  assert_equal (Cli.Attack (3, 5)) (Cli.parse_command "strike 3 5")

let test_board_command _ =
  assert_equal Cli.Show_board (Cli.parse_command "b");
  assert_equal Cli.Show_board (Cli.parse_command "board")

let test_help_command _ =
  assert_equal Cli.Help (Cli.parse_command "h");
  assert_equal Cli.Help (Cli.parse_command "help");
  assert_equal Cli.Help (Cli.parse_command "?")

let test_quit_command _ =
  assert_equal Cli.Quit (Cli.parse_command "q");
  assert_equal Cli.Quit (Cli.parse_command "quit")

let test_invalid_command _ =
  match Cli.parse_command "hello" with
  | Cli.Invalid _ -> ()
  | _ -> assert_failure "Expected invalid command"

let test_invalid_coordinates _ =
  match Cli.parse_command "abc 5" with
  | Cli.Invalid _ -> ()
  | _ -> assert_failure "Expected invalid coordinates"

let test_command_whitespace _ =
  assert_equal (Cli.Attack (3, 5)) (Cli.parse_command "  3   5  ")

(** Tests for manual ship placement. *)

let test_parse_direction _ =
  assert_equal (Some Board.North) (Cli.parse_direction "n");
  assert_equal (Some Board.East) (Cli.parse_direction "e");
  assert_equal (Some Board.South) (Cli.parse_direction "s");
  assert_equal (Some Board.West) (Cli.parse_direction "w")

let test_parse_invalid_direction _ =
  assert_equal None (Cli.parse_direction "x");
  assert_equal None (Cli.parse_direction "")

let test_ship_cells_east _ =
  let expected = [ (2, 3); (2, 4); (2, 5); (2, 6) ] in
  assert_equal expected (Cli.ship_cells 2 3 Board.East 4)

let test_ship_cells_west _ =
  let expected = [ (5, 3); (5, 2); (5, 1) ] in
  assert_equal expected (Cli.ship_cells 5 3 Board.West 3)

let test_ship_cells_north _ =
  let expected = [ (2, 5); (1, 5); (0, 5) ] in
  assert_equal expected (Cli.ship_cells 2 5 Board.North 3)

let test_ship_cells_south _ =
  let expected = [ (2, 3); (3, 3); (4, 3) ] in
  assert_equal expected (Cli.ship_cells 2 3 Board.South 3)

let test_valid_placement _ =
  let occupied = Array.make_matrix 10 10 false in
  let cells = Cli.ship_cells 2 3 Board.East 4 in
  assert_bool "Ship should fit on board" (Cli.valid_placement occupied 10 cells)

let test_out_of_bounds_placement _ =
  let occupied = Array.make_matrix 10 10 false in
  let cells = Cli.ship_cells 3 8 Board.East 4 in
  assert_bool "Ship should not extend beyond board"
    (not (Cli.valid_placement occupied 10 cells))

let test_negative_coordinates _ =
  let occupied = Array.make_matrix 10 10 false in
  let cells = Cli.ship_cells 0 0 Board.West 3 in
  assert_bool "Ship should not have negative coordinates"
    (not (Cli.valid_placement occupied 10 cells))

let test_overlapping_placement _ =
  let occupied = Array.make_matrix 10 10 false in
  occupied.(3).(3) <- true;
  let cells = Cli.ship_cells 3 2 Board.East 4 in
  assert_bool "Ships should not overlap"
    (not (Cli.valid_placement occupied 10 cells))

let test_adjacent_placement _ =
  let occupied = Array.make_matrix 10 10 false in
  occupied.(3).(3) <- true;
  let cells = Cli.ship_cells 3 4 Board.East 3 in
  assert_bool "Adjacent ships should be allowed"
    (Cli.valid_placement occupied 10 cells)

let cli_tests =
  "CLI"
  >::: [
         "attack command" >:: test_attack_command;
         "board command" >:: test_board_command;
         "help command" >:: test_help_command;
         "quit command" >:: test_quit_command;
         "invalid command" >:: test_invalid_command;
         "invalid coordinates" >:: test_invalid_coordinates;
         "command whitespace" >:: test_command_whitespace;
         "parse direction" >:: test_parse_direction;
         "invalid direction" >:: test_parse_invalid_direction;
         "ship cells east" >:: test_ship_cells_east;
         "ship cells west" >:: test_ship_cells_west;
         "ship cells north" >:: test_ship_cells_north;
         "ship cells south" >:: test_ship_cells_south;
         "valid placement" >:: test_valid_placement;
         "out of bounds" >:: test_out_of_bounds_placement;
         "negative coordinates" >:: test_negative_coordinates;
         "overlapping placement" >:: test_overlapping_placement;
         "adjacent placement" >:: test_adjacent_placement;
       ]

let () = run_test_tt_main ("Battleship Tests" >::: [ display_tests; cli_tests ])

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
         "opponent board hides unhit ship"
         >:: test_opponent_board_hides_unhit_ship;
         "hit ship" >:: test_hit_ship;
         "miss" >:: test_miss;
         "revealed empty cell" >:: test_revealed_empty_cell;
         "sunk regular ship" >:: test_sunk_regular_ship;
         "sunk golden ship" >:: test_sunk_golden_ship;
         "collected treasure" >:: test_collected_treasure;
       ]

let () = run_test_tt_main display_tests

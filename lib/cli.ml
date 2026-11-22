(* Parses and validates command-line arguments for the number of players,
   machine difficulty, and takes input keys from user to do game updates for
   each turn. *)

open BatOptParse

type settings = {
  players : int;
  difficulty : string;
}

(** [parse_args ()] reads and validates the command-line options [--players] and
    [--difficulty], returning the game settings. *)
let parse_args () =
  let parser =
    OptParser.make ~usage:"%prog --players NUMBER --difficulty LEVEL"
      ~description:"Play Battleship with power-ups and golden ships." ()
  in
  let players = StdOpt.str_option ~metavar:"NUMBER" () in
  let difficulty = StdOpt.str_option ~metavar:"LEVEL" () in

  OptParser.add parser ~long_name:"players"
    ~help:"number of human players: 1 or 2" players;
  OptParser.add parser ~long_name:"difficulty"
    ~help:"machine difficulty: easy, medium, or hard; ignored with 2 players"
    difficulty;

  let fail message =
    OptParser.error parser message;
    exit 1
  in

  let positional = OptParser.parse_argv parser in
  if positional <> [] then fail "Use --players NUMBER --difficulty LEVEL";

  let players_value =
    match Opt.opt players with
    | Some "1" -> 1
    | Some "2" -> 2
    | Some _ -> fail "--players must be 1 or 2"
    | None -> fail "Missing --players NUMBER"
  in

  let difficulty_value =
    match Opt.opt difficulty with
    | Some "easy" -> "easy"
    | Some "medium" -> "medium"
    | Some "hard" -> "hard"
    | Some _ -> fail "--difficulty must be easy, medium, or hard"
    | None -> fail "Missing --difficulty LEVEL"
  in

  { players = players_value; difficulty = difficulty_value }

(** Represents the possible commands entered by a player. *)
type command =
  | Attack of int * int
  | Show_board
  | Help
  | Quit
  | Invalid of string

(** [parse_command input] interprets the player's input and returns the
    corresponding command. Coordinates are entered as column then row. *)
let parse_command input =
  let words =
    String.lowercase_ascii (String.trim input)
    |> String.split_on_char ' '
    |> List.filter (fun word -> word <> "")
  in

  let parse_coordinates col row =
    match (int_of_string_opt col, int_of_string_opt row) with
    | Some x, Some y -> Attack (x, y)
    | _ -> Invalid "Coordinates must be integers (e.g., 3 5)."
  in

  match words with
  | [ "q" ] | [ "quit" ] -> Quit
  | [ "h" ] | [ "help" ] | [ "?" ] -> Help
  | [ "b" ] | [ "board" ] -> Show_board
  | [ col; row ] -> parse_coordinates col row
  | [ "s"; col; row ] | [ "strike"; col; row ] -> parse_coordinates col row
  | _ -> Invalid "Invalid command. Enter h for help."

(** [print_help ()] displays the available commands during gameplay. *)
let print_help () =
  print_endline "\nBattleship Commands:";
  print_endline "  COLUMN ROW    Attack a coordinate (e.g., 3 5)";
  print_endline "  s COLUMN ROW  Attack a coordinate (e.g., s 3 5)";
  print_endline "  b             Display the boards";
  print_endline "  h             Display available commands";
  print_endline "  q             Quit the game"

(** [read_command ()] reads the player's input and returns the corresponding
    command. If the input stream closes, the game quits. *)
let read_command () =
  print_string "\nEnter COLUMN ROW, or h for help: ";
  flush stdout;
  match read_line () with
  | input -> parse_command input
  | exception End_of_file -> Quit

(** [parse_direction input] converts a direction string into a board direction.
*)
let parse_direction input =
  match String.lowercase_ascii input with
  | "n" | "north" -> Some Board.North
  | "e" | "east" -> Some Board.East
  | "s" | "south" -> Some Board.South
  | "w" | "west" -> Some Board.West
  | _ -> None

(** [ship_cells col row direction length] returns the coordinates occupied by a
    ship, starting at [(col, row)]. *)
let ship_cells col row direction length =
  List.init length (fun i ->
      match direction with
      | Board.North -> (col, row - i)
      | Board.East -> (col + i, row)
      | Board.South -> (col, row + i)
      | Board.West -> (col - i, row))

(** [valid_placement occupied size cells] checks whether all ship cells are
    within the board and do not overlap previously placed ships. *)
let valid_placement occupied size cells =
  List.for_all
    (fun (col, row) ->
      col >= 0 && col < size && row >= 0 && row < size
      && not occupied.(row).(col))
    cells

(** [read_ship_placements size lengths] prompts the player to place each ship
    manually. Invalid placements are rejected and retried. Returns [None] if the
    player quits. *)
let read_ship_placements size lengths =
  let occupied = Array.make_matrix size size false in

  print_endline "\nManual ship placement";
  Printf.printf "Board coordinates range from 0 to %d.\n" (size - 1);
  print_endline "Enter: COLUMN ROW DIRECTION";
  print_endline "Directions: n (north), e (east), s (south), w (west)";
  print_endline
    "Example: 3 5 e. This will place a ship starting at (3,5) facing East";
  print_endline "Enter q to quit.";

  let rec place remaining placed =
    match remaining with
    | [] ->
        print_endline "\nAll ships placed successfully!";
        Some (List.rev placed)
    | length :: rest -> (
        Printf.printf "\nPlace ship of length %d: " length;
        flush stdout;

        let input =
          match read_line () with
          | line -> Some line
          | exception End_of_file -> None
        in

        match input with
        | None -> None
        | Some input -> (
            let words =
              String.lowercase_ascii (String.trim input)
              |> String.split_on_char ' '
              |> List.filter (fun word -> word <> "")
            in

            match words with
            | [ "q" ] | [ "quit" ] -> None
            | [ col; row; direction ] -> (
                match
                  ( int_of_string_opt col,
                    int_of_string_opt row,
                    parse_direction direction )
                with
                | Some x, Some y, Some d ->
                    let cells = ship_cells x y d length in
                    if valid_placement occupied size cells then (
                      List.iter
                        (fun (col, row) -> occupied.(row).(col) <- true)
                        cells;

                      let ship = Board.Ship.create (y, x) d length in

                      print_endline "Ship placed successfully!";
                      place rest (ship :: placed))
                    else (
                      print_endline
                        "Invalid placement: ship is out of bounds or overlaps \
                         another ship.";
                      place remaining placed)
                | _ ->
                    print_endline
                      "Invalid input. Use COLUMN ROW DIRECTION (e.g., 3 5 e).";
                    place remaining placed)
            | _ ->
                print_endline
                  "Invalid input. Use COLUMN ROW DIRECTION (e.g., 3 5 e).";
                place remaining placed))
  in

  place lengths []



(*
(* COMMENT (by Carissa): While testing the manual ship placement after implementing
 game.ml, I added onto your read_ship_placements function to display the grid 
 after each ship is placed since made easier to test. I pasted the updated functions below 

 I was wondering if we could maybe incorporate this into Cli since could  
possibly also make the game more user-friendly???? nw at all not *)


(* Show ship placements after each ship is added. This is temporary until
   ship placement and its display are fully handled by Cli/Display. *)
let show_placement_grid occupied =
  let size = Array.length occupied in
  print_endline "\nYour ship placements:";
  Printf.printf "    ";
  for col = 0 to size - 1 do
    Printf.printf "%3d" col
  done;
  print_newline ();
  for row = 0 to size - 1 do
    Printf.printf "%3d " row;
    for col = 0 to size - 1 do
      Printf.printf "%3s" (if occupied.(row).(col) then "S" else ".")
    done;
    print_newline ()
  done

(* Temporarily handles placement prompts here to display the grid after each
   ship. Uses the existing Cli helpers for directions and placement checks. *)
let read_ship_placements_with_preview size lengths =
  let occupied = Array.make_matrix size size false in
  print_endline "\nManual ship placement";
  Printf.printf "Board coordinates range from 0 to %d.\n" (size - 1);
  show_placement_grid occupied;
  let rec place remaining placed =
    match remaining with
    | [] ->
        print_endline "\nAll ships placed successfully!";
        print_endline "\nPress Enter to start the game!";
        flush stdout;
        (match read_line () with
        | exception End_of_file -> None
    | _ -> Some (List.rev placed))
        | length :: rest ->
        print_endline "Enter: COLUMN ROW DIRECTION (e.g., 3 5 e)";
        print_endline "Directions: n (north), e (east), s (south), w (west)";
        print_endline "Enter q to quit.";
        Printf.printf "\nPlace ship of length %d: " length;
        flush stdout;
        (match read_line () with
        | exception End_of_file -> None
        | input ->
            let words =
              String.lowercase_ascii (String.trim input)
              |> String.split_on_char ' '
              |> List.filter (fun word -> word <> "")
            in
            match words with
            | [ "q" ] | [ "quit" ] -> None
            | [ col; row; direction ] -> (
                match
                  ( int_of_string_opt col,
                    int_of_string_opt row,
                    Cli.parse_direction direction )
                with
                | Some x, Some y, Some d ->
                    let cells = Cli.ship_cells x y d length in
                    if Cli.valid_placement occupied size cells then (
                      List.iter
                        (fun (col, row) -> occupied.(row).(col) <- true)
                        cells;
                      (* Match the coordinate convention used by Cli. *)
                      let ship = Board.Ship.create (y, x) d length in
                      print_endline "Ship placed successfully!";
                      show_placement_grid occupied;
                      place rest (ship :: placed))
                    else (
                      print_endline
                        "Invalid placement: ship is out of bounds or overlaps another ship.";
                      place remaining placed)
                | _ ->
                    print_endline "Invalid input. Use COLUMN ROW DIRECTION (e.g., 3 5 e).";
                    place remaining placed)
            | _ ->
                print_endline "Invalid input. Use COLUMN ROW DIRECTION (e.g., 3 5 e).";
                place remaining placed)
  in
  place lengths []
*)
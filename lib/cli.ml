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

(** [ship_cells row col direction length] returns the coordinates occupied by a
    ship, starting at [(row, col)]. *)
let ship_cells row col direction length =
  List.init length (fun i ->
      match direction with
      | Board.North -> (row - i, col)
      | Board.East -> (row, col + i)
      | Board.South -> (row + i, col)
      | Board.West -> (row, col - i))

(** [valid_placement occupied size cells] checks whether all ship cells are
    within the board and do not overlap previously placed ships. *)
let valid_placement occupied size cells =
  List.for_all
    (fun (row, col) ->
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
  print_endline "Enter: ROW COLUMN DIRECTION";
  print_endline "Directions: n (north), e (east), s (south), w (west)";
  print_endline
    "Example: 3 5 e. This places a ship at row 3, column 5, facing East.";
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
            | [ row; col; direction ] -> (
                match
                  ( int_of_string_opt row,
                    int_of_string_opt col,
                    parse_direction direction )
                with
                | Some row, Some col, Some d ->
                    let cells = ship_cells row col d length in
                    if valid_placement occupied size cells then (
                      List.iter
                        (fun (row, col) -> occupied.(row).(col) <- true)
                        cells;

                      let ship = Board.Ship.create (row, col) d length in

                      print_endline "Ship placed successfully!";
                      place rest (ship :: placed))
                    else (
                      print_endline
                        "Invalid placement: ship is out of bounds or overlaps \
                         another ship.";
                      place remaining placed)
                | _ ->
                    print_endline
                      "Invalid input. Use ROW COLUMN DIRECTION (e.g., 3 5 e).";
                    place remaining placed)
            | _ ->
                print_endline
                  "Invalid input. Use ROW COLUMN DIRECTION (e.g., 3 5 e).";
                place remaining placed))
  in

  place lengths []

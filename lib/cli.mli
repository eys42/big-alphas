(** Command-line configuration for Battleship. *)

type settings = {
  players : int;  (** The number of human players, either [1] or [2]. *)
  difficulty : string;
      (** The machine difficulty, either ["easy"], ["medium"], or ["hard"].
          Ignored when [players] is [2]. *)
}

val parse_args : unit -> settings
(** [parse_args ()] reads and validates the command-line options [--players] and
    [--difficulty]. Returns the game settings. Exits with an error if the
    arguments are missing or invalid. *)

(** Represents the possible commands entered by a player during the game. *)
type command =
  | Attack of int * int  (** Attack the given column and row. *)
  | Show_board  (** Display the current boards. *)
  | Help  (** Display the available commands. *)
  | Quit  (** Exit the game. *)
  | Invalid of string
      (** An invalid command with an explanatory error message. *)

val parse_command : string -> command
(** [parse_command input] converts a string into a game command. Supports
    coordinate attacks, board display, help, and quitting. Does not modify the
    game state. *)

val print_help : unit -> unit
(** [print_help ()] displays the available commands and their usage. *)

val read_command : unit -> command
(** [read_command ()] reads a line of input from the player and returns the
    corresponding command. Returns [Quit] if the input stream ends. *)

val parse_direction : string -> Board.direction option
(** [parse_direction input] converts a direction string into a board direction.
    Returns [None] if the direction is invalid. *)

val ship_cells : int -> int -> Board.direction -> int -> (int * int) list
(** [ship_cells row col direction length] returns the (row, column) coordinates
    occupied by a ship with the given starting position, direction, and length. *)

val valid_placement : bool array array -> int -> (int * int) list -> bool
(** [valid_placement occupied size cells] checks whether a ship fits within the
    board without overlapping previously placed ships. *)

val read_ship_placements : int -> int list -> Board.Ship.s list option
(** [read_ship_placements size lengths] interactively places ships of the
    specified lengths. Returns the ships or [None] if the player quits. *)

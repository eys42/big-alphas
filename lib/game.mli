(** Game rules and turn management for single-player Battleship.

    NOTE (by carissa): Power-ups, golden-ship special rules, and multiplayer r
    not active yet; Saving this for post MS2.... Also waiting for CLI
    implementation for initiating human-made ship placement stuff*)

type player =
  | Human
  | Computer

type t
(** The state of one game (includes the two boards, the computer strategy, the
    current turn, and the winner if any). *)

val create : ?seed:int -> Strategy.difficulty -> t
(** [create ?seed difficulty] initializes a single-player game with randomly
    placed ships, with the human playing first. [difficulty] determines the
    computer's ship placement and attack strategy. The default board is 10x10
    with ships of lengths 5, 4, 3, 3, and 2. [seed] allows reproducible
    initialization.

    Treasure chests are present, but power-ups are not yet implemented.

    NOTE (by carissa) Human ships r currently placed automatically. Manual ship
    placement will be added once the CLI supports it. *)

val human_board : t -> Board.t
(** [human_board game] returns the human's board*)

val opponent_board : t -> Board.t
(** [opponent_board game] returns the computer's board (for now in single player
    mode)*)

val current_player : t -> player
(** [current_player game] returns whose turn it is. After the game ends, it
    stays on the winning player. *)

val winner : t -> player option
(** [winner game] is [Some Human] if the human has sunk all computer ships,
    [Some Computer] if the computer has sunk all human ships, and [None] while
    neither player has won. *)

val human_strike : t -> int -> int -> Board.cell
(** [human_strike game x y] attacks the computer at [(x, y)] and returns the
    result. It also updates the turn or winner accordingly.

    Raises [Invalid_argument] for invalid/repeated attacks, wrong turns, or
    finished games. *)

val machine_strike : t -> (int * int) * Board.cell
(** [machine_strike game] performs a computer attack and returns its coordinates
    and result. Updates the turn or winner accordingly.

    Raises [Invalid_argument] if it is not the computer's turn or the game has
    ended. *)

val run : Cli.settings -> unit
(** [run settings] starts the interactive single-player game using the given
    settings. NOTE: Two-player gameplay hasn't yet been implemented. *)

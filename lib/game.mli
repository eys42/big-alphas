(** Game rules and turn management for single-player Battleship.

    NOTE (by carissa): Nuke power-ups are active, but radar, golden-ship special
    rules, and multiplayer r not active yet. *)

type player =
  | Human
  | Computer

type t
(** The state of one game (includes the two boards, the computer strategy, the
    current turn, and the winner if any). *)

val create :
  ?seed:int -> ?human_ships:Board.Ship.s list -> Strategy.difficulty -> t
(** [create ?seed ?human_ships difficulty] initializes a single-player game,
    with the human playing first. [difficulty] determines the computer's ship
    placement and attack strategy. The default board is 10x10 with ships of
    lengths 5, 4, 3, 3, and 2. [seed] allows reproducible initialization.

    Treasure chests are present. Striking a nuke chest gives that player a nuke
    (see {!nukes}); radar chests do nothing yet.

    NOTE (by carissa) Human ships r automatically placed unless [human_ships] is
    supplied. [run] now gets manual placements from the CLI. *)

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
(** [human_strike game row col] attacks the computer at [(row, col)] and returns
    the result. It also updates the turn or winner accordingly.

    Raises [Invalid_argument] for invalid/repeated attacks, wrong turns, or
    finished games. *)

val machine_strike : t -> (int * int) * Board.cell
(** [machine_strike game] performs a computer attack and returns its coordinates
    and result. Updates the turn or winner accordingly.

    Raises [Invalid_argument] if it is not the computer's turn or the game has
    ended. *)

val nukes : t -> player -> int
(** [nukes game player] is how many nukes [player] holds. Both players start
    with none. *)

val human_nuke : t -> int -> int -> ((int * int) * Board.cell) list
(** [human_nuke game row col] spends one of the human's nukes on the 3x3 area
    centered on [(row, col)] of the computer's board, as {!Board.nuke} does, and
    returns what it struck. The blast takes the human's turn. Any nukes found in
    the area are added to the human's.

    Raises [Invalid_argument], without changing the game, if the human has no
    nuke, [(row, col)] is out of bounds, every cell of the area has already been
    struck, it is not the human's turn, or the game has ended. *)

(** What the computer did on its turn. *)
type machine_move =
  | Shot of (int * int) * Board.cell
      (** A single shot at the given coordinates, and its result. *)
  | Blast of (int * int) * ((int * int) * Board.cell) list
      (** A nuke centered at the given coordinates, and what it struck. *)

val machine_turn : t -> machine_move
(** [machine_turn game] plays the computer's turn. If it holds a nuke and its
    strategy picks an area for it (see {!Strategy.choose_nuke}), it uses the
    nuke on the human's board; otherwise it fires one shot as {!machine_strike}
    does. Updates the turn or winner accordingly.

    Raises [Invalid_argument] if it is not the computer's turn or the game has
    ended. *)

val run : Cli.settings -> unit
(** [run settings] starts the interactive single-player game using the given
    settings and asks the human to place ships through the CLI. NOTE: Two-player
    gameplay hasn't yet been implemented. *)

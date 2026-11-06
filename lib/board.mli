(** Representation of and operations on the game world.*)

type direction =
  | North
  | East
  | South
  | West

module Ship : sig
  type s

  val create : int * int -> direction -> int -> s
  (** [create anchor direction length] creates a new ship with the given anchor,
      orientation, and length. *)
end

type t

type powerup =
  | Nuke
  | Radar
  | Collected

type cell =
  | Empty (* empty cell that has not been revealed *)
  | Hidden (* cell that has not been revealed to the opponent *)
  | Revealed of Ship.s option
    (* cell revealed by radar, carries [None] if empty and [Some ship] if
       occupied *)
  | Occupied of Ship.s * bool
    (* cell occupied by ship, not revealed or hit. boolean indicates whether it
       has been hit *)
  | Sunk of Ship.s (* cell occupied by a sunk ship *)
  | Miss (* cell that has been fired upon but is empty *)
  | Treasure_chest of powerup * bool
(* cell containing a powerup. boolean indicates whether it has been revealed *)

val dim : t -> int
(** [dim board] returns the dimension of the [board]. *)

val init : int -> Ship.s list -> int -> t
(** [init dim ships seed] creates a new [dim]x[dim] game board with the
    specified [ships] and random [seed]. Places one treasure chest containing a
    nuke and one containing a radar. Requires [dim] > 0 and that ships are in
    bounds. NOTE: treasure chest placement is only deterministic with the same
    set of ships. *)

val cell_at_own : t -> int -> int -> cell
(** [cell_at_own board x y] returns the cell at position [(x, y)] on the
    player's own board. Requires [0 <= x < dim board] and [0 <= y < dim board].
*)

val cell_at_opponent : t -> int -> int -> cell
(** [cell_at_opponent board x y] returns the cell at position [(x, y)] on the
    opponent's board, which can be one of
    [Hidden; Hit; Miss; Sunk; Treasure_chest]. Requires [0 <= x < dim board] and
    [0 <= y < dim board]. *)

val strike : t -> int -> int -> cell
(** [strike board x y] strikes the cell at position [(x, y)] on the opponent's
    board. Returns [Miss] if it's empty, [Hit] if the cell contains a ship,
    [Sunk ship] if the ship is sunk, and [Treasure_chest powerup] if the cell
    contains a treasure chest. Requires [0 <= x < dim board] and
    [0 <= y < dim board]. *)

val reveal : t -> int -> int -> cell
(** [reveal board x y] reveals the cell at position [(x, y)] on the opponent's
    board. Returns [Revealed None] if it's empty, [Revealed (Some ship)] if the
    cell contains a ship, and [Treasure_chest powerup] if the cell contains a
    treasure chest. Requires that the cell is not already revealed. Requires
    [0 <= x < dim board] and [0 <= y < dim board]. *)

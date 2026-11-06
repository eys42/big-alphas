(** Representation of and operations on the game world.*)

type direction =
  | North
  | East
  | South
  | West

type ship = {
  anchor : int * int;
  orientation : direction;
  length : int;
}

type powerup =
  | Nuke
  | Radar

type t

type cell =
  | Empty (* empty cell that has not been revealed *)
  | Revealed of ship option
    (* cell revealed by radar, carries [None] if empty and [Some ship] if
       occupied *)
  | Occupied of ship (* cell occupied by ship, not revealed or hit *)
  | Sunk of ship (* cell occupied by a sunk ship*)
  | Hit of ship (* cell occupied by a ship that has been hit *)
  | Miss (* cell that has been fired upon but is empty *)
  | Treasure_chest of powerup (* cell containing an uncollected powerup *)

val init : int -> ship list -> int -> t
(** [init dim ships seed] creates a new [dim]x[dim] game board with the
    specified [ships] and random [seed]. Places one treasure chest containing a
    nuke and one containing a radar. Requires [dim] > 0.*)

val cell_at : t -> int -> int -> cell
(** [cell_at board x y] returns the cell at position (x, y) on the board. Used
    to render the player's own board.*)

val dim : t -> int
(** [dim board] returns the dimension of the [board]. *)

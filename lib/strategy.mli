(** Machine opponents: placing the machine's ships and choosing where it fires.

    A strategy only looks at the opponent's board through
    {!Board.cell_at_opponent}, so it sees exactly what a human player would.
    Power-ups and golden ships are not handled yet. *)

(** How strong the machine opponent is. *)
type difficulty =
  | Easy
  | Medium
  | Hard

val difficulty_of_string : string -> difficulty option
(** [difficulty_of_string s] is [Some d] if [s] is ["easy"], ["medium"], or
    ["hard"], or [None] otherwise. *)

val place_ships :
  difficulty -> Random.State.t -> int -> int list -> Board.Ship.s list
(** [place_ships d rng dim lengths] is one ship of each length in [lengths], in
    the same order, placed at random on a [dim]x[dim] board, advancing [rng].
    Every ship is entirely in bounds and no two ships share a cell. For [Medium]
    and [Hard], no two ships touch either, not even at a corner. Raises
    [Invalid_argument] if a length is not positive, or if the ships cannot be
    placed by these rules after many attempts (for example, if they are too long
    or too many for the board). *)

type t
(** A machine player: its difficulty and the random state it uses. *)

val create : difficulty -> Random.State.t -> t
(** [create d rng] is a new machine player of difficulty [d] that makes its
    random choices with [rng]. *)

val difficulty : t -> difficulty
(** [difficulty m] is the difficulty of [m]. *)

val strikeable : Board.t -> int -> int -> bool
(** [strikeable board x y] is whether [(x, y)] is in bounds and has not been
    struck yet, judged from what the opponent can see: a hidden cell, a cell
    revealed by radar, or a revealed treasure chest that has not been collected.
*)

val choose_shot : t -> Board.t -> int * int
(** [choose_shot m board] is the cell [(x, y)] that [m] fires at on the
    opponent's [board], advancing [m]'s random state. It is always strikeable,
    so a cell is never chosen twice.
    - [Easy] fires at a random strikeable cell, with no memory.
    - [Medium] fires at a random strikeable cell until it sees a hit on a ship
      that has not sunk. While such a hit has a strikeable orthogonal neighbor,
      it fires at one of those neighbors at random.
    - [Hard] currently fires exactly like [Medium]. Its own strategy
      (checkerboard search and directed follow-up) is not implemented yet.

    Raises [Invalid_argument] if no cell of [board] is strikeable. *)

(** Functions for displaying Battleship boards in the terminal. *)

val print_board : Board.t -> bool -> unit
(** [print_board board own] prints [board] with row and column coordinates. If
    [own] is [true], displays the player's own board, including unhit ships and
    treasure chests. Otherwise, displays only information visible to the
    opponent. Does not modify [board]. *)

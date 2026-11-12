(** Command-line configuration for Battleship. *)

type settings = {
  players : int;  (** The number of human players, either [1] or [2]. *)
  difficulty : string;
      (** The machine difficulty, either ["easy"], ["medium"], or ["hard"].
          Ignored when [players] is [2]. *)
}

(** The validated settings used to start a game. *)
val parse_args : unit -> settings
(** [parse_args ()] reads the number of human players and machine difficulty
    from the command-line options [--players] and [--difficulty]. Returns the
    validated settings. Prints an error message and exits with an unsuccessful
    status if either option is missing or invalid, or if unexpected positional
    arguments are supplied. *)

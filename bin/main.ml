(* Runs the game by calling Game.run

   NOTE (by carissa): currents implements basic single-player Battleship
   gameplay (turn management, attacks, and win/loss detection)

   Power-ups, special features, and multiplayer stuff not yet implemented. *)

open Big_alphas

let () = Game.run (Cli.parse_args ())

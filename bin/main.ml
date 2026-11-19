(* Game.run is the module that will start the game later on, but just left here
   as a placeholder for now - Stephanie *)

open Big_alphas

let main () =
  let _settings = Cli.parse_args () in

  (* Temporary placeholder board display until Game.run is implemented. *)
  let ships =
    [
      Board.Ship.create (0, 0) Board.East 2;
      Board.Ship.create (2, 2) Board.South 3;
    ]
  in
  let board = Board.init 5 ships 42 in

  print_endline "Your board:";
  Display.print_board board true;

  print_endline "\nOpponent's board:";
  Display.print_board board false

let () = main ()

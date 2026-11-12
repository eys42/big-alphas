(* Displays Battleship boards in the terminal to play the game *)

(** [print_board board own] prints [board] with row and column coordinates. If
    [own] is [true], displays the player's own board, including unhit ships and
    treasure chests. Otherwise, displays only information visible to the
    opponent. Does not modify [board]. *)
let print_board board own =
  let size = Board.dim board in
  Printf.printf "    ";
  for x = 0 to size - 1 do
    Printf.printf "%3d" x
  done;
  print_newline ();
  for y = 0 to size - 1 do
    Printf.printf "%3d " y;
    for x = 0 to size - 1 do
      let cell =
        if own then Board.cell_at_own board x y
        else Board.cell_at_opponent board x y
      in
      let symbol =
        match cell with
        | Board.Empty | Board.Hidden -> "."
        | Board.Miss -> "X"
        | Board.Occupied (_, true) -> "*"
        | Board.Occupied (_, false) -> if own then "S" else "."
        | Board.Revealed None -> "~"
        | Board.Revealed (Some _) -> "S"
        | Board.Sunk (_, true) -> "G"
        | Board.Sunk (_, false) -> "#"
        | Board.Treasure_chest (Board.Collected, _) -> "C"
        | Board.Treasure_chest (_, revealed) ->
            if own || revealed then "T" else "."
      in
      Printf.printf "%3s" symbol
    done;
    print_newline ()
  done

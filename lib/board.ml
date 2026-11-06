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

type t = cell array array

(** [roll rng low high] generates a natural number between [low] and [high],
    inclusive, advancing [rng]. *)
let roll rng low high = low + Random.State.int rng (high + 1 - low)

(** [in_bounds board x y] checks if the position [(x, y)] is within the bounds
    of [board]. *)
let in_bounds board x y =
  let dim = Array.length board in
  x >= 0 && x < dim && y >= 0 && y < dim

(** [place_ship board ship] places [ship] on [board]. *)
let rec place_ship board ship =
  let x, y = ship.anchor in
  let rec place_ship_cells x y length =
    match length with
    | 0 -> ()
    | _ -> (
        if not (in_bounds board x y) then invalid_arg "Ship out of bounds";
        match board.(x).(y) with
        | Empty ->
            board.(x).(y) <- Occupied ship;
            let x_next, y_next =
              match ship.orientation with
              | North -> (x - 1, y)
              | East -> (x, y + 1)
              | South -> (x + 1, y)
              | West -> (x, y - 1)
            in
            place_ship_cells x_next y_next (length - 1)
        | _ -> invalid_arg "Ship placement overlaps with another ship")
  in
  place_ship_cells x y ship.length

(** [place_treasure_chest board rng powerup] places a treasure chest with the
    specified [powerup] on the [board], using [rng].*)
let place_treasure_chest board rng powerup =
  let dim = Array.length board in
  let rec find_empty_cell () =
    let x = roll rng 0 (dim - 1) in
    let y = roll rng 0 (dim - 1) in
    match board.(x).(y) with
    | Empty -> (x, y)
    | _ -> find_empty_cell ()
  in
  let x, y = find_empty_cell () in
  board.(x).(y) <- Treasure_chest powerup

let init (dim : int) (ships : ship list) (seed : int) : t =
  if dim < 1 then invalid_arg "Dimension must be positive";
  let covered_area =
    List.fold_left (fun area ship -> area + ship.length) 0 ships
  in
  if covered_area > dim * dim / 3 || (dim * dim) - covered_area < 2 then
    invalid_arg "Ships cover more than 1/3 of the board";
  let board = Array.make_matrix dim dim Empty in
  List.iter (place_ship board) ships;
  let rng = Random.State.make [| seed |] in
  place_treasure_chest board rng Nuke;
  place_treasure_chest board rng Radar;
  board

let cell_at board x y =
  if not (in_bounds board x y) then invalid_arg "Coordinates out of bounds"
  else board.(x).(y)

let dim board = Array.length board

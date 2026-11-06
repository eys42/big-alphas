type direction =
  | North
  | East
  | South
  | West

(* Ship module reveals an abstract [Ship.s] type for representing ships, which
   is created by the [create] function. However, [include Ship] exposes the
   implementation in this file. *)
module Ship = struct
  type s = {
    anchor : int * int;
    orientation : direction;
    length : int;
  }

  let create anchor orientation length =
    if length < 1 then invalid_arg "Ship length must be positive";
    { anchor; orientation; length }
end

include Ship

type powerup =
  | Nuke
  | Radar
  | Collected (* treasure has already been collected *)

type cell =
  | Empty (* empty cell that has not been revealed *)
  | Hidden (* cell that has not been revealed to the opponent *)
  | Revealed of s option
    (* cell revealed by radar, carries [None] if empty and [Some ship] if
       occupied *)
  | Occupied of s * bool
    (* cell occupied by ship, not revealed or hit. boolean indicates whether it
       has been hit *)
  | Sunk of s (* cell occupied by a sunk ship *)
  | Miss (* cell that has been fired upon but is empty *)
  | Treasure_chest of powerup * bool
(* cell containing a powerup. boolean indicates whether it has been revealed *)

type t = cell array array
(** RI: The board is a square matrix of cells. Cells contained internally must
    be one of [Empty; Revealed; Occupied; Sunk; Miss; Treasure_chest]. Variant
    [Hidden] is only used for displaying the board to the opponent. *)

(** [roll rng low high] generates a natural number between [low] and [high],
    inclusive, advancing [rng]. *)
let roll rng low high = low + Random.State.int rng (high + 1 - low)

let dim = Array.length

(** [in_bounds board x y] checks if the position [(x, y)] is within the bounds
    of [board]. *)
let in_bounds board x y =
  let dim = dim board in
  x >= 0 && x < dim && y >= 0 && y < dim

(** [traverse_ship board f ship] applies the function [f] to each cell occupied
    by [ship] on [board]. *)
let traverse_ship board f ship =
  let x, y = ship.anchor in
  let rec traverse_ship_rec x y length =
    match length with
    | 0 -> ()
    | _ ->
        if not (in_bounds board x y) then invalid_arg "Ship out of bounds";
        let () = f ship x y in
        let x_next, y_next =
          match ship.orientation with
          | North -> (x - 1, y)
          | East -> (x, y + 1)
          | South -> (x + 1, y)
          | West -> (x, y - 1)
        in
        traverse_ship_rec x_next y_next (length - 1)
  in
  traverse_ship_rec x y ship.length

(** [place_ship board ship] places [ship] on [board]. *)
let rec place_ship board =
  traverse_ship board (fun ship x y ->
      match board.(x).(y) with
      | Empty -> board.(x).(y) <- Occupied (ship, false)
      | _ -> invalid_arg "Ship placement overlaps with another ship")

(** [place_treasure_chest board rng powerup] places a treasure chest with the
    specified [powerup] on the [board], using [rng].*)
let place_treasure_chest board rng powerup =
  let dim = dim board in
  let rec find_empty_cell () =
    let x = roll rng 0 (dim - 1) in
    let y = roll rng 0 (dim - 1) in
    match board.(x).(y) with
    | Empty -> (x, y)
    | _ -> find_empty_cell ()
  in
  let x, y = find_empty_cell () in
  board.(x).(y) <- Treasure_chest (powerup, false)

let init (dim : int) (ships : s list) (seed : int) : t =
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

let cell_at_own board x y =
  if not (in_bounds board x y) then invalid_arg "Coordinates out of bounds"
  else board.(x).(y)

let cell_at_opponent board x y =
  if not (in_bounds board x y) then invalid_arg "Coordinates out of bounds"
  else
    match board.(x).(y) with
    | Empty -> Hidden
    | Revealed _ as cell -> cell
    | Occupied (ship, hit) as cell -> if hit then cell else Hidden
    | Sunk ship -> Sunk ship
    | Miss -> Miss
    | Treasure_chest (p, revealed) -> (
        match p with
        | Collected -> Treasure_chest (Collected, true)
        | _ -> if revealed then Treasure_chest (p, true) else Hidden)
    | Hidden -> failwith "unexpected internal state"

(** [process_hit board ship x y] processes a hit on the given ship at the
    specified coordinates. *)
let process_hit board ship x y =
  board.(x).(y) <- Occupied (ship, true);
  let hits = ref 0 in
  traverse_ship board
    (fun ship x y -> if board.(x).(y) = Occupied (ship, true) then incr hits)
    ship;
  if !hits = ship.length then (
    traverse_ship board (fun ship x y -> board.(x).(y) <- Sunk ship) ship;
    Sunk ship)
  else Occupied (ship, true)

let strike board x y =
  if not (in_bounds board x y) then invalid_arg "Coordinates out of bounds";
  match board.(x).(y) with
  | Empty | Revealed None ->
      board.(x).(y) <- Miss;
      Miss
  | Occupied (ship, false) | Revealed (Some ship) ->
      board.(x).(y) <- Occupied (ship, true);
      process_hit board ship x y
  | Treasure_chest (p, _) -> (
      match p with
      | Collected -> invalid_arg "Can't hit a collected treasure chest"
      | _ ->
          board.(x).(y) <- Treasure_chest (Collected, true);
          Treasure_chest (p, true))
  | Occupied (_, true) | Sunk _ | Miss ->
      invalid_arg "Can't hit the same cell twice"
  | Hidden -> failwith "unexpected internal state"

let reveal board x y =
  if not (in_bounds board x y) then invalid_arg "Coordinates out of bounds";
  match board.(x).(y) with
  | Empty ->
      board.(x).(y) <- Revealed None;
      Revealed None
  | Occupied (ship, _) ->
      board.(x).(y) <- Revealed (Some ship);
      Revealed (Some ship)
  | Treasure_chest (p, false) ->
      board.(x).(y) <- Treasure_chest (p, true);
      Treasure_chest (p, true)
  | _ -> invalid_arg "Can't reveal a cell that has already been revealed"

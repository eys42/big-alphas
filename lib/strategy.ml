(** strategy.ml implements the machine opponent's ship placement and firing
    choices. It reads the opponent's board only through
    [Board.cell_at_opponent]. *)

type difficulty =
  | Easy
  | Medium
  | Hard

(** [difficulty_of_string s] is the difficulty named by [s], if any. *)
let difficulty_of_string = function
  | "easy" -> Some Easy
  | "medium" -> Some Medium
  | "hard" -> Some Hard
  | _ -> None

(** [pick rng xs] is a random element of the nonempty list [xs], advancing
    [rng]. Requires: [xs <> []]. *)
let pick rng xs = List.nth xs (Random.State.int rng (List.length xs))

(** [all_cells dim] is every [(x, y)] of a [dim]x[dim] board, in row-major
    order. *)
let all_cells dim =
  List.concat (List.init dim (fun x -> List.init dim (fun y -> (x, y))))

(** [step (x, y) d] is the cell one step from [(x, y)] in direction [d], using
    the same directions as [Board]: North decreases [x] and East increases [y].
*)
let step (x, y) = function
  | Board.North -> (x - 1, y)
  | Board.East -> (x, y + 1)
  | Board.South -> (x + 1, y)
  | Board.West -> (x, y - 1)

(** [ship_cells anchor d length] is the [length] cells covered by a ship
    anchored at [anchor] and extending in direction [d], as in [Board]. *)
let ship_cells anchor d length =
  let rec go cell n = if n = 0 then [] else cell :: go (step cell d) (n - 1) in
  go anchor length

(** [in_bounds dim (x, y)] is whether [(x, y)] lies on a [dim]x[dim] board. *)
let in_bounds dim (x, y) = x >= 0 && x < dim && y >= 0 && y < dim

(** [around (x, y)] is [(x, y)] and its eight neighbors, in row-major order.
    They may be out of bounds. *)
let around (x, y) =
  List.concat
    (List.map
       (fun dx -> List.map (fun dy -> (x + dx, y + dy)) [ -1; 0; 1 ])
       [ -1; 0; 1 ])

(** [fits ~spaced dim taken cells] is whether [cells] are all in bounds and free
    in [taken], and, if [spaced], also not next to any taken cell (diagonals
    included). *)
let fits ~spaced dim taken cells =
  let free (x, y) = not taken.(x).(y) in
  List.for_all (in_bounds dim) cells
  && List.for_all free cells
  && ((not spaced)
     || List.for_all
          (fun c ->
            List.for_all (fun n -> (not (in_bounds dim n)) || free n) (around c))
          cells)

(** [try_place ~spaced rng dim lengths] places one ship of each length in turn,
    each at a random position among all positions that fit, or returns [None] if
    some ship has no position that fits. Each result is
    [(anchor, direction, length)]. *)
let try_place ~spaced rng dim lengths =
  let taken = Array.make_matrix dim dim false in
  let rec go = function
    | [] -> Some []
    | length :: rest -> (
        let options =
          List.concat_map
            (fun anchor ->
              List.filter_map
                (fun d ->
                  if fits ~spaced dim taken (ship_cells anchor d length) then
                    Some (anchor, d)
                  else None)
                [ Board.North; Board.East; Board.South; Board.West ])
            (all_cells dim)
        in
        match options with
        | [] -> None
        | _ ->
            let anchor, d = pick rng options in
            List.iter
              (fun (x, y) -> taken.(x).(y) <- true)
              (ship_cells anchor d length);
            Option.map (fun placed -> (anchor, d, length) :: placed) (go rest))
  in
  go lengths

(** [attempts] is how many times [place_ships] retries from scratch before
    giving up. *)
let attempts = 100

(** [place_ships d rng dim lengths] is a random placement of one ship per
    length, non-overlapping and, for [Medium] and [Hard], non-touching. *)
let place_ships d rng dim lengths =
  if List.exists (fun n -> n < 1) lengths then
    invalid_arg "Strategy.place_ships: ship lengths must be positive";
  let spaced = d <> Easy in
  let rec attempt n =
    if n = 0 then invalid_arg "Strategy.place_ships: the ships do not fit"
    else
      match try_place ~spaced rng dim lengths with
      | Some placed -> placed
      | None -> attempt (n - 1)
  in
  List.map
    (fun (anchor, d, length) -> Board.Ship.create anchor d length)
    (attempt attempts)

(* AF: [difficulty] and [rng] are the player's difficulty and random state. The
   player needs no other memory: Medium's follow-up targets are visible on the
   opponent's board. *)
type t = {
  difficulty : difficulty;
  rng : Random.State.t;
}

(** [create d rng] is a new machine player of difficulty [d] using [rng]. *)
let create d rng = { difficulty = d; rng }

(** [difficulty m] is the difficulty of [m]. *)
let difficulty m = m.difficulty

(** [strikeable board x y] is whether [(x, y)] is in bounds and not yet struck,
    as seen by the opponent. *)
let strikeable board x y =
  in_bounds (Board.dim board) (x, y)
  &&
  match Board.cell_at_opponent board x y with
  | Board.Hidden | Board.Revealed _ -> true
  | Board.Treasure_chest (Board.Collected, _) -> false
  | Board.Treasure_chest ((Board.Nuke | Board.Radar), _) -> true
  | Board.Empty | Board.Occupied _ | Board.Sunk _ | Board.Miss -> false

(** [follow_ups board] is every strikeable orthogonal neighbor of a visible hit
    on a ship that has not sunk, without duplicates. *)
let follow_ups board =
  let dim = Board.dim board in
  List.concat_map
    (fun (x, y) ->
      match Board.cell_at_opponent board x y with
      | Board.Occupied (_, true) ->
          List.filter
            (fun (nx, ny) -> strikeable board nx ny)
            (List.map
               (step (x, y))
               [ Board.North; Board.East; Board.South; Board.West ])
      | _ -> [])
    (all_cells dim)
  |> List.sort_uniq compare

(** [choose_shot m board] is the cell [m] fires at on [board]; see the interface
    for each difficulty's rule. Raises [Invalid_argument] if nothing is
    strikeable. *)
let choose_shot m board =
  let open_cells =
    List.filter
      (fun (x, y) -> strikeable board x y)
      (all_cells (Board.dim board))
  in
  if open_cells = [] then
    invalid_arg "Strategy.choose_shot: every cell has been struck";
  match m.difficulty with
  | Easy -> pick m.rng open_cells
  | Medium | Hard -> (
      match follow_ups board with
      | [] -> pick m.rng open_cells
      | targets -> pick m.rng targets)

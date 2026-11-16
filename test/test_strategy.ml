(** test_strategy.ml checks the machine opponent's ship placement and firing
    choices through the [Strategy] and [Board] interfaces. Each test builds its
    own boards and random states, so tests cannot affect each other. *)

open OUnit2
open Big_alphas

(** [lengths] is a typical fleet: 17 cells, within a 10x10 board's limit of a
    third of its 100 cells. *)
let lengths = [ 5; 4; 3; 3; 2 ]

(** [seeds] are the random seeds used for property checks. *)
let seeds = List.init 50 Fun.id

(** [rng seed] is a fresh random state made from [seed]. *)
let rng seed = Random.State.make [| seed |]

(** [cells dim] is every [(x, y)] of a [dim]x[dim] board. *)
let cells dim =
  List.concat (List.init dim (fun x -> List.init dim (fun y -> (x, y))))

(** [ship_at board (x, y)] is the ship occupying [(x, y)] on [board] as its
    owner sees it, if any. *)
let ship_at board (x, y) =
  match Board.cell_at_own board x y with
  | Board.Occupied (ship, _) -> Some ship
  | _ -> None

(** [occupied board] is every cell of [board] occupied by a ship. *)
let occupied board =
  List.filter (fun p -> ship_at board p <> None) (cells (Board.dim board))

(** [layout d seed] is a 10x10 board holding the ships that [d] places with
    [seed]. [Board.init] rejects ships that overlap or leave the board. *)
let layout d seed =
  Board.init 10 (Strategy.place_ships d (rng seed) 10 lengths) seed

(** [around (x, y)] is [(x, y)] and its eight neighbors. *)
let around (x, y) =
  List.concat_map
    (fun dx -> List.map (fun dy -> (x + dx, y + dy)) [ -1; 0; 1 ])
    [ -1; 0; 1 ]

(** [say seed text] labels a failure message with [seed]. *)
let say seed text = Printf.sprintf "seed %d: %s" seed text

(** [group_sizes board] is how many cells each ship covers, sorted. *)
let group_sizes board =
  let ships = List.filter_map (ship_at board) (occupied board) in
  let distinct = List.sort_uniq compare ships in
  List.sort compare
    (List.map (fun s -> List.length (List.filter (( = ) s) ships)) distinct)

(** Easy placement: every ship fits on the board without overlapping (or
    [Board.init] would raise), with one ship of each requested length. *)
let test_place_easy _ =
  List.iter
    (fun seed ->
      let board = layout Strategy.Easy seed in
      assert_equal ~msg:(say seed "ship sizes")
        ~printer:(fun l -> String.concat " " (List.map string_of_int l))
        (List.sort compare lengths)
        (group_sizes board))
    seeds

(** Medium and Hard placement: additionally, no two ships touch, not even
    diagonally. *)
let test_place_spaced _ =
  List.iter
    (fun d ->
      List.iter
        (fun seed ->
          let board = layout d seed in
          assert_equal ~msg:(say seed "ship sizes")
            (List.sort compare lengths)
            (group_sizes board);
          List.iter
            (fun ((x, y) as p) ->
              let ship = ship_at board p in
              List.iter
                (fun n ->
                  match ship_at board n with
                  | Some other when Some other <> ship ->
                      assert_failure
                        (say seed (Printf.sprintf "ships touch at (%d,%d)" x y))
                  | _ -> ())
                (List.filter
                   (fun (nx, ny) -> nx >= 0 && nx < 10 && ny >= 0 && ny < 10)
                   (around p)))
            (occupied board))
        seeds)
    [ Strategy.Medium; Strategy.Hard ]

(** The same seed gives the same placement; different seeds vary. *)
let test_place_reproducible _ =
  let layout_cells seed = occupied (layout Strategy.Medium seed) in
  assert_equal ~msg:"same seed" (layout_cells 7) (layout_cells 7);
  let distinct = List.sort_uniq compare (List.map layout_cells seeds) in
  assert_bool "placements vary across seeds" (List.length distinct > 25)

(** Impossible requests raise [Invalid_argument]: a nonpositive length, or three
    3-cell ships, which cannot avoid touching on a 3x3 board. Easy may place
    them side by side, filling the board. *)
let test_place_errors _ =
  assert_raises
    (Invalid_argument "Strategy.place_ships: ship lengths must be positive")
    (fun () -> Strategy.place_ships Strategy.Easy (rng 0) 5 [ 2; 0 ]);
  assert_raises (Invalid_argument "Strategy.place_ships: the ships do not fit")
    (fun () -> Strategy.place_ships Strategy.Medium (rng 0) 3 [ 3; 3; 3 ]);
  assert_equal ~printer:string_of_int 3
    (List.length (Strategy.place_ships Strategy.Easy (rng 0) 3 [ 3; 3; 3 ]))

(** [play d seed] plays a machine player of difficulty [d] against a fresh 6x6
    board until every cell has been struck, and returns the number of shots.
    [Board.strike] raises if a cell is struck twice, so finishing shows that no
    shot repeated a cell. *)
let play d seed =
  let board =
    Board.init 6 (Strategy.place_ships d (rng seed) 6 [ 3; 2; 2 ]) seed
  in
  let m = Strategy.create d (rng (seed + 1000)) in
  let rec go shots =
    if shots = 36 then shots
    else
      let x, y = Strategy.choose_shot m board in
      ignore (Board.strike board x y);
      go (shots + 1)
  in
  let shots = go 0 in
  assert_raises
    (Invalid_argument "Strategy.choose_shot: every cell has been struck")
    (fun () -> Strategy.choose_shot m board);
  shots

(** Every difficulty fires at each of the 36 cells exactly once, then has
    nothing left to fire at. *)
let test_full_games _ =
  List.iter
    (fun d ->
      List.iter
        (fun seed ->
          assert_equal ~msg:(say seed "shots") ~printer:string_of_int 36
            (play d seed))
        (List.init 20 Fun.id))
    [ Strategy.Easy; Strategy.Medium; Strategy.Hard ]

(** [one_ship ()] is a 6x6 board whose only ship covers (2,2), (2,3), and (2,4).
*)
let one_ship () = Board.init 6 [ Board.Ship.create (2, 2) Board.East 3 ] 0

(** After a hit, Medium fires at an unstruck orthogonal neighbor of it. *)
let test_medium_follows_hit _ =
  List.iter
    (fun seed ->
      let board = one_ship () in
      ignore (Board.strike board 2 3);
      let m = Strategy.create Strategy.Medium (rng seed) in
      let x, y = Strategy.choose_shot m board in
      assert_bool
        (say seed (Printf.sprintf "(%d,%d) is next to the hit" x y))
        (List.mem (x, y) [ (1, 3); (2, 4); (3, 3); (2, 2) ]))
    seeds

(** Easy has no memory: even with a hit on the board, its shots are not limited
    to that hit's neighbors. *)
let test_easy_ignores_hit _ =
  let board = one_ship () in
  ignore (Board.strike board 2 3);
  let shots =
    List.map
      (fun seed ->
        Strategy.choose_shot (Strategy.create Strategy.Easy (rng seed)) board)
      seeds
  in
  assert_bool "some shot away from the hit"
    (List.exists
       (fun p -> not (List.mem p [ (1, 3); (2, 4); (3, 3); (2, 2) ]))
       shots)

(** Neighbors already struck are skipped: after a hit on the ship's end (2,2)
    and misses at its other neighbors (1,2), (3,2), and (2,1), the only
    follow-up is (2,3). *)
let test_medium_skips_struck _ =
  let board = one_ship () in
  List.iter
    (fun (x, y) -> ignore (Board.strike board x y))
    [ (2, 2); (1, 2); (3, 2); (2, 1) ];
  List.iter
    (fun seed ->
      let m = Strategy.create Strategy.Medium (rng seed) in
      assert_equal ~msg:(say seed "shot") (2, 3) (Strategy.choose_shot m board))
    (List.init 10 Fun.id)

(** Once the ship sinks, Medium stops following it up: its shots are no longer
    limited to the ship's neighbors. *)
let test_medium_after_sinking _ =
  let board = one_ship () in
  List.iter
    (fun (x, y) -> ignore (Board.strike board x y))
    [ (2, 2); (2, 3); (2, 4) ];
  let neighbors =
    [ (1, 2); (1, 3); (1, 4); (3, 2); (3, 3); (3, 4); (2, 1); (2, 5) ]
  in
  let shots =
    List.map
      (fun seed ->
        Strategy.choose_shot (Strategy.create Strategy.Medium (rng seed)) board)
      seeds
  in
  assert_bool "some shot away from the sunk ship"
    (List.exists (fun p -> not (List.mem p neighbors)) shots)

(** Radar-revealed cells and revealed, uncollected chests can still be struck;
    struck cells cannot. *)
let test_strikeable _ =
  let board = one_ship () in
  ignore (Board.reveal board 0 0);
  ignore (Board.strike board 5 5);
  assert_bool "revealed by radar" (Strategy.strikeable board 0 0);
  assert_bool "hidden" (Strategy.strikeable board 4 4);
  assert_bool "already struck" (not (Strategy.strikeable board 5 5));
  assert_bool "out of bounds" (not (Strategy.strikeable board 6 0))

let test_difficulty_of_string _ =
  assert_equal (Some Strategy.Easy) (Strategy.difficulty_of_string "easy");
  assert_equal (Some Strategy.Hard) (Strategy.difficulty_of_string "hard");
  assert_equal None (Strategy.difficulty_of_string "Easy")

(** [tests] is every test of [Strategy]. *)
let tests =
  "strategy"
  >::: [
         "easy placement" >:: test_place_easy;
         "medium and hard placement do not touch" >:: test_place_spaced;
         "placement is reproducible" >:: test_place_reproducible;
         "placement errors" >:: test_place_errors;
         "full games" >:: test_full_games;
         "medium follows a hit" >:: test_medium_follows_hit;
         "easy ignores a hit" >:: test_easy_ignores_hit;
         "medium skips struck neighbors" >:: test_medium_skips_struck;
         "medium after sinking" >:: test_medium_after_sinking;
         "strikeable cells" >:: test_strikeable;
         "difficulty names" >:: test_difficulty_of_string;
       ]

let () = run_test_tt_main tests

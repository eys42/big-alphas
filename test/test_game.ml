(** Game tests: test the rules/API *)

open OUnit2
open Big_alphas

(* Use a fixed seed so tests are reproducible. *)
let new_game () = Game.create ~seed:42 Strategy.Easy

let assert_invalid_argument f =
  try
    ignore (f ());
    assert_failure "Expected Invalid_argument"
  with Invalid_argument _ -> ()

(* Find a cell matching a condition, without hardcoding ship locations. *)
let find_cell board predicate =
  let size = Board.dim board in
  let rec search x y =
    if x >= size then assert_failure "No matching cell found"
    else if y >= size then search (x + 1) 0
    else if predicate (Board.cell_at_own board x y) then (x, y)
    else search x (y + 1)
  in
  search 0 0

let is_empty = function
  | Board.Empty -> true
  | _ -> false

let is_unhit_ship = function
  | Board.Occupied (_, false) -> true
  | _ -> false

(* Find a coordinate that can currently be attacked. *)
let find_strikeable board =
  let size = Board.dim board in
  let rec search x y =
    if x >= size then assert_failure "No strikeable cell found"
    else if y >= size then search (x + 1) 0
    else if Strategy.strikeable board x y then (x, y)
    else search x (y + 1)
  in
  search 0 0

(* Snapshot board states without comparing ship internals. *)
let cell_description = function
  | Board.Empty -> "empty"
  | Board.Hidden -> "hidden"
  | Board.Revealed None -> "revealed-empty"
  | Board.Revealed (Some _) -> "revealed-ship"
  | Board.Occupied (_, false) -> "unhit-ship"
  | Board.Occupied (_, true) -> "hit-ship"
  | Board.Sunk (_, false) -> "sunk-ship"
  | Board.Sunk (_, true) -> "sunk-golden-ship"
  | Board.Miss -> "miss"
  | Board.Treasure_chest (Board.Nuke, revealed) ->
      if revealed then "revealed-nuke" else "nuke"
  | Board.Treasure_chest (Board.Radar, revealed) ->
      if revealed then "revealed-radar" else "radar"
  | Board.Treasure_chest (Board.Collected, _) -> "collected-chest"

let snapshot_board board =
  let size = Board.dim board in
  List.init size (fun x ->
      List.init size (fun y -> cell_description (Board.cell_at_own board x y)))

(* TEST 1: A new game initializes correctly. *)
let test_initial_state _ =
  let game = new_game () in
  assert_equal Game.Human (Game.current_player game);
  assert_equal None (Game.winner game);
  assert_equal
    (Board.dim (Game.human_board game))
    (Board.dim (Game.opponent_board game));
  ignore (find_cell (Game.human_board game) is_unhit_ship);
  ignore (find_cell (Game.opponent_board game) is_unhit_ship)

(* TEST 2: A human miss is recorded correctly. *)
let test_human_miss _ =
  let game = new_game () in
  let board = Game.opponent_board game in
  let x, y = find_cell board is_empty in

  assert_equal Board.Miss (Game.human_strike game x y);
  assert_equal Board.Miss (Board.cell_at_own board x y);
  assert_equal Game.Computer (Game.current_player game)

(* TEST 3: A human hit is recorded correctly. *)
let test_human_hit _ =
  let game = new_game () in
  let board = Game.opponent_board game in
  let x, y = find_cell board is_unhit_ship in

  (match Game.human_strike game x y with
  | Board.Occupied (_, true) | Board.Sunk _ -> ()
  | _ -> assert_failure "Expected hit or sunk ship");

  assert_bool "Hit cell should not be strikeable"
    (not (Strategy.strikeable board x y))

(* TEST 4: Out-of-bounds attacks are rejected. *)
let test_out_of_bounds _ =
  let game = new_game () in
  let board = Game.opponent_board game in
  let size = Board.dim board in
  let before = snapshot_board board in

  assert_invalid_argument (fun () -> Game.human_strike game (-1) 0);
  assert_invalid_argument (fun () -> Game.human_strike game 0 (-1));
  assert_invalid_argument (fun () -> Game.human_strike game size 0);
  assert_invalid_argument (fun () -> Game.human_strike game 0 size);

  assert_equal before (snapshot_board board);
  assert_equal Game.Human (Game.current_player game)

(* TEST 5: Humans cannot attack out of turn. *)
let test_wrong_turn _ =
  let game = new_game () in
  let board = Game.opponent_board game in
  let x, y = find_cell board is_empty in

  ignore (Game.human_strike game x y);

  let before = snapshot_board board in
  let x2, y2 = find_cell board is_empty in

  assert_invalid_argument (fun () -> Game.human_strike game x2 y2);
  assert_equal before (snapshot_board board);
  assert_equal Game.Computer (Game.current_player game)

(* TEST 6: Computer cannot attack before its turn. *)
let test_computer_cannot_start _ =
  let game = new_game () in
  let board = Game.human_board game in
  let before = snapshot_board board in

  assert_invalid_argument (fun () -> Game.machine_strike game);
  assert_equal before (snapshot_board board);
  assert_equal Game.Human (Game.current_player game)

(* TEST 7: Computer makes a valid attack on its turn. *)
let test_computer_turn _ =
  let game = new_game () in
  let x, y = find_cell (Game.opponent_board game) is_empty in
  ignore (Game.human_strike game x y);

  let board = Game.human_board game in
  let (mx, my), _ = Game.machine_strike game in
  let size = Board.dim board in

  assert_bool "Computer x out of bounds" (mx >= 0 && mx < size);
  assert_bool "Computer y out of bounds" (my >= 0 && my < size);
  assert_bool "Computer attack must be recorded"
    (not (Strategy.strikeable board mx my));

  assert_equal Game.Human (Game.current_player game)

(* TEST 8: Computer cannot attack twice in a row. *)
let test_computer_cannot_go_twice _ =
  let game = new_game () in
  let x, y = find_cell (Game.opponent_board game) is_empty in

  ignore (Game.human_strike game x y);
  ignore (Game.machine_strike game);

  let board = Game.human_board game in
  let before = snapshot_board board in

  assert_invalid_argument (fun () -> Game.machine_strike game);
  assert_equal before (snapshot_board board);
  assert_equal Game.Human (Game.current_player game)

(* TEST 9: Repeated attacks on a missed cell are rejected. *)
let test_repeated_miss _ =
  let game = new_game () in
  let board = Game.opponent_board game in
  let x, y = find_cell board is_empty in

  ignore (Game.human_strike game x y);
  ignore (Game.machine_strike game);

  assert_invalid_argument (fun () -> Game.human_strike game x y);
  assert_equal Board.Miss (Board.cell_at_own board x y);
  assert_equal Game.Human (Game.current_player game)

(* TEST 10: Repeated attacks on a hit ship are rejected. *)
let test_repeated_hit _ =
  let game = new_game () in
  let board = Game.opponent_board game in
  let x, y = find_cell board is_unhit_ship in

  ignore (Game.human_strike game x y);
  ignore (Game.machine_strike game);

  let before = snapshot_board board in

  assert_invalid_argument (fun () -> Game.human_strike game x y);
  assert_equal before (snapshot_board board);
  assert_equal Game.Human (Game.current_player game)

(* TEST 11: Human attacks do not change their own board. *)
let test_human_preserves_own_board _ =
  let game = new_game () in
  let board = Game.human_board game in
  let before = snapshot_board board in
  let x, y = find_cell (Game.opponent_board game) is_empty in

  ignore (Game.human_strike game x y);

  assert_equal before (snapshot_board board)

(* TEST 12: Computer attacks do not change its own board. *)
let test_computer_preserves_own_board _ =
  let game = new_game () in
  let x, y = find_cell (Game.opponent_board game) is_empty in
  ignore (Game.human_strike game x y);

  let board = Game.opponent_board game in
  let before = snapshot_board board in

  ignore (Game.machine_strike game);

  assert_equal before (snapshot_board board)

(* TEST 13: A complete game eventually ends with a winner. *)
let test_full_game _ =
  let game = new_game () in
  let size = Board.dim (Game.human_board game) in

  let rec play fuel =
    match Game.winner game with
    | Some _ -> ()
    | None ->
        if fuel = 0 then assert_failure "Game did not finish";

        (match Game.current_player game with
        | Game.Human ->
            let x, y = find_strikeable (Game.opponent_board game) in
            ignore (Game.human_strike game x y)
        | Game.Computer -> ignore (Game.machine_strike game));

        play (fuel - 1)
  in

  play ((2 * size * size) + 2);

  assert_bool "Game should have a winner" (Game.winner game <> None);

  assert_invalid_argument (fun () -> Game.human_strike game 0 0);
  assert_invalid_argument (fun () -> Game.machine_strike game)

let is_nuke_chest = function
  | Board.Treasure_chest (Board.Nuke, _) -> true
  | _ -> false

(** [with_nuke ()] is a game in which the human has just opened the computer's
    nuke chest and the computer has taken its turn, so it is the human's turn
    again. *)
let with_nuke () =
  let game = new_game () in
  let x, y = find_cell (Game.opponent_board game) is_nuke_chest in
  ignore (Game.human_strike game x y);
  ignore (Game.machine_strike game);
  game

let test_nukes_start_at_zero _ =
  let game = new_game () in
  assert_equal 0 (Game.nukes game Game.Human);
  assert_equal 0 (Game.nukes game Game.Computer)

let test_nuke_chest_gives_nuke _ =
  let game = new_game () in
  let x, y = find_cell (Game.opponent_board game) is_nuke_chest in
  ignore (Game.human_strike game x y);
  assert_equal 1 (Game.nukes game Game.Human)

let test_human_nuke_needs_nuke _ =
  let game = new_game () in
  assert_invalid_argument (fun () -> Game.human_nuke game 5 5);
  assert_equal Game.Human (Game.current_player game)

let test_human_nuke _ =
  let game = with_nuke () in
  let results = Game.human_nuke game 5 5 in
  (* nine cells, minus the nuke chest if it was in the area *)
  assert_bool "blast struck the area" (List.length results >= 8);
  assert_equal 0 (Game.nukes game Game.Human);
  assert_equal Game.Computer (Game.current_player game);
  ignore (Game.machine_strike game);
  assert_invalid_argument (fun () -> Game.human_nuke game 5 5)

(* Invalid game actions must not spend a power-up or advance the turn. *)
let test_human_nuke_errors_keep_nuke _ =
  let game = with_nuke () in
  let board = Game.opponent_board game in
  let size = Board.dim board in
  let before = snapshot_board board in

  assert_invalid_argument (fun () -> Game.human_nuke game (-1) 0);
  assert_invalid_argument (fun () -> Game.human_nuke game 0 size);

  assert_equal before (snapshot_board board);
  assert_equal 1 (Game.nukes game Game.Human);
  assert_equal Game.Human (Game.current_player game);
  assert_equal None (Game.winner game);

  (* The nuke is still available after both failed attempts. *)
  ignore (Game.human_nuke game 5 5);
  assert_equal 0 (Game.nukes game Game.Human);
  assert_equal Game.Computer (Game.current_player game)

let test_human_nuke_wrong_turn _ =
  let game = with_nuke () in
  ignore (Game.human_nuke game 5 5);
  assert_invalid_argument (fun () -> Game.human_nuke game 1 1)

let test_machine_turn_shot _ =
  let game = new_game () in
  let x, y = find_strikeable (Game.opponent_board game) in
  ignore (Game.human_strike game x y);
  match Game.machine_turn game with
  | Game.Shot _ -> assert_equal Game.Human (Game.current_player game)
  | Game.Blast _ -> assert_failure "no nuke, so the computer cannot blast"

let test_machine_turn_wrong_turn _ =
  assert_invalid_argument (fun () -> Game.machine_turn (new_game ()))

(** Medium and Hard use [machine_turn] for whole games, which may include nukes,
    and every game ends with a winner. *)
let test_full_game_machine_turn _ =
  List.iter
    (fun difficulty ->
      List.iter
        (fun seed ->
          let game = Game.create ~seed difficulty in
          let size = Board.dim (Game.human_board game) in
          let rec play fuel =
            match Game.winner game with
            | Some _ -> ()
            | None ->
                if fuel = 0 then assert_failure "Game did not finish";
                (match Game.current_player game with
                | Game.Human ->
                    let x, y = find_strikeable (Game.opponent_board game) in
                    ignore (Game.human_strike game x y)
                | Game.Computer -> ignore (Game.machine_turn game));
                play (fuel - 1)
          in
          play ((2 * size * size) + 2))
        [ 1; 2; 3; 4; 5 ])
    [ Strategy.Medium; Strategy.Hard ]

(* TEST: Manually placed human ships are used to initialize the board. *)
let test_manual_ship_placement _ =
  let ships =
    [
      Board.Ship.create (0, 0) Board.East 5;
      Board.Ship.create (2, 0) Board.East 4;
      Board.Ship.create (4, 0) Board.East 3;
      Board.Ship.create (6, 0) Board.East 3;
      Board.Ship.create (8, 0) Board.East 2;
    ]
  in
  let game = Game.create ~seed:42 ~human_ships:ships Strategy.Easy in
  let board = Game.human_board game in

  List.iter
    (fun (row, length) ->
      for col = 0 to length - 1 do
        match Board.cell_at_own board row col with
        | Board.Occupied (_, false) -> ()
        | _ -> assert_failure "Manually placed ship is missing"
      done)
    [ (0, 5); (2, 4); (4, 3); (6, 3); (8, 2) ];

  assert_equal Game.Human (Game.current_player game);
  assert_equal None (Game.winner game)

(* TEST: computer's win victory handling. *)
let test_computer_victory _ =
  let game = new_game () in
  let board = Game.human_board game in
  let last_x, last_y = find_cell board is_unhit_ship in
  let size = Board.dim board in

  for x = 0 to size - 1 do
    for y = 0 to size - 1 do
      if (x, y) <> (last_x, last_y) then ignore (Board.strike board x y)
    done
  done;

  let x, y = find_cell (Game.opponent_board game) is_empty in
  ignore (Game.human_strike game x y);
  assert_equal Game.Computer (Game.current_player game);
  assert_equal None (Game.winner game);

  let (mx, my), result = Game.machine_strike game in
  assert_equal (last_x, last_y) (mx, my);
  (match result with
  | Board.Sunk _ -> ()
  | _ -> assert_failure "Final computer strike should sink the last ship");
  assert_equal (Some Game.Computer) (Game.winner game);
  assert_invalid_argument (fun () -> Game.human_strike game 0 0);
  assert_invalid_argument (fun () -> Game.machine_turn game)

(* TEST: Computer nuke collection. *)
let computer_with_nuke () =
  let rec try_seed seed =
    if seed > 300 then
      assert_failure "Could not find a computer nuke pickup in test seeds"
    else
      let game = Game.create ~seed Strategy.Medium in
      let rec find_nuke shots_left =
        if shots_left = 0 then None
        else (
          let x, y = find_cell (Game.opponent_board game) is_empty in
          ignore (Game.human_strike game x y);
          let (mx, my), result = Game.machine_strike game in
          match result with
          | Board.Treasure_chest (Board.Nuke, _) -> Some (game, (mx, my))
          | _ -> find_nuke (shots_left - 1))
      in
      match find_nuke 6 with
      | Some found -> found
      | None -> try_seed (seed + 1)
  in
  try_seed 1

(* Opening a chest via a computer attack credits the COMPUTER, not the human. *)
let test_computer_nuke_collection _ =
  let game, (x, y) = computer_with_nuke () in
  assert_equal 1 (Game.nukes game Game.Computer);
  assert_equal 0 (Game.nukes game Game.Human);
  assert_equal Game.Human (Game.current_player game);
  assert_equal
    (Board.Treasure_chest (Board.Collected, true))
    (Board.cell_at_own (Game.human_board game) x y)

(* machine_turn, unlike machine_strike, should spend a collected nuke. *)
let test_computer_nuke_usage _ =
  let game, _ = computer_with_nuke () in
  assert_equal 1 (Game.nukes game Game.Computer);

  let x, y = find_cell (Game.opponent_board game) is_empty in
  ignore (Game.human_strike game x y);
  let board = Game.human_board game in
  let before = snapshot_board board in

  (match Game.machine_turn game with
  | Game.Blast (_, results) ->
      assert_bool "Computer blast must affect at least one cell" (results <> []);
      assert_bool "Computer blast must update the human's board"
        (before <> snapshot_board board)
  | Game.Shot _ -> assert_failure "Medium should use its available nuke");

  assert_equal 0 (Game.nukes game Game.Computer);
  assert_equal 0 (Game.nukes game Game.Human);
  assert_equal Game.Human (Game.current_player game);
  assert_equal None (Game.winner game)

(* The Game module must recognize victory caused by a blast, not only a shot. *)
let test_nuke_victory _ =
  let game = with_nuke () in
  let board = Game.opponent_board game in
  let last_x, last_y = find_cell board is_unhit_ship in
  let size = Board.dim board in

  (* Set up a nearly finished opponent board without changing Game's turn. *)
  for x = 0 to size - 1 do
    for y = 0 to size - 1 do
      if (x, y) <> (last_x, last_y) then
        match Board.cell_at_own board x y with
        | Board.Occupied (_, false) -> ignore (Board.strike board x y)
        | _ -> ()
    done
  done;

  assert_equal None (Game.winner game);
  let results = Game.human_nuke game last_x last_y in
  assert_bool "Blast must strike the last ship cell"
    (List.exists
       (function
         | (x, y), Board.Sunk _ -> (x, y) = (last_x, last_y)
         | _ -> false)
       results);
  assert_equal (Some Game.Human) (Game.winner game);
  assert_equal 0 (Game.nukes game Game.Human);
  assert_invalid_argument (fun () -> Game.human_strike game 0 0);
  assert_invalid_argument (fun () -> Game.machine_turn game)

(* Verify that Game does not consume the nuke or change turns when the blast 
  is rejected. *)
let test_nuke_used_area_preserves_state _ =
  let game = with_nuke () in
  let board = Game.opponent_board game in
  ignore (Board.nuke board 5 5);
  let before = snapshot_board board in

  assert_invalid_argument (fun () -> Game.human_nuke game 5 5);
  assert_equal before (snapshot_board board);
  assert_equal 1 (Game.nukes game Game.Human);
  assert_equal Game.Human (Game.current_player game);
  assert_equal None (Game.winner game)

let suite =
  "Game rules"
  >::: [
         "initial state" >:: test_initial_state;
         "human miss" >:: test_human_miss;
         "human hit" >:: test_human_hit;
         "out of bounds" >:: test_out_of_bounds;
         "wrong turn" >:: test_wrong_turn;
         "computer cannot start" >:: test_computer_cannot_start;
         "computer attack" >:: test_computer_turn;
         "computer cannot go twice" >:: test_computer_cannot_go_twice;
         "repeated miss" >:: test_repeated_miss;
         "repeated hit" >:: test_repeated_hit;
         "human preserves own board" >:: test_human_preserves_own_board;
         "computer preserves own board" >:: test_computer_preserves_own_board;
         "complete game" >:: test_full_game;
         "nukes start at zero" >:: test_nukes_start_at_zero;
         "nuke chest gives a nuke" >:: test_nuke_chest_gives_nuke;
         "human nuke needs a nuke" >:: test_human_nuke_needs_nuke;
         "human nuke" >:: test_human_nuke;
         "failed nuke keeps the nuke" >:: test_human_nuke_errors_keep_nuke;
         "human nuke out of turn" >:: test_human_nuke_wrong_turn;
         "computer shot without a nuke" >:: test_machine_turn_shot;
         "computer cannot go out of turn" >:: test_machine_turn_wrong_turn;
         "complete game with nukes" >:: test_full_game_machine_turn;
         "testing manual ship placement" >:: test_manual_ship_placement;
         "computer victory" >:: test_computer_victory;
         "computer collects a nuke" >:: test_computer_nuke_collection;
         "computer uses a nuke" >:: test_computer_nuke_usage;
         "nuke causes victory" >:: test_nuke_victory;
         "nuking an already-struck area preserves game state"
         >:: test_nuke_used_area_preserves_state;
       ]

let () = run_test_tt_main suite

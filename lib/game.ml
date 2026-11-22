(** Game rules and turn management for single-player Battleship.

    NOTE (by carissa): Nuke power-ups are active, but radar, golden-ship special
    rules, and multiplayer r not active yet. *)

type player =
  | Human
  | Computer

type t = {
  human_board : Board.t;
  computer_board : Board.t;
  machine : Strategy.t;
  mutable turn : player;
  mutable winner : player option;
  mutable human_nukes : int;
  mutable computer_nukes : int;
}

(* Provisional board settings. *)
let board_size = 10
let ship_lengths = [ 5; 4; 3; 3; 2 ]

let create ?seed ?human_ships difficulty =
  (* A random number generator for ship placement, treasure chests, and the
     computer's strategy *)
  let rng =
    match seed with
    | Some value -> Random.State.make [| value |]
    | None -> Random.State.make_self_init ()
  in
  (*Ship placements

    NOTE (by carissa) Human ship placement is random (based on Strategy.Easy's
    auto placement) unless ships are provided by the CLI. *)
  let human_ships =
    match human_ships with
    | Some ships -> ships
    | None -> Strategy.place_ships Strategy.Easy rng board_size ship_lengths
  in
  let computer_ships =
    Strategy.place_ships difficulty rng board_size ship_lengths
  in

  (*Initialize the boards*)
  let human_board = Board.init board_size human_ships (Random.State.bits rng) in
  let computer_board =
    Board.init board_size computer_ships (Random.State.bits rng)
  in
  {
    human_board;
    computer_board;
    machine = Strategy.create difficulty rng;
    turn = Human;
    winner = None;
    human_nukes = 0;
    computer_nukes = 0;
  }

let human_board game = game.human_board
let opponent_board game = game.computer_board
let current_player game = game.turn
let winner game = game.winner

(** [has_afloat_ship board] is whether the board contains at least one cell
    belonging to a ship that has not been sunk. A previously hit but not yet
    sunk ship is still afloat. *)
let has_afloat_ship board =
  let size = Board.dim board in
  let rec search x y =
    if x >= size then false
    else if y >= size then search (x + 1) 0
    else
      match Board.cell_at_own board x y with
      | Board.Occupied _ | Board.Revealed (Some _) -> true
      | _ -> search x (y + 1)
  in
  search 0 0

(** Prevent attacks out of turn or after the game has ended. *)
let require_turn game player =
  if game.winner <> None then invalid_arg "Game has already ended";
  if game.turn <> player then invalid_arg "It is not this player's turn"

(** Complete an attack: either record a winner or hand over the turn. *)
let finish_turn game attacker target_board =
  if not (has_afloat_ship target_board) then game.winner <- Some attacker
  else
    game.turn <-
      (match attacker with
      | Human -> Computer
      | Computer -> Human)

let nukes game = function
  | Human -> game.human_nukes
  | Computer -> game.computer_nukes

(** [nukes_found cells] is how many of [cells] are nuke chests that were just
    opened. *)
let nukes_found cells =
  List.length
    (List.filter
       (function
         | Board.Treasure_chest (Board.Nuke, _) -> true
         | _ -> false)
       cells)

(** [add_nukes game player n] gives [player] [n] more nukes. *)
let add_nukes game player n =
  match player with
  | Human -> game.human_nukes <- game.human_nukes + n
  | Computer -> game.computer_nukes <- game.computer_nukes + n

let human_strike game x y =
  require_turn game Human;
  if not (Strategy.strikeable game.computer_board x y) then
    invalid_arg "Invalid or previously attacked coordinate";
  let result = Board.strike game.computer_board x y in
  add_nukes game Human (nukes_found [ result ]);
  finish_turn game Human game.computer_board;
  result

let machine_strike game =
  require_turn game Computer;
  let x, y = Strategy.choose_shot game.machine game.human_board in
  let result = Board.strike game.human_board x y in
  add_nukes game Computer (nukes_found [ result ]);
  finish_turn game Computer game.human_board;
  ((x, y), result)

(** [blast game attacker target_board x y] spends one of [attacker]'s nukes on
    the 3x3 area centered on [(x, y)] of [target_board], gives [attacker] any
    nukes found in the area, and ends the turn. Requires that [attacker] has a
    nuke. Raises [Invalid_argument], without changing the game, if [(x, y)] is
    out of bounds or the whole area has already been struck. *)
let blast game attacker target_board x y =
  let results = Board.nuke target_board x y in
  if results = [] then invalid_arg "Nothing left to nuke in that area";
  add_nukes game attacker (nukes_found (List.map snd results) - 1);
  finish_turn game attacker target_board;
  results

let human_nuke game x y =
  require_turn game Human;
  if game.human_nukes < 1 then invalid_arg "No nukes available";
  blast game Human game.computer_board x y

type machine_move =
  | Shot of (int * int) * Board.cell
  | Blast of (int * int) * ((int * int) * Board.cell) list

let machine_turn game =
  require_turn game Computer;
  let target =
    if game.computer_nukes > 0 then
      Strategy.choose_nuke game.machine game.human_board
    else None
  in
  match target with
  | Some (x, y) -> Blast ((x, y), blast game Computer game.human_board x y)
  | None ->
      let shot, result = machine_strike game in
      Shot (shot, result)

(** Convert the result of one attack to a message for the CLI. *)
let describe_attack = function
  | Board.Miss -> "Miss!"
  | Board.Occupied (_, true) -> "Hit!"
  | Board.Sunk _ -> "Ship sunk!"
  | Board.Treasure_chest (Board.Nuke, _) -> "Found a nuke chest!"
  | Board.Treasure_chest (Board.Radar, _) ->
      "Found a radar chest (power-ups not active yet)."
  | _ -> "Attack completed."

(** What the player asked to do on their turn. *)
type action =
  | Fire of int * int
  | Nuke_at of int * int

let show_boards game =
  print_endline "\nYour board:";
  Display.print_board game.human_board true;
  print_endline "\nComputer's board (ships hidden until hit):";
  Display.print_board game.computer_board false

(** [prompt_attack game] reads an attack in column/row order, or
    [nuke COLUMN ROW] to spend a nuke. Uses CLI for normal commands and help.
    [None] means the user typed q/quit or closed standard input. *)
let rec prompt_attack game =
  let board = game.computer_board in
  let nukes = game.human_nukes in
  if nukes > 0 then
    Printf.printf "\nYou have %d nuke(s); type 'nuke COLUMN ROW' to use one."
      nukes;
  Printf.printf
    "\nPick where to strike! Enter COLUMN ROW (0-%d), or h for help: "
    (Board.dim board - 1);
  flush stdout;
  match read_line () with
  | exception End_of_file -> None
  | input -> (
      let input = String.trim input in
      let words =
        String.split_on_char ' ' (String.lowercase_ascii input)
        |> List.filter (fun word -> word <> "")
      in
      match words with
      (* CLI does not parse nuke commands yet, so handle those here. *)
      | [ "nuke"; col; row ] -> (
          match (int_of_string_opt col, int_of_string_opt row) with
          | Some x, Some y when nukes > 0 -> Some (Nuke_at (x, y))
          | _, _ when nukes = 0 ->
              print_endline "You have no nukes.";
              prompt_attack game
          | _ ->
              print_endline "Please enter two numbers after nuke.";
              prompt_attack game)
      | "nuke" :: _ ->
          print_endline "Use: nuke COLUMN ROW";
          prompt_attack game
      | _ -> (
          match Cli.parse_command input with
          | Cli.Attack (x, y) when Strategy.strikeable board x y ->
              Some (Fire (x, y))
          | Cli.Attack _ ->
              print_endline "Invalid or previously attacked coordinate.";
              prompt_attack game
          | Cli.Show_board ->
              show_boards game;
              prompt_attack game
          | Cli.Help ->
              Cli.print_help ();
              if nukes > 0 then print_endline "  nuke COLUMN ROW  Use a nuke";
              prompt_attack game
          | Cli.Quit -> None
          | Cli.Invalid message ->
              print_endline message;
              prompt_attack game))

(** [describe_blast results] is one line per cell that a nuke struck. *)
let describe_blast results =
  List.iter
    (fun ((x, y), cell) ->
      Printf.printf "  (%d, %d): %s\n" x y (describe_attack cell))
    results

(** Game loop for single-player Battleship. One loop iteration processes a turn;
    recursion stops after victory or q. *)
let rec play game =
  match game.winner with
  | Some Human ->
      show_boards game;
      print_endline "\nYou win! All computer ships have been sunk."
  | Some Computer ->
      show_boards game;
      print_endline "\nComputer wins! All your ships have been sunk."
  | None -> (
      match game.turn with
      | Human -> (
          show_boards game;
          match prompt_attack game with
          | None -> print_endline "Game exited."
          | Some (Fire (x, y)) ->
              let result = human_strike game x y in
              Printf.printf "You attacked (%d, %d): %s\n" x y
                (describe_attack result);
              play game
          | Some (Nuke_at (x, y)) ->
              (match human_nuke game x y with
              | results ->
                  Printf.printf "You nuked the area around (%d, %d):\n" x y;
                  describe_blast results
              | exception Invalid_argument msg -> print_endline msg);
              play game)
      | Computer -> (
          match machine_turn game with
          | Shot ((x, y), result) ->
              Printf.printf "Computer attacked (%d, %d): %s\n" x y
                (describe_attack result);
              play game
          | Blast ((x, y), results) ->
              Printf.printf "Computer nuked the area around (%d, %d):\n" x y;
              describe_blast results;
              play game))

let run (settings : Cli.settings) =
  match settings.players with
  | 1 -> (
      match Strategy.difficulty_of_string settings.difficulty with
      | Some difficulty -> (
          print_endline "Welcome to Battleship!";
          print_endline
            "Place your ships first. Strike a nuke chest to win a nuke; radar \
             and golden ships are not active yet.";
          match Cli.read_ship_placements board_size ship_lengths with
          | Some ships -> play (create ~human_ships:ships difficulty)
          | None -> print_endline "Game exited.")
      | None -> invalid_arg "Invalid computer difficulty")
  | 2 ->
      (* Temporary placeholder for multiplayer *)
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
  | _ -> invalid_arg "Number of players must be 1 or 2"

(* Parses and validates command-line arguments for the number of players and
   machine difficulty. *)

open BatOptParse

type settings = {
  players : int;
  difficulty : string;
}

(** [parse_args ()] reads and validates the command-line options [--players] and
    [--difficulty], returning the game settings. Prints an error message and
    exits with an unsuccessful status if the arguments are invalid or required
    options are missing. *)
let parse_args () =
  let parser =
    OptParser.make ~usage:"%prog --players NUMBER --difficulty LEVEL"
      ~description:
        "Play Battleship with power-ups and golden ships. Here are some \
         helpful commands that you can enter:"
      ()
  in
  let players = StdOpt.str_option ~metavar:"NUMBER" () in
  let difficulty = StdOpt.str_option ~metavar:"LEVEL" () in

  OptParser.add parser ~long_name:"players"
    ~help:"number of human players: 1 or 2" players;
  OptParser.add parser ~long_name:"difficulty"
    ~help:"machine difficulty: easy, medium, or hard; ignored with 2 players"
    difficulty;

  let fail message =
    OptParser.error parser message;
    exit 1
  in
  let positional = OptParser.parse_argv parser in
  if positional <> [] then fail "Use --players NUMBER --difficulty LEVEL";

  let players_value =
    match Opt.opt players with
    | Some "1" -> 1
    | Some "2" -> 2
    | Some _ -> fail "--players must be 1 or 2"
    | None -> fail "Missing --players NUMBER"
  in
  let difficulty_value =
    match Opt.opt difficulty with
    | Some "easy" -> "easy"
    | Some "medium" -> "medium"
    | Some "hard" -> "hard"
    | Some _ -> fail "--difficulty must be easy, medium, or hard"
    | None -> fail "Missing --difficulty LEVEL"
  in
  { players = players_value; difficulty = difficulty_value }

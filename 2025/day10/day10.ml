(** Day 10: Factory

    Toggle indicator lights using buttons to match a target pattern.

    {2 Problem Summary:}
    - {b Part 1:} Find minimum button presses to configure all machines'
      indicator lights
    - {b Part 2:} [Involves joltage requirements - not implemented]

    See details at:
    {{:https://adventofcode.com/2025/day/10} Advent of Code 2025, Day 10} *)

(** Represents a machine with target state and available buttons *)
type machine = {
  target : int;  (** Target light configuration as bitmask *)
  buttons : int list;
      (** List of button masks (each button toggles certain lights) *)
}

(** Parse a single indicator light pattern like "[.##.]" into a bitmask where
    '#' = 1 (on) and '.' = 0 (off) *)
let parse_target s =
  (* Remove brackets and convert to bitmask *)
  let inner = String.sub s 1 (String.length s - 2) in
  String.fold_left
    (fun (acc, pos) c ->
      match c with
      | '#' -> (acc lor (1 lsl pos), pos + 1)
      | '.' -> (acc, pos + 1)
      | _ -> (acc, pos))
    (0, 0) inner
  |> fst

(** Parse a button schematic like "(0,2,3)" into a bitmask *)
let parse_button s =
  (* Remove parentheses *)
  let inner = String.sub s 1 (String.length s - 2) in
  if String.length inner = 0 then 0
  else
    String.split_on_char ',' inner
    |> List.fold_left
         (fun acc pos_str ->
           let pos = int_of_string (String.trim pos_str) in
           acc lor (1 lsl pos))
         0

(** Parse a single line describing one machine *)
let parse_line line =
  (* Split by spaces and categorize by bracket type *)
  let parts =
    String.split_on_char ' ' line |> List.filter (fun s -> String.length s > 0)
  in
  let target = ref 0 in
  let buttons = ref [] in
  List.iter
    (fun part ->
      if String.length part > 0 then
        match part.[0] with
        | '[' -> target := parse_target part
        | '(' -> buttons := parse_button part :: !buttons
        | '{' -> () (* Ignore joltage requirements for part 1 *)
        | _ -> ())
    parts;
  { target = !target; buttons = List.rev !buttons }

(** Parse input string into a list of machines *)
let parse input =
  input |> String.split_on_char '\n'
  |> List.filter (fun line -> String.trim line <> "")
  |> List.map parse_line

(** BFS state for searching minimum button presses *)
module IntSet = Set.Make (Int)

(** Solve for minimum button presses for a single machine using BFS *)
let solve_machine machine =
  (* BFS: queue contains (current_state, cost) pairs *)
  let rec bfs queue visited =
    match queue with
    | [] -> -1 (* No solution found *)
    | (state, cost) :: rest ->
        if state = machine.target then cost
        else if IntSet.mem state visited then bfs rest visited
        else
          let visited = IntSet.add state visited in
          (* Try pressing each button *)
          let new_states =
            List.filter_map
              (fun button ->
                let next_state = state lxor button in
                if IntSet.mem next_state visited then None
                else Some (next_state, cost + 1))
              machine.buttons
          in
          bfs (rest @ new_states) visited
  in
  bfs [ (0, 0) ] IntSet.empty

(** [part1 input] solves part 1 of the challenge

    @param input Raw input string from the puzzle
    @return Sum of minimum presses for all machines *)
let part1 input =
  let machines = parse input in
  List.fold_left (fun acc machine -> acc + solve_machine machine) 0 machines

(** [part2 input] solves part 2 of the challenge

    @param input Raw input string from the puzzle
    @return Solution for part 2 *)
let part2 input =
  let _ = parse input in
  (* TODO: Implement part 2 solution - involves joltage requirements *)
  0L

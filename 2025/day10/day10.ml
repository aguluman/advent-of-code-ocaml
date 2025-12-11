(** Day 10: Factory

    Toggle indicator lights using buttons to match a target pattern.

    {2 Problem Summary:}
    - {b Part 1:} Find minimum button presses to configure all machines'
      indicator lights
    - {b Part 2:} Find minimum button presses to configure joltage counters

    See details at:
    {{:https://adventofcode.com/2025/day/10} Advent of Code 2025, Day 10} *)

(** Represents a machine with target state and available buttons *)
type machine = {
  target : int;
  buttons : int list;
  joltage : float list;
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

(** Parse joltage requirements like "{3,5,4,7}" into a float list *)
let parse_joltage s =
  let inner = String.sub s 1 (String.length s - 2) in
  String.split_on_char ',' inner
  |> List.map (fun x -> float_of_string (String.trim x))

(** Parse a single line describing one machine *)
let parse_line line =
  (* Split by spaces and categorize by bracket type *)
  let parts =
    String.split_on_char ' ' line |> List.filter (fun s -> String.length s > 0)
  in
  let target = ref 0 in
  let buttons = ref [] in
  let joltage = ref [] in
  List.iter
    (fun part ->
      if String.length part > 0 then
        match part.[0] with
        | '[' -> target := parse_target part
        | '(' -> buttons := parse_button part :: !buttons
        | '{' -> joltage := parse_joltage part
        | _ -> ())
    parts;
  { target = !target; buttons = List.rev !buttons; joltage = !joltage }

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

(** Solve for minimum button presses for joltage requirements using LP *)
let solve_machine_joltage machine =
  let open Lp in
  let num_buttons = List.length machine.buttons in
  (* Create integer variables for each button press count *)
  let v =
    Array.init num_buttons (fun i ->
        var ~integer:true (Printf.sprintf "button%d" i))
  in
  (* Sum of variables at given indices *)
  let sum indices =
    List.fold_left (fun acc i -> acc ++ v.(i)) (c 0.0) indices
  in
  (* Objective: minimize total button presses *)
  let obj = minimize (sum (List.init num_buttons (fun i -> i))) in
  (* Constraints: each joltage counter must reach its target *)
  let constraints =
    List.mapi
      (fun counter_idx target_val ->
        (* Find which buttons affect this counter *)
        let button_indices =
          List.mapi
            (fun btn_idx btn_mask ->
              if btn_mask land (1 lsl counter_idx) > 0 then
                Some (num_buttons - btn_idx - 1)
              else None)
            machine.buttons
          |> List.filter_map (fun v -> v)
        in
        sum button_indices =~ c target_val)
      machine.joltage
  in
  let problem = make obj constraints in
  match Lp_glpk.solve problem with
  | Ok (obj_val, _) -> obj_val
  | Error msg ->
      Printf.eprintf "LP solver error: %s\n" msg;
      0.0

(** [part1 input] solves part 1 of the challenge

    @param input Raw input string from the puzzle
    @return Sum of minimum presses for all machines *)
let part1 input =
  let machines = parse input in
  List.fold_left (fun acc machine -> acc + solve_machine machine) 0 machines

(** [part2 input] solves part 2 of the challenge

    @param input Raw input string from the puzzle
    @return Sum of minimum presses for joltage configuration *)
let part2 input =
  let machines = parse input in
  List.fold_left
    (fun acc machine -> acc +. solve_machine_joltage machine)
    0.0 machines
  |> Int64.of_float

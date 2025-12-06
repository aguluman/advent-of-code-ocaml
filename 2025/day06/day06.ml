(** Day 06: Trash Compactor

    [Brief description of the problem and what this module solves]

    {2 Problem Summary:}
    - {b Part 1:} Parse vertically-stacked math problems, compute results, and
      sum them
    - {b Part 2:} Read numbers column-wise (right-to-left), concatenate digits
      vertically per column, accumulate per problem, fold on operators

    See details at:
    {{:https://adventofcode.com/2025/day/6} Advent of Code 2025, Day 06} *)

(** Helper function to compute the result for a single problem

    @param nums List of numbers in the problem
    @param op Operator to apply ("+" or "*")
    @return Result of applying the operator to all numbers *)
let compute (nums, op) =
  match nums with
  | [] -> 0
  | hd :: tl ->
      List.fold_left
        (fun acc n ->
          match op with
          | "+" -> acc + n
          | "*" -> acc * n
          | _ -> failwith ("Invalid operator: " ^ op))
        hd tl

(** Helper function to parse input string into a structured data format

    @param input Raw input string from the puzzle
    @return List of problems, where each problem is (numbers list, operator) *)
let parse_part1 input =
  let lines =
    input |> String.split_on_char '\n'
    |> List.filter (fun line -> String.trim line <> "")
  in

  if List.length lines < 2 then failwith "Input must have at least 2 lines";

  (* Separate operators (last line) and number lines *)
  let ops_line = List.hd (List.rev lines) in
  let num_lines = List.rev (List.tl (List.rev lines)) in

  (* Parse operators: split and filter empties *)
  let ops =
    String.split_on_char ' ' ops_line |> List.filter (fun s -> s <> "")
  in

  (* Parse number grids: each line split and filtered *)
  let num_grids =
    List.map
      (fun line ->
        String.split_on_char ' ' line |> List.filter (fun s -> s <> ""))
      num_lines
  in

  (* Build problems: for each operator index, collect numbers from that column *)
  List.mapi
    (fun i op ->
      let nums =
        List.map (fun grid -> int_of_string (List.nth grid i)) num_grids
      in
      (nums, op))
    ops

(** [part1 input] solves part 1 of the challenge

    @param input Raw input string from the puzzle
    @return Grand total of all problem results *)
let part1 input =
  let problems = parse_part1 input in

  let results = List.map compute problems in

  List.fold_left ( + ) 0 results

(** Parse input for Part 2: column-wise traversal right-to-left, concatenate
    digits vertically

    @param input Raw input string
    @return Grand total for Part 2 *)
let parse_part2 input =
  let lines =
    input |> String.split_on_char '\n'
    |> List.filter (fun line -> String.trim line <> "")
    |> Array.of_list
  in
  let h = Array.length lines in
  let w = String.length lines.(0) in

  let rec traverse x res nbuf =
    if x < 0 then res
    else
      let num_str =
        String.concat ""
          (List.init (h - 1) (fun y ->
               if x < String.length lines.(y) then
                 let c = lines.(y).[x] in
                 if c <> ' ' then String.make 1 c else ""
               else ""))
      in
      match int_of_string_opt num_str with
      | Some n ->
          let op =
            if x < String.length lines.(h - 1) then lines.(h - 1).[x] else ' '
          in
          if op = '+' then
            traverse (x - 1) (res + List.fold_left ( + ) 0 (n :: nbuf)) []
          else if op = '*' then
            traverse (x - 1) (res + List.fold_left ( * ) 1 (n :: nbuf)) []
          else traverse (x - 1) res (n :: nbuf)
      | None -> traverse (x - 1) res []
  in
  traverse (w - 1) 0 []

(** [part2 input] solves part 2 of the challenge

    @param input Raw input string from the puzzle
    @return Grand total with column-wise reading *)
let part2 input = parse_part2 input |> Int64.of_int

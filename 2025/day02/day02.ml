(** Day 02: Gift Shop

    [Brief description of the problem and what this module solves]

    {2 Problem Summary:}
    - {b Part 1:} [Description of part 1]
    - {b Part 2:} [Description of part 2]

    See details at:
    {{:https://adventofcode.com/2025/day/2} Advent of Code 2025, Day 02} *)

let parse_range range =
  let range = String.trim range in
  let parts = String.split_on_char '-' range in
  match parts with
  | [ start_str; end_str ] -> (
      match
        ( int_of_string_opt (String.trim start_str),
          int_of_string_opt (String.trim end_str) )
      with
      | Some s, Some e -> Some (s, e)
      | _ -> None)
  | _ -> None

(** Helper function to parse input string into a structured data format

    @param input Raw input string from the puzzle
    @return Parsed data structure *)
let parse input =
  input |> String.split_on_char ','
  |> List.filter (fun range -> String.trim range <> "")
  |> List.filter_map parse_range

let is_invalid id =
  let s = string_of_int id in
  let len = String.length s in
  if len mod 2 <> 0 then false
  else
    let half = len / 2 in
    String.sub s 0 half = String.sub s half half

(** [part1 input] solves part 1 of the challenge

    @param input Raw input string from the puzzle
    @return Solution for part 1 *)
let part1 input =
  let ranges = parse input in
  List.fold_left
    (fun acc (start, finish) ->
      let range_sum =
        List.fold_left
          (fun inner_acc id ->
            if is_invalid id then Int.add inner_acc id else inner_acc)
          0
          (List.init (finish - start + 1) (fun i -> start + i))
      in
      Int.add acc range_sum)
    0 ranges

let is_invalid_part2 id =
  let s = string_of_int id in
  let len = String.length s in
  let rec check repeat_len =
    if repeat_len >= len then false
    else if len mod repeat_len <> 0 then check (repeat_len + 1)
    else
      let unit = String.sub s 0 repeat_len in
      let repeats = len / repeat_len in
      if repeats < 2 then check (repeat_len + 1)
      else
        let rec verify i =
          if i >= repeats then true
          else if String.sub s (i * repeat_len) repeat_len <> unit then false
          else verify (i + 1)
        in
        if verify 1 then true else check (repeat_len + 1)
  in
  check 1

(** [part2 input] solves part 2 of the challenge

    @param input Raw input string from the puzzle
    @return Solution for part 2 *)
let part2 input =
  let ranges = parse input in
  List.fold_left
    (fun acc (start, finish) ->
      let range_sum =
        List.fold_left
          (fun inner_acc id ->
            if is_invalid_part2 id then Int64.add inner_acc (Int64.of_int id)
            else inner_acc)
          0L
          (List.init (finish - start + 1) (fun i -> start + i))
      in
      Int64.add acc range_sum)
    0L ranges

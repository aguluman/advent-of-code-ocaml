(** Day 05: Cafeteria

    [Brief description of the problem and what this module solves]

    {2 Problem Summary:}
    - {b Part 1:} [Description of part 1]
    - {b Part 2:} [Description of part 2]

    See details at:
    {{:https://adventofcode.com/2025/day/5} Advent of Code 2025, Day 05} *)

(** Helper function to parse input string into a structured data format

    @param input Raw input string from the puzzle
    @return Parsed data structure *)
let parse input =
  let lines = String.split_on_char '\n' input in
  let rec split_on_blank acc = function
    | [] -> (List.rev acc, [])
    | "" :: rest -> (List.rev acc, rest)
    | line :: rest -> split_on_blank (line :: acc) rest
  in
  let range_lines, query_lines = split_on_blank [] lines in
  let parse_range line =
    let line = String.trim line in
    match String.split_on_char '-' line with
    | [ s; e ] -> (Int64.of_string s, Int64.of_string e)
    | _ -> failwith ("Invalid range: " ^ line)
  in
  let ranges =
    List.filter_map
      (fun s ->
        let s = String.trim s in
        if s = "" then None else Some (parse_range s))
      range_lines
  in
  let queries =
    List.filter_map
      (fun s ->
        let s = String.trim s in
        if s = "" then None else Some (Int64.of_string s))
      query_lines
  in
  (ranges, queries)

(** Merge overlapping intervals after sorting by start *)
let merge_intervals intervals =
  let sorted =
    List.sort (fun (s1, _) (s2, _) -> Int64.compare s1 s2) intervals
  in
  match sorted with
  | [] -> []
  | first :: rest ->
      let merged, last =
        List.fold_left
          (fun (acc, (curr_s, curr_e)) (s, e) ->
            if Int64.compare s (Int64.succ curr_e) <= 0 then
              (acc, (curr_s, Int64.max curr_e e))
            else ((curr_s, curr_e) :: acc, (s, e)))
          ([], first) rest
      in
      List.rev (last :: merged)

(** Binary search to check if point is in any merged interval array *)
let is_fresh arr n point =
  if n = 0 then false
  else
    let rec bsearch lo hi =
      if lo > hi then false
      else
        let mid = (lo + hi) / 2 in
        let s, e = arr.(mid) in
        let cmp_s = Int64.compare point s in
        if cmp_s < 0 then bsearch lo (mid - 1)
        else if Int64.compare point e > 0 then bsearch (mid + 1) hi
        else true
    in
    bsearch 0 (n - 1)

(** [part1 input] solves part 1 of the challenge

    @param input Raw input string from the puzzle
    @return Count of fresh ingredient IDs *)
let part1 input =
  let ranges, queries = parse input in
  let merged = merge_intervals ranges in
  let arr = Array.of_list merged in
  let n = Array.length arr in
  List.fold_left
    (fun count id -> if is_fresh arr n id then count + 1 else count)
    0 queries

(** [part2 input] solves part 2 of the challenge

    @param input Raw input string from the puzzle
    @return Total number of fresh ingredient IDs covered by the ranges *)
let part2 input =
  let ranges, _ = parse input in
  let merged = merge_intervals ranges in
  List.fold_left
    (fun total (s, e) -> Int64.add total (Int64.add (Int64.sub e s) 1L))
    0L merged

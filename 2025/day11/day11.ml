(** Day 11: Reactor

    Count all distinct paths through a network of devices from 'you' to 'out'.
    The input describes a directed graph where each device lists its outputs.

    {2 Problem Summary:}
    - {b Part 1:} Count all paths from 'you' to 'out'
    - {b Part 2:} [Description of part 2]

    See details at:
    {{:https://adventofcode.com/2025/day/11} Advent of Code 2025, Day 11} *)

module StringMap = Map.Make (String)

(** Helper function to parse input string into an adjacency list

    @param input Raw input string from the puzzle
    @return Map from device name to list of connected devices *)
let parse input =
  input |> String.split_on_char '\n'
  |> List.filter (fun line -> String.trim line <> "")
  |> List.fold_left
       (fun acc line ->
         match String.split_on_char ':' line with
         | [ src; dests ] ->
             let src = String.trim src in
             let dests =
               dests |> String.trim |> String.split_on_char ' '
               |> List.filter (fun s -> s <> "")
             in
             StringMap.add src dests acc
         | _ -> acc)
       StringMap.empty

(** [count_paths graph start target] counts all paths from start to target

    Uses memoization to avoid recomputing paths from the same node *)
let count_paths graph start target =
  let memo = Hashtbl.create 256 in
  let rec dfs node =
    if node = target then 1
    else
      match Hashtbl.find_opt memo node with
      | Some count -> count
      | None ->
          let neighbors =
            match StringMap.find_opt node graph with
            | Some lst -> lst
            | None -> []
          in
          let count =
            List.fold_left (fun acc neighbor -> acc + dfs neighbor) 0 neighbors
          in
          Hashtbl.add memo node count;
          count
  in
  dfs start

(** [part1 input] solves part 1 of the challenge

    @param input Raw input string from the puzzle
    @return Number of distinct paths from 'you' to 'out' *)
let part1 input =
  let graph = parse input in
  count_paths graph "you" "out"

(** [part2 input] solves part 2 of the challenge

    @param input Raw input string from the puzzle
    @return Solution for part 2 *)
let part2 input =
  let _ = parse input in
  (* TODO: Implement part 2 solution *)
  0L

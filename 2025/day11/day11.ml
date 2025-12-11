(** Day 11: Reactor

    Count all distinct paths through a network of devices from start to end. The
    input describes a directed graph where each device lists its outputs.

    {2 Problem Summary:}
    - {b Part 1:} Count all paths from 'you' to 'out'
    - {b Part 2:} Count all paths from 'svr' to 'out' that visit both 'dac' and
      'fft'

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

(** [count_paths_through graph start target required] counts paths that visit
    all required nodes

    Uses state-based memoization where state tracks which required nodes have
    been visited. State is a bitmask: bit i is set if required.(i) has been
    visited.

    @param graph Adjacency list
    @param start Starting node
    @param target Ending node
    @param required Array of nodes that must be visited *)
let count_paths_through graph start target required =
  let n = Array.length required in
  let full_mask = (1 lsl n) - 1 in
  (* memo: (node, visited_mask) -> count *)
  let memo = Hashtbl.create 1024 in

  (* Get mask bit for a node if it's required *)
  let node_mask node =
    let rec find i =
      if i >= n then 0 else if required.(i) = node then 1 lsl i else find (i + 1)
    in
    find 0
  in

  let rec dfs node visited =
    (* Update visited mask if current node is required *)
    let visited = visited lor node_mask node in
    if node = target then
      (* Only count if all required nodes were visited *)
      if visited = full_mask then 1L else 0L
    else
      let key = (node, visited) in
      match Hashtbl.find_opt memo key with
      | Some count -> count
      | None ->
          let neighbors =
            match StringMap.find_opt node graph with
            | Some lst -> lst
            | None -> []
          in
          let count =
            List.fold_left
              (fun acc neighbor -> Int64.add acc (dfs neighbor visited))
              0L neighbors
          in
          Hashtbl.add memo key count;
          count
  in
  dfs start 0

(** [part1 input] solves part 1 of the challenge

    @param input Raw input string from the puzzle
    @return Number of distinct paths from 'you' to 'out' *)
let part1 input =
  let graph = parse input in
  count_paths graph "you" "out"

(** [part2 input] solves part 2 of the challenge

    @param input Raw input string from the puzzle
    @return Number of paths from 'svr' to 'out' visiting both 'dac' and 'fft' *)
let part2 input =
  let graph = parse input in
  count_paths_through graph "svr" "out" [| "dac"; "fft" |]

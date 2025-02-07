(** Custom Set module for storing unique coordinates (x,y points) *)
module CoordSet = Set.Make(struct
  type t = int * int  (* Represents a point as (x,y) coordinate *)
  let compare = compare
end)

(** Custom Set module for storing unique paths (sequences of coordinates) *)
module PathSet = Set.Make(struct
  type t = (int * int) list  (* Represents a path as list of coordinates *)
  let compare = compare
end)

(** Helper function to create a list of integers from a to b inclusive *)
let rec range a b = 
  if a > b then [] 
  else a :: range (a+1) b

(** Part 1: Count paths from 0 to 9 in the height map
    @param map The 2D array representing height values
    @return Number of valid paths found *)
let part1 map =
  let n = Array.length map in
  (* Ensure map is square *)
  Array.iter (fun row -> assert (Array.length row = n)) map;

  (* Depth-first search to find all reachable coordinates from a starting point
     that increase in height by exactly 1 at each step *)
  let rec dfs (i, j) =
    List.fold_left
      (fun acc (di, dj) ->
        let ni = i + di in  (* next i coordinate *)
        let nj = j + dj in  (* next j coordinate *)
        (* Check if next position is valid and exactly 1 higher *)
        if 0 <= ni && ni < n && 0 <= nj && nj < n && map.(i).(j) + 1 = map.(ni).(nj) then
          CoordSet.union acc (dfs (ni, nj))
        else acc)
      (CoordSet.singleton (i, j))
      [(-1, 0); (0, -1); (1, 0); (0, 1)]  (* Four possible directions *)
  in
  
  (* Generate all coordinates in the map *)
  let coords = List.concat (List.map (fun i -> List.map (fun j -> (i,j)) (range 0 (n-1))) (range 0 (n-1))) in
  
  (* For each starting point (height 0), count paths reaching height 9 *)
  List.fold_left
    (fun acc (i, j) ->
      if map.(i).(j) = 0 then
        acc + (CoordSet.filter 
        (fun (i, j) -> map.(i).(j) = 9) 
        (dfs (i, j)) 
        |> CoordSet.cardinal)
      else acc)
    0
    coords

(** Part 2: Count all possible paths from height 0 to 9
    Similar to part1 but tracks entire paths instead of just endpoints *)
let part2 map =
  let n = Array.length map in
  Array.iter (fun row -> assert (Array.length row = n)) map;

  (* DFS that maintains complete paths instead of just visited coordinates *)
  let rec dfs (i, j) =
    List.fold_left
      (fun acc (di, dj) ->
        let ni = i + di in
        let nj = j + dj in
        if 0 <= ni && ni < n && 0 <= nj && nj < n && map.(i).(j) + 1 = map.(ni).(nj) then
          (* Add new coordinate to all existing paths *)
          let new_paths = 
            PathSet.map (fun path -> (ni, nj) :: path) (dfs (ni, nj)) in
          PathSet.union acc new_paths
        else acc)
      (PathSet.singleton [(i, j)])  (* Start with single-point path *)
      [(-1, 0); (0, -1); (1, 0); (0, 1)]
  in

  (* Generate all coordinates *)
  let coords = List.concat (List.map (fun i -> List.map (fun j -> (i,j)) (range 0 (n-1))) (range 0 (n-1))) in
  
  (* Count paths from height 0 that reach height 9 *)
  List.fold_left
    (fun acc (i, j) ->
      if map.(i).(j) = 0 then
        acc + (PathSet.filter 
          (fun path -> List.exists (fun (i, j) -> map.(i).(j) = 9) path)
          (dfs (i, j))
          |> PathSet.cardinal)
      else acc)
    0
    coords

(** Parse input string into 2D array of integers
    @param input String containing newline-separated rows of single digits *)
let parse input =
  let lines = String.split_on_char '\n' input in
  Array.of_list (
    List.map
      (fun line ->
        Array.of_list (
          List.map
            (fun c -> int_of_char c - int_of_char '0')
            (List.init (String.length (String.trim line)) (String.get (String.trim line)))))
      lines)

(** Main execution *)
let () =
  (* Read input from stdin *)
  let input = In_channel.input_all In_channel.stdin |> String.trim in
  let map = parse input in

  (* Time the execution *)
  let timer_start = Unix.gettimeofday () in

  map |> part1 |> Printf.printf "Part 1: %d\n";
  map |> part2 |> Printf.printf "Part 2: %d\n";

  let timer_end = Unix.gettimeofday () in
  let elapsed = timer_end -. timer_start in
  Printf.printf "Elapsed time: %.4f seconds\n" (elapsed)
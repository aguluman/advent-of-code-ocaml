(** Custom Set module for storing unique coordinates [(x,y) points] *)
module CoordSet = Set.Make (struct
  type t = int * int (* Represents a point as (x,y) coordinate *)

  let compare = compare
end)

(** Custom Set module for storing unique paths [(sequences of coordinates)] *)
module PathSet = Set.Make (struct
  type t = (int * int) list (* Represents a path as list of coordinates *)

  let compare = compare
end)

(** Helper function to create a list of integers from start to end inclusive *)
let rec range start end_val =
  if start > end_val then [] else start :: range (start + 1) end_val

(** Part 1: Count paths from height 0 to height 9 in the height map
    @param height_map The 2D array representing height values
    @return Number of valid paths(endpoints) found *)
let part1 height_map =
  let rows = Array.length height_map in
  let cols = if rows > 0 then Array.length height_map.(0) else 0 in
  (* Ensure all rows have the same number of columns *)
  height_map |> Array.iter (fun row -> assert (Array.length row = cols));

  (* Depth-first search to find all reachable coordinates from a starting point
     that increase in height by exactly 1 at each step *)
  let rec depth_first_search (row, col) =
    (row, col) |> CoordSet.singleton (* Creating a set with one coordinate *)
    |> fun visited ->
    [ (-1, 0); (0, -1); (1, 0); (0, 1) ] (* Four possible directions *)
    |> List.fold_left
         (fun visited_coords (delta_row, delta_col) ->
           let next_row = row + delta_row in
           let next_col = col + delta_col in

           (* Check if next position is valid and exactly 1 higher *)
           if
             0 <= next_row && next_row < rows && 0 <= next_col
             && next_col < cols
             && height_map.(row).(col) + 1 = height_map.(next_row).(next_col)
           then
             visited_coords
             (* Combining sets *)
             |> CoordSet.union (depth_first_search (next_row, next_col))
           else visited_coords)
         visited
  in

  (* Generate all coordinates in the height_map *)
  range 0 (rows - 1) (* Create row indices *)
  |> List.map (fun row ->
      (* For each row *)
      range 0 (cols - 1) (* Create column indices *)
      |> List.map (fun col -> (row, col))) (* Make coordinates *)
  |> List.concat (* Flatten the result *)
  |> List.fold_left (* Count paths from height 0 reaching height 9 *)
       (fun path_count (row, col) ->
         if height_map.(row).(col) = 0 then
           depth_first_search (row, col)
           |> CoordSet.filter (fun (r, c) -> height_map.(r).(c) = 9)
           (* Counting elements *)
           |> CoordSet.cardinal
           |> ( + ) path_count
         else path_count)
       0

(** Part 2: Count all possible paths from height 0 to 9 in the height map
    @param height_map
      A 2D array of integers representing elevation at each point
    @return
      Total number of unique paths that start at height 0 and end at height 9 *)
let part2 height_map =
  let rows = Array.length height_map in
  let cols = if rows > 0 then Array.length height_map.(0) else 0 in
  height_map |> Array.iter (fun row -> assert (Array.length row = cols));

  (* DFS that maintains complete paths instead of just visited coordinates *)
  let rec depth_first_search (row, col) =
    [ (row, col) ] |> PathSet.singleton (* Creating a set with one path *)
    |> fun path_set ->
    [ (-1, 0); (0, -1); (1, 0); (0, 1) ]
    |> List.fold_left
         (fun paths (delta_row, delta_col) ->
           let next_row = row + delta_row in
           let next_col = col + delta_col in
           if
             0 <= next_row && next_row < rows && 0 <= next_col
             && next_col < cols
             && height_map.(row).(col) + 1 = height_map.(next_row).(next_col)
           then
             depth_first_search (next_row, next_col)
             |> PathSet.map (fun current_path ->
                 (next_row, next_col) :: current_path)
             (* Combining sets *)
             |> PathSet.union paths
           else paths)
         path_set
  in

  (* Generate all coordinates and count paths *)
  range 0 (rows - 1)
  |> List.map (fun row ->
      range 0 (cols - 1) |> List.map (fun col -> (row, col)))
  |> List.concat
  |> List.fold_left
       (fun path_count (row, col) ->
         if height_map.(row).(col) = 0 then
           depth_first_search (row, col)
           |> PathSet.filter (fun path ->
               List.exists (fun (r, c) -> height_map.(r).(c) = 9) path)
           (* Counting elements *)
           |> PathSet.cardinal
           |> ( + ) path_count
         else path_count)
       0

(** Parse input string into 2D array of integers
    @param input String containing newline-separated rows of single digits *)
let parse input =
  input
  |> String.split_on_char '\n' (* Split into lines *)
  |> List.map (fun line ->
      (* For each line *)
      String.trim line |> String.to_seq (* Convert string to sequence *)
      |> List.of_seq (* Convert sequence to list *)
      |> List.map (fun c -> int_of_char c - int_of_char '0')
        (* Convert to ints *)
      |> Array.of_list) (* Convert to array *)
  |> Array.of_list (* Convert outer list to array *)

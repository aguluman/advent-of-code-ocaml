(** Day 18: RAM Run
    This module implements a solution to navigate through a corrupted memory space
    where bytes are falling and blocking paths. The challenge involves finding the
    shortest path through a grid and determining the first byte that makes the exit
    unreachable.
    
    The memory space is represented as a grid where:
    - (0,0) is the starting position (top-left corner)
    - (n,n) is the exit position (bottom-right corner)
    - Corrupted memory positions cannot be entered
    
    @see <https://adventofcode.com/2024/day/18> Problem Description
*)


(** [IntPair] defines a comparable pair of integers for use in maps and sets. *)
module IntPair = struct
  type t = int * int
  let compare = compare
end



(** [PairMap] provides map operations for integer coordinate pairs. *)
module PairMap = Map.Make(IntPair)



(** [create_memory_grid grid_size positions] creates a grid representation of memory space.
    The grid is a boolean matrix where true indicates corrupted memory (bytes that
    have fallen) and false indicates safe memory locations.
    
    @param grid_size Size of the grid (grid_size+1 x grid_size+1)
    @param positions List of (x,y) coordinates where bytes have fallen
    @return A boolean matrix where true indicates corrupted memory
*)
let create_memory_grid grid_size positions =
  let grid = Array.make_matrix (grid_size+1) (grid_size+1) false in
  List.iter (fun (pos_x, pos_y) -> grid.(pos_y).(pos_x) <- true) positions;
  grid



(** [find_shortest_path grid_size positions] finds the shortest path from (0,0) to (grid_size,grid_size)
    through a memory space with corrupted positions.
    
    The function uses Breadth-First Search to find the shortest path:
    1. Start at position (0,0) with distance 0
    2. Explore adjacent positions in all four directions
    3. For each reachable uncorrupted position, record distance
    4. Continue until destination is reached or all reachable positions are explored
    
    @param grid_size Size of the memory grid (grid_size+1 x grid_size+1)
    @param positions List of (x,y) coordinates of corrupted memory
    @return Some distance if a path exists, None if destination is unreachable
    @raise Invalid_argument if grid parameters are invalid
*)
let find_shortest_path grid_size positions =
  let grid = create_memory_grid grid_size positions in
  
  (* Using a queue for BFS efficiency *)
  let queue = Queue.create () in
  Queue.add (0, 0) queue;
  
  let distance = ref (PairMap.singleton (0, 0) 0) in
  
  let directions = [(-1, 0); (0, -1); (1, 0); (0, 1)] in
  
  (* BFS implementation with tail recursion *)
  let rec process_queue () =
    if Queue.is_empty queue then
      !distance
    else
      let (pos_x, pos_y) = Queue.take queue in
      let current_dist = PairMap.find (pos_x, pos_y) !distance in
      
      List.iter (fun (dir_x, dir_y) ->
        let next_x, next_y = pos_x + dir_x, pos_y + dir_y in
        if next_x >= 0 && next_x <= grid_size && next_y >= 0 && next_y <= grid_size &&
           not grid.(next_y).(next_x) &&
           not (PairMap.mem (next_x, next_y) !distance)
        then begin
          distance := PairMap.add (next_x, next_y) (current_dist + 1) !distance;
          Queue.add (next_x, next_y) queue
        end
      ) directions;
      
      process_queue ()
  in
  
  let final_distance = process_queue () in
  PairMap.find_opt (grid_size, grid_size) final_distance



(** [calculate_min_steps grid_size bytes positions] determines the minimum steps needed
    to reach the exit after a specific number of bytes have fallen.
    
    @param grid_size Size of the memory grid (grid_size+1 x grid_size+1)
    @param bytes Number of bytes to consider from the sequence
    @param positions Sequence of (x,y) coordinates where bytes will fall
    @return The minimum number of steps to reach the exit
    @raise Failure if no path to the exit exists
*)
let calculate_min_steps (grid_size, bytes, positions) =
  let positions_list = 
    positions
    |> Seq.take bytes
    |> List.of_seq
  in
  match find_shortest_path grid_size positions_list with
  | Some value -> value
  | None -> failwith "No path to exit exists"



(** [find_blocking_byte grid_size positions] finds the first byte that makes the exit
    unreachable from the starting position.
    
    The function uses binary search to efficiently find the precise byte:
    1. Start with the full list of positions
    2. Use binary search to narrow down the critical position
    3. When found, return the exact byte coordinates
    
    @param grid_size Size of the memory grid (grid_size+1 x grid_size+1)
    @param positions Sequence of (x,y) coordinates where bytes will fall
    @return The (x,y) coordinates of the first byte that blocks all paths
*)
let find_blocking_byte (grid_size, positions) =
  let positions_list = List.of_seq positions in
  
  (* Using Array.sub for more efficient slicing *)
  let positions_array = Array.of_list positions_list in
  
  let take count =
    if count > Array.length positions_array then positions_list
    else Array.sub positions_array 0 count |> Array.to_list
  in
  
  (* Binary search with early exit optimization *)
  let rec bisect lower_bound upper_bound =
    if lower_bound + 1 = upper_bound then 
      upper_bound
    else 
      let middle_index = (lower_bound + upper_bound) / 2 in
      
      match find_shortest_path grid_size (take (middle_index + 1)) with
      | Some _ -> 
          (* Path exists, try with more bytes *)
          bisect middle_index upper_bound
      | None -> 
          (* No path exists with middle_index+1 bytes *)
          match find_shortest_path grid_size (take middle_index) with
          | Some _ -> 
              (* Found exact cutoff point *)
              middle_index
          | None ->
              (* Keep searching in lower half *)
              bisect lower_bound middle_index
  in
  
  let blocking_index = bisect 0 (List.length positions_list) in
  List.nth positions_list blocking_index



(** [parse_byte_positions input] parses the input string into a sequence of byte positions.
    
    Expected format:
    - Each line contains "x,y" coordinates
    - Coordinates are comma-separated integers
    
    @param input String containing byte position data
    @return Sequence of (x,y) coordinates
    @raise Failure if input format is invalid
*)

(* Helper function to safely convert string to int *)
let safe_int_of_string s =
  try Some (int_of_string s)
  with Failure _ -> None

(* Function to preprocess and clean input lines *)
let preprocess_lines lines =
  lines
  |> List.map String.trim  (* Remove unnecessary spaces *)
  |> List.filter (fun s -> String.length s > 0)  (* Remove empty lines *)

(* Function to parse byte positions with robust error handling *)
let parse_byte_positions input =
  let lines = input |> String.split_on_char '\n' |> preprocess_lines in
  lines
  |> List.filter_map (fun line ->
         let parts = String.split_on_char ',' line in
         match parts with
         | x_str :: y_str :: _ -> (
             match (safe_int_of_string x_str, safe_int_of_string y_str) with
             | Some x, Some y -> Some (x, y)
             | _ ->
                 Printf.printf "Skipping invalid line: %s\n" line;
                 None)
         | _ ->
             Printf.printf "Invalid format: %s\n" line;
             None)
  |> List.to_seq

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



(** [create_memory_grid n positions] creates a grid representation of memory space.
    The grid is a boolean matrix where true indicates corrupted memory (bytes that
    have fallen) and false indicates safe memory locations.
    
    @param n Size of the grid (n+1 x n+1)
    @param positions List of (x,y) coordinates where bytes have fallen
    @return A boolean matrix where true indicates corrupted memory
*)
let create_memory_grid n positions =
  let grid = Array.make_matrix (n+1) (n+1) false in
  List.iter (fun (x, y) -> grid.(y).(x) <- true) positions;
  grid

(** [find_shortest_path n positions] finds the shortest path from (0,0) to (n,n)
    through a memory space with corrupted positions.
    
    The function uses Breadth-First Search to find the shortest path:
    1. Start at position (0,0) with distance 0
    2. Explore adjacent positions in all four directions
    3. For each reachable uncorrupted position, record distance
    4. Continue until destination is reached or all reachable positions are explored
    
    @param n Size of the memory grid (n+1 x n+1)
    @param positions List of (x,y) coordinates of corrupted memory
    @return Some distance if a path exists, None if destination is unreachable
    @raise Invalid_argument if grid parameters are invalid
*)
let find_shortest_path n positions =
  let grid = create_memory_grid n positions in
  
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
      let (x, y) = Queue.take queue in
      let current_dist = PairMap.find (x, y) !distance in
      
      List.iter (fun (dx, dy) ->
        let nx, ny = x + dx, y + dy in
        if nx >= 0 && nx <= n && ny >= 0 && ny <= n &&
           not grid.(ny).(nx) &&
           not (PairMap.mem (nx, ny) !distance)
        then begin
          distance := PairMap.add (nx, ny) (current_dist + 1) !distance;
          Queue.add (nx, ny) queue
        end
      ) directions;
      
      process_queue ()
  in
  
  let final_distance = process_queue () in
  PairMap.find_opt (n, n) final_distance

(** [calculate_min_steps n bytes positions] determines the minimum steps needed
    to reach the exit after a specific number of bytes have fallen.
    
    @param n Size of the memory grid (n+1 x n+1)
    @param bytes Number of bytes to consider from the sequence
    @param positions Sequence of (x,y) coordinates where bytes will fall
    @return The minimum number of steps to reach the exit
    @raise Failure if no path to the exit exists
*)
let calculate_min_steps (n, bytes, positions) =
  let positions_list = 
    positions
    |> Seq.take bytes
    |> List.of_seq
  in
  match find_shortest_path n positions_list with
  | Some value -> value
  | None -> failwith "No path to exit exists"

(** [find_blocking_byte n positions] finds the first byte that makes the exit
    unreachable from the starting position.
    
    The function uses binary search to efficiently find the precise byte:
    1. Start with the full list of positions
    2. Use binary search to narrow down the critical position
    3. When found, return the exact byte coordinates
    
    @param n Size of the memory grid (n+1 x n+1)
    @param positions Sequence of (x,y) coordinates where bytes will fall
    @return The (x,y) coordinates of the first byte that blocks all paths
*)
let find_blocking_byte (n, positions) =
  let positions_list = List.of_seq positions in
  
  (* Using Array.sub for more efficient slicing *)
  let positions_array = Array.of_list positions_list in
  
  let take count =
    if count > Array.length positions_array then positions_list
    else Array.sub positions_array 0 count |> Array.to_list
  in
  
  (* Binary search with early exit optimization *)
  let rec bisect ok ng =
    if ok + 1 = ng then 
      ng
    else 
      let mid = (ok + ng) / 2 in
      
      match find_shortest_path n (take (mid + 1)) with
      | Some _ -> 
          (* Path exists, try with more bytes *)
          bisect mid ng
      | None -> 
          (* No path exists with mid+1 bytes *)
          match find_shortest_path n (take mid) with
          | Some _ -> 
              (* Found exact cutoff point *)
              mid
          | None ->
              (* Keep searching in lower half *)
              bisect ok mid
  in
  
  let i = bisect 0 (List.length positions_list) in
  List.nth positions_list i

(** [parse_byte_positions input] parses the input string into a sequence of byte positions.
    
    Expected format:
    - Each line contains "x,y" coordinates
    - Coordinates are comma-separated integers
    
    @param input String containing byte position data
    @return Sequence of (x,y) coordinates
    @raise Failure if input format is invalid
*)
let parse_byte_positions input =
  input
  |> String.split_on_char '\n'
  |> List.filter (fun s -> String.length s > 0)
  |> List.map (fun line ->
      let parts = String.split_on_char ',' line in
      match parts with
      | x :: y :: _ -> (int_of_string x, int_of_string y)
      | _ -> failwith "Invalid input format: each line should have at least two numbers"
    )
  |> List.to_seq

(** Main execution function that solves both parts of the challenge. *)
let () =
  try
    let input = In_channel.input_all In_channel.stdin |> String.trim in
    let positions = parse_byte_positions input in
    
    let start_time = Unix.gettimeofday () in
    
    let answer1 = calculate_min_steps (70, 1024, positions) in
    Printf.printf "Part 1: %d\n" answer1;
    
    let (x, y) = find_blocking_byte (70, positions) in
    Printf.printf "Part 2: (%d, %d)\n" x y;
    
    Unix.gettimeofday () -. start_time
    |> Printf.printf "Elapsed time: %.8f seconds\n"
    
  with
  | Failure msg -> Printf.printf "Error: %s\n" msg
  | e -> Printf.printf "Unexpected error: %s\n" (Printexc.to_string e)
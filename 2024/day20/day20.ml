(** 
 * Day 20: Maze Optimization Challenge
 *
 * This module implements solutions for finding optimal paths in a maze.
 * The maze is represented as a 2D character array where:
 * - 'S' represents the start position
 * - 'E' represents the end position (goal)
 * - '#' represents walls that cannot be traversed
 * - '.' represents open spaces that can be traversed
 *
 * The challenge involves:
 * - Part 1: Analyzing the impact of removing walls to create shortcuts
 * - Part 2: Examining path inefficiencies compared to Manhattan distances
 *)


open Domainslib

(** [CoordinateHash] provides an efficient hash implementation for (x,y) coordinates *)
module CoordinateHash = Hashtbl.Make(struct
  type t = int * int
  let equal (x1, y1) (x2, y2) = x1 = x2 && y1 = y2
  let hash (x, y) = (x lsl 16) + y
end)


(** [OptimizedArray] provides specialized array operations with better memory layout *)
module OptimizedArray = struct
  type 'a t = 'a array
  external create: int -> 'a -> 'a t = "caml_make_vect"
  external get: 'a t -> int -> 'a = "%array_safe_get"
  external set: 'a t -> int -> 'a -> unit = "%array_safe_set"
  external length: 'a t -> int = "%array_length"
end



(** [calculate_index cols row col] converts 2D coordinates to a 1D array index
    @param cols Number of columns in the grid
    @param row Row index
    @param col Column index
    @return The 1D array index
*)
let calculate_index cols row col = row * cols + col



(** [find_position maze target_char] finds the position of a specific character in the maze
    @param maze The 2D character array representing the maze
    @param target_char The character to find
    @return A tuple (row, col) with the position of the character
    @raise Not_found if the character isn't present in the maze
*)
let find_position maze target_char =
  let rows = Array.length maze in
  let cols = Array.length maze.(0) in
  let position = ref None in
  
  for row = 0 to rows - 1 do
    for col = 0 to cols - 1 do
      if maze.(row).(col) = target_char then
        position := Some (row, col)
    done;
  done;
  
  match !position with
  | Some pos -> pos
  | None -> raise Not_found



(** [breadth_first_search start_pos maze] computes shortest paths from start_pos to all reachable positions
    
    This function implements an optimized BFS algorithm using:
    - A queue for frontier management
    - Flat arrays for distance tracking
    - Bytes array for efficient visited marking
    
    @param start_pos Starting position tuple (row, col)
    @param maze The 2D character array representing the maze
    @return An array mapping each cell to its minimum distance from start_pos (max_int for unreachable cells)
*)
let breadth_first_search (start_row, start_col) maze =
  let module Queue = Queue in
  let frontier = Queue.create () in
  let rows = Array.length maze in
  let cols = Array.length maze.(0) in
  let distances = OptimizedArray.create (rows * cols) max_int in
  let visited = Bytes.make (rows * cols) '\000' in
  
  OptimizedArray.set distances (calculate_index cols start_row start_col) 0;
  Bytes.set visited (calculate_index cols start_row start_col) '\001';
  Queue.push (start_row, start_col) frontier;
  
  while not (Queue.is_empty frontier) do
    let (current_row, current_col) = Queue.pop frontier in
    let current_dist = OptimizedArray.get distances (calculate_index cols current_row current_col) in
    let next_dist = current_dist + 1 in
    
    (* Explore all four cardinal directions *)
    [(-1, 0); (0, -1); (1, 0); (0, 1)]
    |> List.iter (fun (delta_row, delta_col) ->
      let neighbor_row, neighbor_col = current_row + delta_row, current_col + delta_col in
      let neighbor_idx = calculate_index cols neighbor_row neighbor_col in
      
      (* Check bounds, walls, and visited status *)
      if neighbor_row >= 0 && neighbor_row < rows && 
         neighbor_col >= 0 && neighbor_col < cols &&
         maze.(neighbor_row).(neighbor_col) <> '#' && 
         Bytes.get visited neighbor_idx = '\000'
      then begin
        OptimizedArray.set distances neighbor_idx next_dist;
        Bytes.set visited neighbor_idx '\001';
        Queue.push (neighbor_row, neighbor_col) frontier
      end)
  done;
  distances




(** [part1 maze] analyzes which walls, when removed, create shortcuts in the maze
    
    Algorithm:
    - 1. Find start (S) and end (E) positions in the maze
    - 2. Compute the shortest path in the original maze
    - 3. For each candidate wall that could be a shortcut:
      - a. Remove the wall temporarily
      - b. Recompute shortest path
      - c. If path length decreases, record the improvement
    - 4. Return statistics on path improvements sorted by magnitude
    
    @param maze The 2D character array representing the maze
    @return A list of (improvement_value, frequency) pairs sorted by improvement value
*)
let part1 maze =
  (* Identify maze dimensions and key positions *)
  let rows = Array.length maze in
  let cols = Array.length maze.(0) in
  let start_row, start_col = find_position maze 'S' in
  let goal_row, goal_col = find_position maze 'E' in
  let initial_distances = breadth_first_search (start_row, start_col) maze in
  let original_distance = OptimizedArray.get initial_distances (calculate_index cols goal_row goal_col) in
  
  (* Identify candidate walls that might be shortcuts *)
  let walls = ref [] in
  for row = 1 to rows - 2 do
    for col = 1 to cols - 2 do
      if maze.(row).(col) = '#' then
        (* Check if wall has adjacent passages *)
        let has_vertical_passage = maze.(row-1).(col) <> '#' && maze.(row+1).(col) <> '#' in
        let has_horizontal_passage = maze.(row).(col-1) <> '#' && maze.(row).(col+1) <> '#' in
        if has_vertical_passage || has_horizontal_passage then
          walls := (row, col) :: !walls
    done
  done;
  
  (* Set up parallel task pool *)
  let num_domains = try int_of_string Sys.argv.(1) with _ -> 4 in
  let pool = Task.setup_pool ~num_domains:(num_domains - 1) () in
  
  (* Process walls in parallel *)
  let improvement_list = Atomic.make [] in
  
  (* Mutex to protect the shared improvement list *)
  let mutex = Mutex.create () in
  
  (* Run parallel tasks *)
  Task.run pool (fun () ->
    Task.parallel_for pool ~start:0 ~finish:(List.length !walls - 1) ~body:(fun i ->
      let (row, col) = List.nth !walls i in
      let maze_copy = Array.map Array.copy maze in
      
      (* Try removing wall *)
      maze_copy.(row).(col) <- '.';
      let new_distances = breadth_first_search (start_row, start_col) maze_copy in
      let new_distance = OptimizedArray.get new_distances (calculate_index cols goal_row goal_col) in
      
      (* If improvement found, add to list *)
      if new_distance <> max_int && new_distance < original_distance then begin
        let improvement = original_distance - new_distance in
        Mutex.lock mutex;
        Atomic.set improvement_list (improvement :: (Atomic.get improvement_list));
        Mutex.unlock mutex
      end
    )
  );
  
  (* Tear down pool *)
  Task.teardown_pool pool;
  
  (* Count frequency of each improvement value *)
  let module IntMap = Map.Make(Int) in
  let frequency_map = 
    List.fold_left (fun acc improvement ->
      let count = try IntMap.find improvement acc with Not_found -> 0 in
      IntMap.add improvement (count + 1) acc
    ) IntMap.empty (Atomic.get improvement_list)
  in
  
  IntMap.bindings frequency_map |> List.sort compare




(** [part2 maze] analyzes path inefficiency compared to Manhattan distance
    
   - For each pair of reachable points in the maze, this function:
   - 1. Calculates the Manhattan distance between them
   - 2. Compares it with the actual path distance
   - 3. Records positive differences (inefficiencies)
    
    @param maze The 2D character array representing the maze
    @return A list of (inefficiency_value, frequency) pairs sorted by inefficiency value
*)
let part2 maze =
  let start_row, start_col = find_position maze 'S' in
  let distances = breadth_first_search (start_row, start_col) maze in
  let rows = Array.length maze in
  let cols = Array.length maze.(0) in
  
  (* Set up parallel task pool *)
  let num_domains = try int_of_string Sys.argv.(1) with _ -> 4 in
  let pool = Task.setup_pool ~num_domains:(num_domains - 1) () in
  
  (* Shared data structure for results *)
  let inefficiency_list = Atomic.make [] in
  let mutex = Mutex.create () in
  
  (* Chunk the first dimension for parallel processing *)
  Task.run pool (fun () ->
    Task.parallel_for pool ~start:0 ~finish:(rows - 1) ~body:(fun row1 ->
      let local_inefficiencies = ref [] in
      
      for col1 = 0 to cols - 1 do
        let dist1 = OptimizedArray.get distances (calculate_index cols row1 col1) in
        if dist1 <> max_int then begin
          for row2 = 0 to rows - 1 do
            for col2 = 0 to cols - 1 do
              let dist2 = OptimizedArray.get distances (calculate_index cols row2 col2) in
              
              (* Only compare reachable points *)
              if dist2 <> max_int then begin
                let manhattan_dist = abs (row1 - row2) + abs (col1 - col2) in
                
                (* Record positive inefficiencies with Manhattan distance <= 20 *)
                if dist2 - dist1 >= 0 && manhattan_dist <= 20 then
                  local_inefficiencies := (dist2 - dist1 - manhattan_dist) :: !local_inefficiencies
              end
            done
          done
        end
      done;
      
      (* Merge local results with global list *)
      if !local_inefficiencies <> [] then begin
        Mutex.lock mutex;
        Atomic.set inefficiency_list (!local_inefficiencies @ (Atomic.get inefficiency_list));
        Mutex.unlock mutex
      end
    )
  );
  
  (* Tear down pool *)
  Task.teardown_pool pool;
  
  (* Count frequency of each inefficiency value *)
  let module IntMap = Map.Make(Int) in
  let frequency_map = 
    List.fold_left (fun acc inefficiency ->
      let count = try IntMap.find inefficiency acc with Not_found -> 0 in
      IntMap.add inefficiency (count + 1) acc
    ) IntMap.empty (Atomic.get inefficiency_list)
  in
  
  IntMap.bindings frequency_map |> List.sort compare




(** [parse input] parses the input string into a 2D maze representation
    
    @param input Raw input string with maze characters
    @return A 2D char array representing the maze
*)
let parse input =
  input
  |> String.split_on_char '\n'
  |> List.to_seq
  |> Seq.filter (fun line -> String.trim line <> "")
  |> Seq.map (fun row -> Array.of_list (List.init (String.length row) (String.get row)))
  |> Array.of_seq
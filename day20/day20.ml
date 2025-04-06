(**
  Day 20: Maze Optimization - Path Shortening Challenge

  Solves Advent of Code Day 20 challenge about finding optimal paths through a maze.
  The module analyzes potential shortcuts by removing walls and calculates distance improvements.

  Problem details:
  - Input: A 2D maze with '#' as walls, 'S' as start, 'E' as the end, and '.' as open spaces
  - Part 1: Identify which wall removals create shortcuts and calculate their impact
  - Part 2: Analyze path inefficiencies by comparing actual vs. Manhattan distances

  The solutions use breadth-first search with parallelism for performance optimization.

  See {{:https://adventofcode.com/2024/day/20}Advent of Code 2024, Day 20}
*)


(**
 - Finds the coordinates of a specific value in a 2D array.

 - Uses list comprehension to generate all possible coordinates
  and then finds the first occurrence of the target value.

  @param grid The 2D array to search
  @param target The value to find
  @return A tuple (row, col) representing the row and column indices of the value
*)
let find_index_2d grid target =
    let rows = Array.length grid 
  in
    let cols = Array.length grid.(0) 
  in
    let rec loop row col =
      if row = rows then raise Not_found
      else if col = cols then loop (row + 1) 0
      else if grid.(row).(col) = target then (row, col)
      else loop row (col + 1)
    in
    loop 0 0



(**
  Performs an optimized breadth-first search on a 2D maze to find the shortest paths.

  Implements BFS with several performance optimizations:
  - Uses struct tuples to reduce memory allocations
  - Pre-calculates maze bounds to avoid repeated bounds checks
  - Manually unrolls direction loops for better performance
  - Uses a dictionary to track visited cells and their distances

  @param startRow Starting row index
  @param startCol Starting column index
  @param maze 2D character array representing the maze
  @return Dictionary mapping cell coordinates to their shortest distance from start
*)

let bfs_fast start_row start_col maze =
    let queue = Queue.create () 
  in
    Queue.add (start_row, start_col) queue;
    let distances = Hashtbl.create (Array.length maze * Array.length maze.(0)) 
  in
    Hashtbl.add distances (start_row, start_col) 0;

    let max_row = Array.length maze - 1 
  in
    let max_col = Array.length maze.(0) - 1 
  in

    while 
      not (Queue.is_empty queue) 
    do
      let (row, col) = Queue.take queue 
    in
      let distance = Hashtbl.find distances (row, col) 
    in
      let next_distance = distance + 1 
    in

      if 
        row > 0 && maze.(row - 1).(col) <> '#' 
      then
        let next_pos = (row - 1, col) 
    in
        if 
          not (Hashtbl.mem distances next_pos) 
        then (
          Hashtbl.add distances next_pos next_distance;
          Queue.add next_pos queue
        );

      if 
        col > 0 && maze.(row).(col - 1) <> '#' 
      then
        let next_pos = (row, col - 1) 
    in
        if 
          not (Hashtbl.mem distances next_pos) 
        then (
          Hashtbl.add distances next_pos next_distance;
          Queue.add next_pos queue
        );

      if 
        row < max_row && maze.(row + 1).(col) <> '#' 
      then
        let next_pos = (row + 1, col) 
    in
        if 
          not (Hashtbl.mem distances next_pos) 
        then (
          Hashtbl.add distances next_pos next_distance;
          Queue.add next_pos queue
        );

      if 
        col < max_col && maze.(row).(col + 1) <> '#' 
      then
        let next_pos = (row, col + 1) 
    in
        if 
          not (Hashtbl.mem distances next_pos) 
        then (
          Hashtbl.add distances next_pos next_distance;
          Queue.add next_pos queue
        );
    done;

    distances




(**
  Identifies walls that, when removed, create shortcuts and measures their impact.

  Algorithm steps:
  1. Find start (S) and end (E) points in the maze
  2. Compute the shortest path from S to E in the original maze
  3. Identify walls adjacent to the path using BFS
  4. For each wall, remove it and recalculate paths in parallel
  5. Calculate improvement in distance when each wall is removed

  The function returns a list of (improvement, count) tuples, where:
  - improvement: how many steps are saved by removing a wall
  - count: how many different walls provide this same improvement

  @param maze 2D character array representing the maze
  @return List of (improvement, count) tuples sorted by improvement value
*)

let part1 maze =
  let start_row, start_col = find_index_2d maze 'S' in
  let end_row, end_col = find_index_2d maze 'E' in

  let distances_from_start = bfs_fast start_row start_col maze in
  let end_position = (end_row, end_col) in

  if not (Hashtbl.mem distances_from_start end_position) then
    []
  else
    let original_distance = Hashtbl.find distances_from_start end_position in
    let visited = Hashtbl.create (Array.length maze * Array.length maze.(0)) in
    let walls = Hashtbl.create (Array.length maze * Array.length maze.(0)) in

    (* Find walls along the path using BFS *)
    let queue = Queue.create () in
    Queue.add end_position queue;
    Hashtbl.add visited end_position ();

    while not (Queue.is_empty queue) do
      let (row, col) = Queue.take queue in
      [| (-1, 0); (1, 0); (0, -1); (0, 1) |]
      |> Array.iter (fun (delta_row, delta_col) ->
          let neighbor_row, neighbor_col = row + delta_row, col + delta_col in

          if neighbor_row >= 0
             && neighbor_row < Array.length maze
             && neighbor_col >= 0
             && neighbor_col < Array.length maze.(0) then
            let neighbor_pos = (neighbor_row, neighbor_col) in

            if maze.(neighbor_row).(neighbor_col) = '#' then
              if not (Hashtbl.mem walls neighbor_pos) then
                Hashtbl.add walls neighbor_pos ()
            else if not (Hashtbl.mem visited neighbor_pos)
                    && Hashtbl.mem distances_from_start neighbor_pos then
              Hashtbl.add visited neighbor_pos ();
              Queue.add neighbor_pos queue
         )
    done;

    (* Convert walls hashtable to list for parallel processing *)
    let wall_list = Hashtbl.fold (fun pos _ acc -> pos :: acc) walls [] in
    
    (* Use Domain.spawn for parallel processing *)
    let tasks = 
      List.map (fun (wall_row, wall_col) ->
        Domain.spawn (fun () ->
          let maze_with_removed_wall = Array.map Array.copy maze in
          maze_with_removed_wall.(wall_row).(wall_col) <- '.';

          let new_distances = bfs_fast start_row start_col maze_with_removed_wall in

          if Hashtbl.mem new_distances end_position then
            let improvement = original_distance - Hashtbl.find new_distances end_position in
            if improvement > 0 then (improvement, 1) else (0, 0)
          else (0, 0)
        )
      ) wall_list
    in
    
    (* Collect results from all tasks *)
    let results = List.map Domain.join tasks in
    
    (* Group by improvement value *)
    let improvements = Hashtbl.create 50 in
    List.iter (fun (improvement, count) ->
      if improvement > 0 then
        Hashtbl.replace improvements improvement
          (count + (try Hashtbl.find improvements improvement with Not_found -> 0))
    ) results;

    (* Convert improvements to a sorted list *)
    Hashtbl.fold (fun key value acc -> (key, value) :: acc) improvements []
    |> List.sort compare



(**
  Analyzes path inefficiency by comparing actual path distances with Manhattan distances.

  Algorithm steps:
  1. Find start point (S) in the maze
  2. Compute the shortest paths from S to all reachable points
  3. For each pair of points:
      - Calculate Manhattan distance between them
      - Compare with actual path distances
      - Record positive differences (inefficiencies)
  4. Process point pairs in parallel for better performance

  The function returns a list of (inefficiency, count) tuples, where:
  - inefficiency: extra steps in the path compared to Manhattan distance
  - count: how many point pairs have this same inefficiency value

  @param maze 2D character array representing the maze
  @return List of (inefficiency, count) tuples sorted by inefficiency value
*)

let part2 maze =
  let start_row, start_col = find_index_2d maze 'S' in
  let distances = bfs_fast start_row start_col maze in

  let reachable_points = 
    Hashtbl.fold (fun pos _ acc -> pos :: acc) distances []
    |> Array.of_list in
  
  let chunk_size = max 1 (Array.length reachable_points / (Domain.recommended_domain_count() * 2)) in
  
  (* Create tasks in chunks for better performance *)
  let tasks = 
    let rec create_chunks start acc =
      if start >= Array.length reachable_points then acc
      else
        let end_idx = min (start + chunk_size) (Array.length reachable_points) in
        let task = Domain.spawn (fun () ->
          let local_inefficiencies = Hashtbl.create 50 in
          
          for i = start to end_idx - 1 do
            let (row1, col1) = reachable_points.(i) in
            let distance1 = Hashtbl.find distances (row1, col1) in
            
            for j = i + 1 to Array.length reachable_points - 1 do
              let (row2, col2) = reachable_points.(j) in
              let distance2 = Hashtbl.find distances (row2, col2) in
              
              let manhattan_distance = abs (row1 - row2) + abs (col1 - col2) in
              
              if manhattan_distance <= 20 then
                let inefficiency1 = distance2 - distance1 - manhattan_distance in
                if inefficiency1 >= 0 then
                  Hashtbl.replace local_inefficiencies inefficiency1
                    (1 + (try Hashtbl.find local_inefficiencies inefficiency1 with Not_found -> 0));
                
                let inefficiency2 = distance1 - distance2 - manhattan_distance in
                if inefficiency2 >= 0 then
                  Hashtbl.replace local_inefficiencies inefficiency2
                    (1 + (try Hashtbl.find local_inefficiencies inefficiency2 with Not_found -> 0));
            done;
          done;
          local_inefficiencies
        ) in
        create_chunks end_idx (task :: acc)
    in
    create_chunks 0 []
  in
  
  (* Collect and combine results *)
  let inefficiencies = Hashtbl.create 50 in
  List.iter (fun task ->
    let local_results = Domain.join task in
    Hashtbl.iter (fun key value ->
      Hashtbl.replace inefficiencies key
        (value + (try Hashtbl.find inefficiencies key with Not_found -> 0))
    ) local_results;
  ) tasks;
  
  Hashtbl.fold (fun key value acc -> (key, value) :: acc) inefficiencies []
  |> List.sort compare




(**
  Parses the input string into a 2D character array representing the maze.

  Splits the input by newlines and converts each line to a character array,
  ensuring trailing whitespace is removed for consistent maze dimensions.

  @param input Raw input string
  @return 2D character array representing the maze
*)


let parse input =
  input
  |> String.split_on_char '\n'
  |> List.filter (fun s -> String.trim s <> "")
  |> List.map (fun row -> Array.of_list (List.init (String.length row) (String.get row)))
  |> Array.of_list




(** * Day 16: Reindeer Maze * * This module solves the Reindeer Maze challenge.
    The challenge involves finding * the minimum-cost path through a maze where:
    * - Moving forward costs 1 point * - Rotating 90 degrees costs 1000 points *
    * Part 1: Find the minimum possible score to reach the end * Part 2: Count
    tiles that are part of any minimum-score path *)

(** Direction that a reindeer can face in the maze *)
type direction =
  | East
  | North
  | West
  | South

(** Rotate a direction 90 degrees counterclockwise *)
let rotate_counterclockwise direction =
  match direction with
  | East -> North
  | North -> West
  | West -> South
  | South -> East

(** Rotate a direction 90 degrees clockwise *)
let rotate_clockwise direction =
  match direction with
  | East -> South
  | South -> West
  | West -> North
  | North -> East

(** A position in the maze with coordinates and facing direction *)
type position = {
  row : int;
  column : int;
  facing : direction;
}

(** Move forward one step in the current facing direction if possible
    @param pos Current position
    @param maze The maze layout
    @return
      Some new_position if the move is valid, None if blocked by a wall or
      boundary *)
let move_forward pos maze =
  let next_row, next_col =
    match pos.facing with
    | East -> (pos.row, pos.column + 1)
    | North -> (pos.row - 1, pos.column)
    | West -> (pos.row, pos.column - 1)
    | South -> (pos.row + 1, pos.column)
  in

  (* Check if the move is valid: within bounds and not a wall *)
  if
    next_row >= 0
    && next_row < Array.length maze
    && next_col >= 0
    && next_col < Array.length maze.(next_row)
    && maze.(next_row).(next_col) <> '#'
  then Some { pos with row = next_row; column = next_col }
  else None

(** Get positions after rotating in both possible directions
    @param pos Current position
    @return List of positions after rotating clockwise and counterclockwise *)
let rotate_positions pos =
  [
    { pos with facing = rotate_counterclockwise pos.facing };
    { pos with facing = rotate_clockwise pos.facing };
  ]

(** An edge in the movement graph connecting two positions with a cost *)
type edge = {
  from_pos : position;
  to_pos : position;
  cost : int;
}

(** Custom map implementation for positions *)
module PositionMap = Map.Make (struct
  type t = position

  let compare p1 p2 =
    let row_comparison = compare p1.row p2.row in
    if row_comparison <> 0 then row_comparison
    else
      let col_comparison = compare p1.column p2.column in
      if col_comparison <> 0 then col_comparison
      else compare p1.facing p2.facing
end)

(** Group elements by a key function
    @param key_fn Function to extract the key from an element
    @param list List of elements to group
    @return Map from keys to lists of elements with that key *)
let group_by key_fn list =
  List.fold_left
    (fun acc elem ->
      let key = key_fn elem in
      let data = try PositionMap.find key acc with Not_found -> [] in
      PositionMap.add key (elem :: data) acc)
    PositionMap.empty list

(** Priority queue implementation for Dijkstra's algorithm *)
module PriorityQueue = struct
  (** Priority queue with a binary heap implementation *)
  type 'a t = {
    mutable heap : (int * 'a) array;
    mutable size : int;
  }

  (** Create a new priority queue with the given initial capacity *)
  let create capacity =
    { heap = Array.make capacity (max_int, Obj.magic ()); size = 0 }

  (** Helper functions for binary heap operations *)
  let parent i = (i - 1) / 2

  let left i = (2 * i) + 1
  let right i = (2 * i) + 2

  (** Swap two elements in an array *)
  let swap a i j =
    let temp = a.(i) in
    a.(i) <- a.(j);
    a.(j) <- temp

  (** Grow the heap capacity when needed *)
  let grow q =
    let new_heap =
      Array.make (Array.length q.heap * 2) (max_int, Obj.magic ())
    in
    Array.blit q.heap 0 new_heap 0 q.size;
    q.heap <- new_heap

  (** Insert an item with given priority into the queue *)
  let insert q priority item =
    (* Grow if needed *)
    if q.size = Array.length q.heap then grow q;

    (* Add item at the end *)
    q.heap.(q.size) <- (priority, item);

    (* Restore heap property by bubbling up *)
    let rec bubble_up i =
      let p = parent i in
      if i > 0 && fst q.heap.(i) < fst q.heap.(p) then (
        swap q.heap i p;
        bubble_up p)
    in
    bubble_up q.size;
    q.size <- q.size + 1

  (** Extract the minimum priority item from the queue *)
  let extract_min q =
    if q.size = 0 then None
    else
      let min_item = q.heap.(0) in
      q.heap.(0) <- q.heap.(q.size - 1);
      q.size <- q.size - 1;

      (* Restore heap property by pushing down *)
      let rec heapify i =
        let l = left i in
        let r = right i in
        let smallest = ref i in
        if l < q.size && fst q.heap.(l) < fst q.heap.(!smallest) then
          smallest := l;
        if r < q.size && fst q.heap.(r) < fst q.heap.(!smallest) then
          smallest := r;
        if !smallest <> i then (
          swap q.heap i !smallest;
          heapify !smallest)
      in

      if q.size > 0 then heapify 0;
      Some min_item
end

(** Dijkstra's algorithm to find shortest paths from a source position
    @param start Starting position
    @param edges List of all possible moves in the maze
    @return Map from positions to their minimum distance from start *)
let find_shortest_paths start edges =
  (* Group edges by their origin position *)
  let adjacent_edges = group_by (fun e -> e.from_pos) edges in

  (* Track minimum distances *)
  let distances = ref (PositionMap.singleton start 0) in

  (* Priority queue to process positions in order of increasing distance *)
  let queue = PriorityQueue.create 1024 in
  PriorityQueue.insert queue 0 start;

  (* Process positions until priority queue is empty *)
  let rec process_queue () =
    match PriorityQueue.extract_min queue with
    | None -> !distances
    | Some (dist, pos) ->
        (* Skip if we've already found a better path *)
        let current_dist =
          try PositionMap.find pos !distances with Not_found -> max_int
        in
        if dist > current_dist then process_queue ()
        else
          (* Process all outgoing edges from this position *)
          let pos_edges =
            try PositionMap.find pos adjacent_edges with Not_found -> []
          in
          List.iter
            (fun edge ->
              let new_dist = dist + edge.cost in
              let target_pos = edge.to_pos in
              let existing_dist =
                try Some (PositionMap.find target_pos !distances)
                with Not_found -> None
              in
              match existing_dist with
              | Some old_dist when new_dist < old_dist ->
                  distances := PositionMap.add target_pos new_dist !distances;
                  PriorityQueue.insert queue new_dist target_pos
              | None ->
                  distances := PositionMap.add target_pos new_dist !distances;
                  PriorityQueue.insert queue new_dist target_pos
              | _ -> ())
            pos_edges;
          process_queue ()
  in

  process_queue ()

(** Creates pairs of elements from two lists
    @param list1 First list
    @param list2 Second list
    @return List of all possible pairs (one from each list) *)
let all_pairs list1 list2 =
  List.concat_map (fun x -> List.map (fun y -> (x, y)) list2) list1

(** Generate all possible movement edges in the maze
    @param maze The maze layout
    @return List of edges representing all possible moves *)
let generate_movement_edges maze =
  (* Generate coordinates for every position in the maze *)
  all_pairs
    (List.init (Array.length maze) (fun i -> i))
    (List.init (Array.length maze.(0)) (fun j -> j))
  |> List.concat_map (fun (row, col) ->
         (* Skip walls *)
         if maze.(row).(col) = '#' then []
         else
           (* Create edges for each direction at this position *)
           [ East; North; West; South ]
           |> List.concat_map (fun dir ->
                  let pos = { row; column = col; facing = dir } in

                  (* Forward movement edge (cost 1) *)
                  let forward_edge =
                    match move_forward pos maze with
                    | Some next_pos ->
                        [ { from_pos = pos; to_pos = next_pos; cost = 1 } ]
                    | None -> []
                  in

                  (* Rotation edges (cost 1000) *)
                  let rotation_edges =
                    rotate_positions pos
                    |> List.map (fun next_pos ->
                           { from_pos = pos; to_pos = next_pos; cost = 1000 })
                  in

                  forward_edge @ rotation_edges))

(** Find coordinates of a specific character in the maze
    @param maze The maze layout
    @param target_char Character to find
    @return Some (row, col) if found, None otherwise *)
let find_character_position maze target_char =
  let height = Array.length maze in
  if height = 0 then None
  else
    let width = Array.length maze.(0) in
    let rec scan_maze row col =
      if row >= height then None
      else if col >= width then scan_maze (row + 1) 0
      else if maze.(row).(col) = target_char then Some (row, col)
      else scan_maze row (col + 1)
    in
    scan_maze 0 0

(** Solve part 1: Find the minimum score to reach the end
    @param maze The maze layout
    @return Minimum possible score *)
let solve_part1 maze =
  let movement_edges = generate_movement_edges maze in
  (* Find start and end positions *)
  let start_row, start_col =
    match find_character_position maze 'S' with
    | Some pos -> pos
    | None -> failwith "Start position not found"
  in

  let end_row, end_col =
    match find_character_position maze 'E' with
    | Some pos -> pos
    | None -> failwith "End position not found"
  in

  (* Create starting position (facing East as per problem) *)
  let start_pos = { row = start_row; column = start_col; facing = East } in

  (* Calculate shortest paths from start *)
  let distances = find_shortest_paths start_pos movement_edges in
  (* Find minimum distance to end in any direction *)
  [ East; North; West; South ]
  |> List.map (fun dir ->
         let end_pos = { row = end_row; column = end_col; facing = dir } in
         try PositionMap.find end_pos distances with Not_found -> max_int)
  |> List.fold_left min max_int

(** Solve part 2: Count tiles that are part of any minimum-score path
    @param maze The maze layout
    @return Number of tiles on optimal paths *)
let solve_part2 maze =
  let movement_edges = generate_movement_edges maze in

  (* Find start and end positions *)
  let start_row, start_col =
    match find_character_position maze 'S' with
    | Some pos -> pos
    | None -> failwith "Start position not found"
  in

  let end_row, end_col =
    match find_character_position maze 'E' with
    | Some pos -> pos
    | None -> failwith "End position not found"
  in

  (* Create starting position (facing East as per problem) *)
  let start_pos = { row = start_row; column = start_col; facing = East } in

  (* Calculate shortest paths from start *)
  let distances = find_shortest_paths start_pos movement_edges in
  (* Find optimal ending direction and the minimum score *)
  let min_score, best_direction =
    List.fold_left
      (fun (min_dist, min_dir) dir ->
        let end_pos = { row = end_row; column = end_col; facing = dir } in
        let dist =
          try PositionMap.find end_pos distances with Not_found -> max_int
        in
        if dist < min_dist then (dist, dir) else (min_dist, min_dir))
      (max_int, East)
      [ East; North; West; South ]
  in

  (* Create optimal end position with best direction *)
  let optimal_end_pos =
    { row = end_row; column = end_col; facing = best_direction }
  in

  (* Create reversed edges for backward path calculation *)
  let reversed_edges =
    List.map
      (fun edge ->
        { from_pos = edge.to_pos; to_pos = edge.from_pos; cost = edge.cost })
      movement_edges
  in

  (* Calculate paths from end back to start *)
  let reverse_distances = find_shortest_paths optimal_end_pos reversed_edges in
  (* Cache for positions on optimal path to avoid redundant checks *)
  let optimal_path_cache = Hashtbl.create 1024 in
  (* Count tiles on the shortest path *)
  let tile_count = ref 0 in
  for row = 0 to Array.length maze - 1 do
    for col = 0 to Array.length maze.(0) - 1 do
      (* Skip walls *)
      if maze.(row).(col) <> '#' then
        if Hashtbl.mem optimal_path_cache (row, col) then
          (* Already counted this tile *)
          incr tile_count
        else
          (* Check if any direction at this position is on an optimal path *)
          let on_optimal_path = ref false in
          List.iter
            (fun dir ->
              if not !on_optimal_path then
                let pos = { row; column = col; facing = dir } in

                let forward_dist =
                  try Some (PositionMap.find pos distances)
                  with Not_found -> None
                in

                let backward_dist =
                  try Some (PositionMap.find pos reverse_distances)
                  with Not_found -> None
                in

                (* If sum of forward and backward distances equals the minimum score, it's on an optimal path *)
                match (forward_dist, backward_dist) with
                | Some fd, Some bd when fd + bd = min_score ->
                    on_optimal_path := true;
                    Hashtbl.add optimal_path_cache (row, col) true;
                    incr tile_count
                | _ -> ())
            [ East; North; West; South ]
    done
  done;
  !tile_count

(** Parse input string into 2D maze array
    @param input Raw input string
    @return 2D array of characters representing the maze *)
let parse_input input =
  input
  |> Str.split (Str.regexp "[\r\n]+") (* Split on newlines *)
  |> List.map String.trim |> List.map String.to_seq |> List.map Array.of_seq
  |> Array.of_list

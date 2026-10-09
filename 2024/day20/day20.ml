(** * Day 20: Maze Optimization Challenge * * This module implements solutions
    for finding optimal paths in a maze. * The maze is represented as a 2D
    character array where: * - 'S' represents the start position * - 'E'
    represents the end position (goal) * - '#' represents walls that cannot be
    traversed * - '.' represents open spaces that can be traversed * * The
    challenge involves: * - Part 1: Analyzing the impact of removing walls to
    create shortcuts * - Part 2: Examining path inefficiencies compared to
    Manhattan distances *)

open Domainslib

(** [CoordinateHash] provides an efficient hash implementation for (x,y)
    coordinates *)
module CoordinateHash = Hashtbl.Make (struct
  type t = int * int

  let equal (x1, y1) (x2, y2) = x1 = x2 && y1 = y2
  let hash (x, y) = (x lsl 16) + y
end)

(** [OptimizedArray] provides specialized array operations with better memory
    layout *)
module OptimizedArray = struct
  type 'a t = 'a array

  external create : int -> 'a -> 'a t = "caml_make_vect"
  external get : 'a t -> int -> 'a = "%array_safe_get"
  external set : 'a t -> int -> 'a -> unit = "%array_safe_set"
  external length : 'a t -> int = "%array_length"
end

(** [calculate_index cols row col] converts 2D coordinates to a 1D array index
    @param cols Number of columns in the grid
    @param row Row index
    @param col Column index
    @return The 1D array index *)
let calculate_index cols row col = (row * cols) + col

(** [find_position maze target_char] finds the position of a specific character
    in the maze
    @param maze The 2D character array representing the maze
    @param target_char The character to find
    @return A tuple (row, col) with the position of the character
    @raise Not_found if the character isn't present in the maze *)
let find_position maze target_char =
  let rows = Array.length maze in
  let cols = Array.length maze.(0) in
  let position = ref None in

  for row = 0 to rows - 1 do
    for col = 0 to cols - 1 do
      if maze.(row).(col) = target_char then position := Some (row, col)
    done
  done;

  match !position with
  | Some pos -> pos
  | None -> raise Not_found

(** [breadth_first_search start_pos maze] computes shortest paths from start_pos
    to all reachable positions

    This function implements an optimized BFS algorithm using:
    - A queue for frontier management
    - Flat arrays for distance tracking
    - Bytes array for efficient visited marking

    @param start_pos Starting position tuple (row, col)
    @param maze The 2D character array representing the maze
    @return
      An array mapping each cell to its minimum distance from start_pos (max_int
      for unreachable cells) *)
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
    let current_row, current_col = Queue.pop frontier in
    let current_dist =
      OptimizedArray.get distances
        (calculate_index cols current_row current_col)
    in
    let next_dist = current_dist + 1 in

    (* Explore all four cardinal directions *)
    [ (-1, 0); (0, -1); (1, 0); (0, 1) ]
    |> List.iter (fun (delta_row, delta_col) ->
        let neighbor_row, neighbor_col =
          (current_row + delta_row, current_col + delta_col)
        in
        let neighbor_idx = calculate_index cols neighbor_row neighbor_col in

        (* Check bounds, walls, and visited status *)
        if
          neighbor_row >= 0 && neighbor_row < rows && neighbor_col >= 0
          && neighbor_col < cols
          && maze.(neighbor_row).(neighbor_col) <> '#'
          && Bytes.get visited neighbor_idx = '\000'
        then (
          OptimizedArray.set distances neighbor_idx next_dist;
          Bytes.set visited neighbor_idx '\001';
          Queue.push (neighbor_row, neighbor_col) frontier))
  done;
  distances

(** [with_pool f] runs [f pool] on a Domainslib pool sized to this machine, and
    always tears the pool down, even if [f] raises. The domain calling
    [Task.run] also works, so the pool gets one fewer than the recommended
    count. *)
let with_pool f =
  let num_domains = Domain.recommended_domain_count () - 1 in
  let pool = Task.setup_pool ~num_domains () in
  Fun.protect
    ~finally:(fun () -> Task.teardown_pool pool)
    (fun () -> Task.run pool (fun () -> f pool))

(** [add_count counts key] increments [key]'s count in [counts]. *)
let add_count counts key =
  let count = Option.value (Hashtbl.find_opt counts key) ~default:0 in
  Hashtbl.replace counts key (count + 1)

(** [sorted_counts counts] is [counts] as a list of (value, count) pairs, sorted
    by value. *)
let sorted_counts counts =
  Hashtbl.fold (fun key count acc -> (key, count) :: acc) counts []
  |> List.sort compare

(** [part1 maze] analyzes which walls, when removed, create shortcuts in the
    maze.

    Removing wall [w] creates exactly one new kind of route: S to a neighbour
    [a] of [w], through [w], to another neighbour [b], then on to E. Its length
    is [dS(a) + 2 + dE(b)], where [dS] and [dE] are the distances from S and
    from E in the original maze. (A shortest path visits [w] at most once, so
    the parts before and after it are ordinary shortest paths.) So two BFS runs
    give every wall's effect, instead of one BFS per wall.

    @param maze The 2D character array representing the maze
    @return
      A list of (improvement_value, frequency) pairs sorted by improvement value
*)
let part1 maze =
  let rows = Array.length maze in
  let cols = Array.length maze.(0) in
  let start = find_position maze 'S' in
  let goal = find_position maze 'E' in
  let from_start = breadth_first_search start maze in
  let from_goal = breadth_first_search goal maze in
  let distance distances (row, col) =
    OptimizedArray.get distances (calculate_index cols row col)
  in
  let original_distance = distance from_start goal in
  let is_open (row, col) = maze.(row).(col) <> '#' in
  let improvements = Hashtbl.create 64 in
  for row = 1 to rows - 2 do
    for col = 1 to cols - 2 do
      let up = (row - 1, col) and down = (row + 1, col) in
      let left = (row, col - 1) and right = (row, col + 1) in
      (* Candidate walls: open on two opposite sides *)
      if
        maze.(row).(col) = '#'
        && ((is_open up && is_open down) || (is_open left && is_open right))
      then
        let neighbours = List.filter is_open [ up; down; left; right ] in
        let shortest_through_wall =
          List.fold_left
            (fun best a ->
              List.fold_left
                (fun best b ->
                  let to_a = distance from_start a in
                  let from_b = distance from_goal b in
                  if a = b || to_a = max_int || from_b = max_int then best
                  else min best (to_a + 2 + from_b))
                best neighbours)
            max_int neighbours
        in
        if shortest_through_wall < original_distance then
          add_count improvements (original_distance - shortest_through_wall)
    done
  done;
  sorted_counts improvements

(** [part2 maze] analyzes path inefficiency compared to Manhattan distance.

    For each pair of reachable points at most 20 apart (Manhattan distance), it
    records how much shorter the straight jump is than the path. Only the
    diamond of cells within distance 20 of each point is visited, rather than
    the whole grid.

    Rows are processed in parallel. Each row counts into its own table in
    [row_counts], so no two workers share data and no lock is needed; the tables
    are merged once at the end.

    @param maze The 2D character array representing the maze
    @return
      A list of (inefficiency_value, frequency) pairs sorted by inefficiency
      value *)
let part2 maze =
  let max_jump = 20 in
  let start = find_position maze 'S' in
  let distances = breadth_first_search start maze in
  let rows = Array.length maze in
  let cols = Array.length maze.(0) in
  let distance row col =
    OptimizedArray.get distances (calculate_index cols row col)
  in
  let row_counts = Array.make rows (Hashtbl.create 0) in
  with_pool (fun pool ->
      Task.parallel_for pool ~start:0 ~finish:(rows - 1) ~body:(fun row1 ->
          let counts = Hashtbl.create 256 in
          for col1 = 0 to cols - 1 do
            let dist1 = distance row1 col1 in
            if dist1 <> max_int then
              (* The diamond of cells within max_jump of (row1, col1) *)
              let first_row = max 0 (row1 - max_jump) in
              let last_row = min (rows - 1) (row1 + max_jump) in
              for row2 = first_row to last_row do
                let reach = max_jump - abs (row1 - row2) in
                let first_col = max 0 (col1 - reach) in
                let last_col = min (cols - 1) (col1 + reach) in
                for col2 = first_col to last_col do
                  let dist2 = distance row2 col2 in
                  if dist2 <> max_int && dist2 - dist1 >= 0 then
                    let manhattan = abs (row1 - row2) + abs (col1 - col2) in
                    add_count counts (dist2 - dist1 - manhattan)
                done
              done
          done;
          row_counts.(row1) <- counts));
  let totals = Hashtbl.create 256 in
  Array.iter
    (Hashtbl.iter (fun key count ->
         let total = Option.value (Hashtbl.find_opt totals key) ~default:0 in
         Hashtbl.replace totals key (total + count)))
    row_counts;
  sorted_counts totals

(** [parse input] parses the input string into a 2D maze representation

    @param input Raw input string with maze characters
    @return A 2D char array representing the maze *)
let parse input =
  input |> String.split_on_char '\n' |> List.to_seq
  |> Seq.filter (fun line -> String.trim line <> "")
  |> Seq.map (fun row ->
      Array.of_list (List.init (String.length row) (String.get row)))
  |> Array.of_seq

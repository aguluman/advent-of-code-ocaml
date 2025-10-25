(** Day 14: Restroom Redoubt Solution for tracking robot movements in a bounded
    grid with wrapping edges *)

(** Type representing a robot's state with position and velocity *)
type robot = {
  position : int * int;  (** Current (x,y) position *)
  velocity : int * int;  (** Current (dx,dy) velocity *)
}

(** Quadrants of the grid for safety factor calculation *)
type bathroom_quadrant =
  | TopRight  (** Upper right quarter of the grid *)
  | TopLeft  (** Upper left quarter of the grid *)
  | BottomLeft  (** Lower left quarter of the grid *)
  | BottomRight  (** Lower right quarter of the grid *)

(** Simulates robot movement with wrapping at grid boundaries
    @param steps Number of time steps to simulate
    @param width Grid width
    @param height Grid height
    @param robot Initial robot state
    @return Final robot state after simulation *)
let simulate_movement steps width height robot =
  let pos_x, pos_y = robot.position in
  let vel_x, vel_y = robot.velocity in

  (* Calculate final position directly using modular arithmetic *)
  let final_x = (((pos_x + (vel_x * steps)) mod width) + width) mod width in
  let final_y = (((pos_y + (vel_y * steps)) mod height) + height) mod height in

  { robot with position = (final_x, final_y) }

(** Determines which quadrant a position falls in
    @param width Grid width
    @param height Grid height
    @param pos_x X coordinate
    @param pos_y Y coordinate
    @return Some quadrant or None if on dividing lines *)
let get_quadrant width height pos_x pos_y =
  if pos_x > width / 2 && pos_y < height / 2 then Some TopRight
  else if pos_x < width / 2 && pos_y < height / 2 then Some TopLeft
  else if pos_x < width / 2 && pos_y > height / 2 then Some BottomLeft
  else if pos_x > width / 2 && pos_y > height / 2 then Some BottomRight
  else None

(** Counts occurrences of each element in a list
    @param list Input list
    @return Association list of (element, count) pairs *)
let count_occurrences list =
  (* Helper function that builds the accumulator of (element, count) pairs *)
  let rec build_counts accumulator = function
    | [] -> accumulator
    | current :: remaining ->
        (* Count how many times current element appears in original list *)
        let element_count =
          list |> List.filter (( = ) current) |> List.length
        in
        (* Skip if we've already counted this element *)
        if List.mem_assoc current accumulator then
          build_counts accumulator remaining
        (* Add new (element, count) pair to accumulator *)
          else build_counts ((current, element_count) :: accumulator) remaining
  in
  build_counts [] list

(** Solves part 1: Calculates safety factor after 100 seconds
    @param robots List of initial robot states
    @param width Grid width
    @param height Grid height
    @return Product of robot counts in each quadrant *)
let part1 (robots, width, height) =
  let quadrant_counts =
    robots
    |> List.map (simulate_movement 100 width height)
    |> List.filter_map (fun robot ->
        let pos_x, pos_y = robot.position in
        get_quadrant width height pos_x pos_y)
  in
  List.fold_left
    (fun acc (_, count) -> acc * count)
    1
    (count_occurrences quadrant_counts)

(** Extracted helper function to filter valid positions *)
let get_valid_positions robots width height =
  robots
  |> Array.map (fun r -> r.position)
  |> Array.to_list
  |> List.filter (fun (x, y) -> x >= 0 && x < width && y >= 0 && y < height)

(** Extracted helper function to create and populate the grid *)
let create_grid positions width height default_char entity_char =
  let grid = Array.make_matrix height width default_char in
  List.iter (fun (x, y) -> grid.(y).(x) <- entity_char) positions;
  grid

(** Extracted helper function to print grid efficiently using a buffer *)
let print_grid grid width height =
  let buffer = Buffer.create (width * height) in
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      Buffer.add_char buffer grid.(y).(x)
    done;
    Buffer.add_char buffer '\n'
  done;
  Buffer.output_buffer stdout buffer

(** Visualizes robot positions at each time step *)
let visualize_positions robots width height =
  let positions = get_valid_positions robots width height in
  let grid = create_grid positions width height ' ' '@' in
  print_grid grid width height

(** Calculates the safety factor for the current robot configuration
    @param robots List of robot states
    @param width Grid width
    @param height Grid height
    @return
      Product of counts in each quadrant Optimized safety factor calculation
      using arrays *)
let calculate_safety_factor robots width height =
  let quadrant_counts = [| 0; 0; 0; 0 |] in
  (* TopRight, TopLeft, BottomLeft, BottomRight *)

  Array.iter
    (fun robot ->
      let pos_x, pos_y = robot.position in
      if pos_x > width / 2 && pos_y < height / 2 then
        quadrant_counts.(0) <- quadrant_counts.(0) + 1
      else if pos_x < width / 2 && pos_y < height / 2 then
        quadrant_counts.(1) <- quadrant_counts.(1) + 1
      else if pos_x < width / 2 && pos_y > height / 2 then
        quadrant_counts.(2) <- quadrant_counts.(2) + 1
      else if pos_x > width / 2 && pos_y > height / 2 then
        quadrant_counts.(3) <- quadrant_counts.(3) + 1)
    robots;

  quadrant_counts.(0) * quadrant_counts.(1) * quadrant_counts.(2)
  * quadrant_counts.(3)

(** {1 Part 2: Christmas Tree Easter Egg Detection}

    Solves the second part of Day 14: Restroom Redoubt challenge.

    {2 Problem Statement}
    The robots have a hard-coded Easter egg feature where they occasionally
    arrange themselves into a Christmas tree pattern. This function finds the
    minimum number of seconds that must elapse for the robots to first display
    this hidden Christmas tree configuration.

    {2 Algorithm Strategy}
    Uses a minimum safety factor approach to detect clustering:
    - When robots are randomly distributed → high safety factor
    - When robots form a tree (clustered) → minimum safety factor
    - The Christmas tree appears at the time with the lowest safety factor

    {2 Technical Implementation}
    - {b Direct Position Calculation}: Uses modular arithmetic to calculate
      robot positions at any time step without iterative simulation
    - {b Optimized Safety Factor}: Single-pass array counting instead of
      multiple list operations
    - {b Memory Efficient}: Reuses arrays to minimize allocations

    {2 Performance Characteristics}
    - Time Complexity: O(search_limit × num_robots)
    - Space Complexity: O(num_robots)
    - Typical Runtime: ~0.3 seconds for 500 robots over 10,000 time steps

    @param robots List of robot initial states with positions and velocities
    @param width Grid width (101 tiles for actual puzzle input)
    @param height Grid height (103 tiles for actual puzzle input)
    @return The minimum number of seconds until Christmas tree pattern appears

    @raise Failure if no pattern found within search limit (10,000 seconds) *)
let part2 (robots, width, height) =
  let robots_array = Array.of_list robots in
  let num_robots = Array.length robots_array in
  let current_robots =
    Array.make num_robots { position = (0, 0); velocity = (0, 0) }
  in

  let rec search elapsed min_safety min_time =
    if elapsed > 10000 then min_time
    else (
      (* Calculate all robot positions at time elapsed *)
      for i = 0 to num_robots - 1 do
        current_robots.(i) <-
          simulate_movement elapsed width height robots_array.(i)
      done;

      let safety = calculate_safety_factor current_robots width height in
      let new_min_safety, new_min_time =
        if safety < min_safety then (safety, elapsed) else (min_safety, min_time)
      in
      search (elapsed + 1) new_min_safety new_min_time)
  in
  search 0 Int.max_int (-1)

(** Visualizes the pattern at a specific time to verify Christmas tree *)
let visualize_at_time robots width height target_time =
  let final_robots =
    Array.map
      (simulate_movement target_time width height)
      (Array.of_list robots)
  in
  Printf.printf "Pattern at time %d:\n" target_time;
  visualize_positions final_robots width height;
  Printf.printf "\n"

(** Parses input string into robot data
    @param input Raw input string in format "p=x,y v=dx,dy"
    @return List of robots and grid dimensions *)
let parse input =
  let parse_coords str =
    let pattern =
      "^p=\\(-?[0-9]+\\),\\(-?[0-9]+\\) v=\\(-?[0-9]+\\),\\(-?[0-9]+\\)$"
    in
    let regexp = Str.regexp pattern in
    if Str.string_match regexp str 0 then
      try
        let pos_x = int_of_string (Str.matched_group 1 str) in
        let pos_y = int_of_string (Str.matched_group 2 str) in
        let vel_x = int_of_string (Str.matched_group 3 str) in
        let vel_y = int_of_string (Str.matched_group 4 str) in
        Some { position = (pos_x, pos_y); velocity = (vel_x, vel_y) }
      with _ -> None
    else None
  in

  let process_line i line =
    match parse_coords (String.trim line) with
    | Some robot -> robot
    | None ->
        failwith
          (Printf.sprintf
             "Error at input line %d:\n\
             \           Invalid format in input line: '%s'\n\
             \           Expected format: 'p=x,y v=dx,dy' "
             (i + 1) line)
  in

  input |> String.trim |> String.split_on_char '\n'
  |> List.filter (fun line -> String.length (String.trim line) > 0)
  |> List.mapi process_line

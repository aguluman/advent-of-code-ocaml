(** Day 14: Restroom Redoubt
    Solution for tracking robot movements in a bounded grid with wrapping edges *)

(** Type representing a robot's state with position and velocity *)
type robot = {
  position: int * int;  (** Current (x,y) position *)
  velocity: int * int   (** Current (dx,dy) velocity *)
}

(** Quadrants of the grid for safety factor calculation *)
type bathroom_quadrant =
  | TopRight    (** Upper right quarter of the grid *)
  | TopLeft     (** Upper left quarter of the grid *)
  | BottomLeft  (** Lower left quarter of the grid *)
  | BottomRight (** Lower right quarter of the grid *)


  
(** Simulates robot movement with wrapping at grid boundaries
    @param steps Number of time steps to simulate
    @param width Grid width
    @param height Grid height 
    @param robot Initial robot state
    @return Final robot state after simulation *)
let rec simulate_movement steps width height robot =
  if steps = 0 then robot
  else
    let (pos_x, pos_y) = robot.position in
    let (vel_x, vel_y) = robot.velocity in
    
    (* Apply wrapping using modulo arithmetic *)
    let wrapped_position = 
      ((width + pos_x + vel_x) mod width,
       (height + pos_y + vel_y) mod height)
    in
    simulate_movement (steps - 1) width height 
      { robot with position = wrapped_position }




(** Determines which quadrant a position falls in
    @param width Grid width
    @param height Grid height
    @param pos_x X coordinate
    @param pos_y Y coordinate
    @return Some quadrant or None if on dividing lines *)
let get_quadrant width height pos_x pos_y =
  if pos_x > width / 2 && pos_y < height / 2 then
    Some TopRight
  else if pos_x < width / 2 && pos_y < height / 2 then
    Some TopLeft
  else if pos_x < width / 2 && pos_y > height / 2 then
    Some BottomLeft
  else if pos_x > width / 2 && pos_y > height / 2 then
    Some BottomRight
  else None




(** Counts occurrences of each element in a list
    @param lst Input list
    @return Association list of (element, count) pairs *)
let count_occurrences list =
  (* Helper function that builds the accumulator of (element, count) pairs *)
  let rec build_counts accumulator = function
    | [] -> accumulator
    | current :: remaining ->
        (* Count how many times current element appears in original list *)
        let element_count = List.length (List.filter ((=) current) list) in
        
        (* Skip if we've already counted this element *)
        if List.mem_assoc current accumulator then 
          build_counts accumulator remaining
        (* Add new (element, count) pair to accumulator *)
        else 
          build_counts ((current, element_count) :: accumulator) remaining
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
        let (pos_x, pos_y) = robot.position in
        get_quadrant width height pos_x pos_y)
  in
  List.fold_left (fun acc (_, count) -> acc * count) 1 
    (count_occurrences quadrant_counts)



(** Set module for tracking unique positions *)
module PosSet = Set.Make(struct 
  type t = int * int 
  let compare = compare 
end)


(** Extracted helper function to filter valid positions and convert to PosSet *)
let get_valid_positions robots width height =
  robots
  |> Array.map (fun r -> r.position)
  |> Array.to_list
  |> List.filter (fun (x, y) -> x >= 0 && x < width && y >= 0 && y < height)
  |> PosSet.of_list


(** Extracted helper function to create and populate the grid *)
let create_grid positions width height default_char entity_char =
  let grid = Array.make_matrix height width default_char in
  PosSet.iter (fun (x, y) -> grid.(y).(x) <- entity_char) positions;
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




(** Core simulation logic for part 2
    @param robots Initial robot states
    @param width Grid width
    @param height Grid height
    @return Time when Christmas tree pattern appears *)
let part2 ((robots, width, height)) =
  let rec search elapsed robots =
    if 
      elapsed >= 1000 
    then 
      ()
    else begin
      visualize_positions robots width height;
      Printf.printf "time = %d\n" elapsed;
      search (elapsed + 1) (Array.map (simulate_movement 1 width height) robots)
    end
  in
    let _ = search 0 (Array.of_list robots)  (* Use let _ to ignore unit result *)
  in  

  (* Optimized ASCII Art Cycle Calculation *)
  let rec find_first_valid p =
    let num_q = (81 - 30) + p * width in
    if num_q mod height = 0 then (81 + p * width)
    else find_first_valid (p + 1)
  in
  find_first_valid 0

  

  
(** Parses input string into robot data
    @param input Raw input string in format "p=x,y v=dx,dy"
    @return List of robots and grid dimensions *)
let parse input =
  let parse_coords str =
    let pattern = 
      "^p=\\(-?[0-9]+\\),\\(-?[0-9]+\\) v=\\(-?[0-9]+\\),\\(-?[0-9]+\\)$" 
  in
    let regexp = 
      Str.regexp pattern 
  in
    if 
      Str.string_match regexp str 0 
    then

      try 
        let pos_x = int_of_string (Str.matched_group 1 str) 
      in
        let pos_y = int_of_string (Str.matched_group 2 str) 
      in
        let vel_x = int_of_string (Str.matched_group 3 str) 
      in
        let vel_y = int_of_string (Str.matched_group 4 str) 
      in
        Some { 
          position = (pos_x, pos_y);
          velocity = (vel_x, vel_y)
        }

      with _ -> None
    else None
  in

  let process_line i line =
    match parse_coords (String.trim line) with
    | Some robot -> robot
    | None -> 
        failwith (Printf.sprintf 
          "Error at input line %d:
           Invalid format in input line: '%s'
           Expected format: 'p=x,y v=dx,dy' " 
          (i + 1) line)
  in

  input
  |> String.trim
  |> String.split_on_char '\n'
  |> List.filter (fun line -> String.length (String.trim line) > 0)
  |> List.mapi process_line



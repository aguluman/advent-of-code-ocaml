type robot = {
  position: int * int;
  velocity: int * int
}

type bathroom_quadrant =
  | Top_right
  | Top_left
  | Bottom_left
  | Bottom_right


  
let rec simulate_robot_movement seconds_remaining grid_width grid_height robot =
  if seconds_remaining = 0 then 
    robot
  else
    let (pos_x, pos_y) = robot.position in
    let (vel_x, vel_y) = robot.velocity in

    let teleported_position = 
      ((grid_width + pos_x + vel_x) mod grid_width, (* X axis *)
       (grid_height + pos_y + vel_y) mod grid_height) (* Y axis *)
    in
    
    simulate_robot_movement
      (seconds_remaining - 1)
      grid_width
      grid_height
      { robot with position = teleported_position }




let part1 (security_robots, grid_width, grid_height) =
  let quadrant_counts =
    security_robots 
    |> List.map (simulate_robot_movement 100 grid_width grid_height)
    |> List.filter_map (fun robot -> 
        let (pos_x, pos_y) = robot.position in      if pos_x > grid_width / 2 && pos_y < grid_height / 2 then
        Some Top_right
      else if pos_x < grid_width / 2 && pos_y < grid_height / 2 then
        Some Top_left
      else if pos_x < grid_width / 2 && pos_y > grid_height / 2 then
        Some Bottom_left
      else if pos_x > grid_width / 2 && pos_y > grid_height / 2 then
        Some Bottom_right
      else
        None) (* Robots on dividing lines don't count *)
  in

  (* Multiply counts from each quadrant to get the safety factor *)
  let count_occurrences lst =
    let rec aux acc = function
      | [] -> acc
      | x :: xs ->
          let count = List.length (List.filter ((=) x) lst) in
          if List.mem_assoc x acc then aux acc xs
          else aux ((x, count) :: acc) xs
    in
    aux [] lst
  in

  List.fold_left (fun acc (_, count) -> acc * count) 1 (count_occurrences quadrant_counts)




module PosSet = Set.Make(struct 
  type t = int * int 
  let compare = compare 
end)

let part2 ((robots, width, height)) =
  let rec search elapsed robots =
    if elapsed >= 1000 then ()
    else
      let positions =
        robots 
        |> Array.map (fun robot -> robot.position) 
        |> Array.to_list 
        |> List.filter (fun (row, col) -> row >= 0 && row < height && col >= 0 && col < width) 
        |> PosSet.of_list
      in

      let map = Array.make_matrix height width ' ' in
      PosSet.iter (fun (row, col) -> 
        if row >= 0 && row < height && col >= 0 && col < width then
          map.(row).(col) <- '@'
        else
          Printf.printf "Warning: Out of bounds position (%d, %d)\n" row col
      ) positions;

      Printf.printf "t = %d\n" elapsed;
      let buffer = Buffer.create (width * height) in
      for row = 0 to height - 1 do
        for col = 0 to width - 1 do
          Buffer.add_char buffer map.(row).(col)
        done;
        Buffer.add_char buffer '\n'
      done;
      Buffer.output_buffer stdout buffer;

      search (elapsed + 1) (Array.map (simulate_robot_movement 1 width height) robots)
  in
  search 0 (Array.of_list robots);

  (* Optimized ASCII Art Cycle Calculation *)
  let rec find_first_valid p =
    let num_q = (81 - 30) + p * width in
    if num_q mod height = 0 then (81 + p * width)
    else find_first_valid (p + 1)
  in
  find_first_valid 0


  
  

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



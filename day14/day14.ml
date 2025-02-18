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




let part2 ((robots, width, height)) =
  let move_robot_once robot =
    let (pos_x, pos_y) = robot.position in
    let (vel_x, vel_y) = robot.velocity in
    let new_x = (pos_x + vel_x + width) mod width in
    let new_y = (pos_y + vel_y + height) mod height in
    { robot with position = (new_x, new_y) }
  in
  let rec search elapsed robots =
    if elapsed >= 1000 then
      ()
    else
      let positions_hash = Hashtbl.create (List.length robots) in
      List.iter (fun r -> Hashtbl.replace positions_hash r.position ()) robots;
      let map =
        Array.init height (fun row ->
          Array.init width (fun col ->
            if Hashtbl.mem positions_hash (col, row) then '@' else ' '))
      in
      Printf.printf "t = %d\n" elapsed;
      map |> Array.map (fun row -> String.init width (fun i -> row.(i))) 
          |> Array.to_list 
          |> String.concat "\n" 
          |> Printf.printf "%s\n";
      Printf.printf "\n\n\n\n\n";
      let new_robots = List.map move_robot_once robots in
      search (elapsed + 1) new_robots
  in
  search 0 robots;

  (* ASCII art cycle detection *)
  let solution = 
    List.init 1001 (fun p -> p)
    |> List.filter_map (fun p ->
        let num_q = (81 - 30) + p * width in
        if num_q mod height = 0 then
          Some (81 + p * width)
        else
          None)
    |> List.hd
  in
  solution

  
  

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



(** Day 15: Warehouse Woes
    Solution for simulating a robot pushing boxes in a warehouse where:
    - Robot (@) can move in four directions (^ v < >)
    - Boxes (O) can be pushed if space behind is empty
    - Walls (#) block movement
    - GPS coordinates are calculated as (100 * row + column)
    - Part 1: Calculate sum of all boxes' GPS coordinates after moves *)

(** Types representing warehouse contents *)
type cell =
  | Robot    (** @ - The robot *)
  | Box      (** O - Pushable box *)
  | Wall     (** # - Immovable wall *)
  | Empty    (** . - Empty space *)

type scaled_cell =
  | Scaled_Robot    (** @ - The robot *)
  | BoxLeft  (** [ - Left half of large box *)
  | BoxRight (** ] - Right half of large box *) 
  | Scaled_Wall     (** # - Immovable wall *)
  | Scaled_Empty    (** . - Empty space *)

(** Movement directions for the robot *)
type direction =
  | Up       (** ^ - Move north *)
  | Down     (** v - Move south *)
  | Left     (** < - Move west *)
  | Right    (** > - Move east *)

(** Representation of the warehouse state *)
type warehouse = {
  cells: cell array array;    (** Grid of cells *)
  robot_pos: int * int;       (** Current robot position (row, col) *)
  width: int;                 (** Warehouse width *)
  height: int                 (** Warehouse height *)
}

type scaled_warehouse = {
  scaled_cells: scaled_cell array array;    (** Grid of cells *)
  scaled_robot_pos: int * int;              (** Current robot position (row, col) *)
  scaled_width: int;                        (** Warehouse width *)
  scaled_height: int                        (** Warehouse height *)
}

(** Part 1 Logic *)

(** Try to push a box in the given direction
    @param warehouse Current warehouse state
    @param row Box's current row
    @param col Box's current column
    @param delta_row Row movement (-1/0/1)
    @param delta_col Column movement (-1/0/1)
    @return true if box was pushed successfully *)
let push_box warehouse row col delta_row delta_col =
  let rec attempt_push current_row current_col cont =
    let next_row = current_row + delta_row in
    let next_col = current_col + delta_col in
    
    (* Check bounds *)
    if next_row < 0 || next_row >= warehouse.height ||
       next_col < 0 || next_col >= warehouse.width then
      cont false
    else match warehouse.cells.(next_row).(next_col) with
      | Wall -> cont false
      | Empty -> 
          warehouse.cells.(next_row).(next_col) <- Box;
          warehouse.cells.(current_row).(current_col) <- Empty;
          cont true
      | Box ->
          attempt_push next_row next_col (fun success ->
            if success then begin
              warehouse.cells.(next_row).(next_col) <- Box;
              warehouse.cells.(current_row).(current_col) <- Empty;
              cont true
            end else
              cont false)
      | Robot -> cont false
  in
  attempt_push row col (fun x -> x)

(** Move robot in specified direction, handling box pushing
    @param warehouse Current warehouse state
    @param direction Movement direction
    @return Updated warehouse state *)
let move_robot warehouse direction =
  let row, col = warehouse.robot_pos in
  let delta_row, delta_col = match direction with
    | Up -> (-1, 0)
    | Down -> (1, 0)
    | Left -> (0, -1)
    | Right -> (0, 1)
  in
  let next_row = row + delta_row in
  let next_col = col + delta_col in
  
  if next_row < 0 || next_row >= warehouse.height ||
     next_col < 0 || next_col >= warehouse.width then
    warehouse
  else match warehouse.cells.(next_row).(next_col) with
    | Empty ->
        warehouse.cells.(row).(col) <- Empty;
        warehouse.cells.(next_row).(next_col) <- Robot;
        { warehouse with robot_pos = (next_row, next_col) }
    | Box ->
        if push_box warehouse next_row next_col delta_row delta_col then begin
          warehouse.cells.(row).(col) <- Empty;
          warehouse.cells.(next_row).(next_col) <- Robot;
          { warehouse with robot_pos = (next_row, next_col) }
        end else
          warehouse
    | Wall | Robot -> warehouse

(** Calculate GPS coordinate sum for all boxes
    @param warehouse Current warehouse state
    @return Sum of GPS coordinates (100 * row + col) for all boxes *)
let calculate_gps_sum warehouse =
  let sum = ref 0 in
  for row = 0 to warehouse.height - 1 do
    for col = 0 to warehouse.width - 1 do
      if warehouse.cells.(row).(col) = Box then
        sum := !sum + (100 * row + col)
    done
  done;
  !sum

(* Part 2 Logic *)
let expand_warehouse warehouse =
  let new_width = warehouse.width * 2 in
  let scaled_cells = Array.make_matrix warehouse.height new_width Scaled_Empty in
  
  (* Map original cells to expanded grid *)
  for row = 0 to warehouse.height - 1 do
    for col = 0 to warehouse.width - 1 do
      match warehouse.cells.(row).(col) with
      | Robot -> 
          scaled_cells.(row).(col * 2) <- Scaled_Robot;
          scaled_cells.(row).(col * 2 + 1) <- Scaled_Empty
      | Box -> 
          scaled_cells.(row).(col * 2) <- BoxLeft;
          scaled_cells.(row).(col * 2 + 1) <- BoxRight
      | Wall -> 
          scaled_cells.(row).(col * 2) <- Scaled_Wall;
          scaled_cells.(row).(col * 2 + 1) <- Scaled_Wall
      | Empty ->
          scaled_cells.(row).(col * 2) <- Scaled_Empty;
          scaled_cells.(row).(col * 2 + 1) <- Scaled_Empty
    done
  done;
  
  { scaled_cells;
    scaled_robot_pos = (fst warehouse.robot_pos, snd warehouse.robot_pos * 2);
    scaled_width = new_width;
    scaled_height = warehouse.height }


let push_scaled_box scaled_warehouse row col delta_row delta_col =
  let rec attempt_push current_row current_col cont =
    let next_row = current_row + delta_row in
    let next_col = current_col + delta_col in
    
    if next_row < 0 || next_row >= scaled_warehouse.scaled_height ||
       next_col < 0 || next_col + 1 >= scaled_warehouse.scaled_width then
      cont false
    else
      let current_cells = (scaled_warehouse.scaled_cells.(current_row).(current_col),
                         scaled_warehouse.scaled_cells.(current_row).(current_col + 1)) in
      let next_cells = (scaled_warehouse.scaled_cells.(next_row).(next_col),
                      scaled_warehouse.scaled_cells.(next_row).(next_col + 1)) in
      
      match (current_cells, next_cells) with
      | ((BoxLeft, BoxRight), (Scaled_Empty, Scaled_Empty)) ->
          scaled_warehouse.scaled_cells.(next_row).(next_col) <- BoxLeft;
          scaled_warehouse.scaled_cells.(next_row).(next_col + 1) <- BoxRight;
          scaled_warehouse.scaled_cells.(current_row).(current_col) <- Scaled_Empty;
          scaled_warehouse.scaled_cells.(current_row).(current_col + 1) <- Scaled_Empty;
          cont true
      | ((BoxLeft, BoxRight), (BoxLeft, BoxRight)) ->
          attempt_push next_row next_col (fun success ->
            if success then begin
              scaled_warehouse.scaled_cells.(next_row).(next_col) <- BoxLeft;
              scaled_warehouse.scaled_cells.(next_row).(next_col + 1) <- BoxRight;
              scaled_warehouse.scaled_cells.(current_row).(current_col) <- Scaled_Empty;
              scaled_warehouse.scaled_cells.(current_row).(current_col + 1) <- Scaled_Empty;
              cont true
            end else cont false)
      | _ -> cont false
  in
  attempt_push row col (fun x -> x)



let move_scaled_robot scaled_warehouse direction =
  let row, col = scaled_warehouse.scaled_robot_pos in
  let delta_row, delta_col = match direction with
    | Up -> (-1, 0)
    | Down -> (1, 0)
    | Left -> (0, -2)  (* Note: moving 2 cells at a time horizontally *)
    | Right -> (0, 2)  (* Note: moving 2 cells at a time horizontally *)
  in
  let next_row = row + delta_row in
  let next_col = col + delta_col in
  
  if next_row < 0 || next_row >= scaled_warehouse.scaled_height ||
     next_col < 0 || next_col >= scaled_warehouse.scaled_width then
    scaled_warehouse
  else match scaled_warehouse.scaled_cells.(next_row).(next_col) with
    | Scaled_Empty ->
        scaled_warehouse.scaled_cells.(row).(col) <- Scaled_Empty;
        scaled_warehouse.scaled_cells.(next_row).(next_col) <- Scaled_Robot;
        { scaled_warehouse with scaled_robot_pos = (next_row, next_col) }
    | BoxLeft ->
        if push_scaled_box scaled_warehouse next_row next_col delta_row delta_col then begin
          scaled_warehouse.scaled_cells.(row).(col) <- Scaled_Empty;
          scaled_warehouse.scaled_cells.(next_row).(next_col) <- Scaled_Robot;
          { scaled_warehouse with scaled_robot_pos = (next_row, next_col) }
        end else
          scaled_warehouse
    | Scaled_Wall | Scaled_Robot | BoxRight -> scaled_warehouse


let calculate_scaled_gps_sum scaled_warehouse =
  let sum = ref 0 in
  for row = 0 to scaled_warehouse.scaled_height - 1 do
    for col = 0 to (scaled_warehouse.scaled_width - 2) / 2 * 2 do
      if scaled_warehouse.scaled_cells.(row).(col) = BoxLeft &&
         scaled_warehouse.scaled_cells.(row).(col + 1) = BoxRight then
        sum := !sum + (100 * row + (col / 2))
    done
  done;
  !sum


(** Parse warehouse map from string input
    @param input String containing warehouse layout
    @return Warehouse state with initial positions *)
let parse_map input =
  let lines = String.split_on_char '\n' input |> List.filter (fun s -> s <> "") in
  let height = List.length lines in
  let width = String.length (List.hd lines) in
  let cells = Array.make_matrix height width Empty in
  let robot_pos = ref (0, 0) in
  
  List.iteri (fun row line ->
    String.iteri (fun col ch ->
      cells.(row).(col) <- match ch with
        | '@' -> robot_pos := (row, col); Robot
        | 'O' -> Box
        | '#' -> Wall
        | '.' -> Empty
        | c -> failwith (Printf.sprintf "Invalid character in map: %c" c)
    ) line
  ) lines;
  
  { cells; robot_pos = !robot_pos; width; height }

(** Parse complete input into warehouse and movement sequence
    @param input Raw puzzle input string
    @return Tuple of initial warehouse state and movement directions *)
let parse input =
  let parts = String.split_on_char '\n' input 
              |> List.filter (fun s -> s <> "") 
              |> List.partition (fun s -> 
                   String.length s > 0 && String.contains "#@O." s.[0]) in
  match parts with
  | (map_lines, move_lines) ->
      let warehouse = parse_map (String.concat "\n" map_lines) in
      let moves = String.concat "" move_lines
                 |> String.to_seq
                 |> List.of_seq
                 |> List.filter_map (function
                     | '^' -> Some Up
                     | 'v' -> Some Down
                     | '<' -> Some Left
                     | '>' -> Some Right
                     | _ -> None) in
      (warehouse, moves)

(** Solve part 1: simulate all moves and calculate final GPS sum
    @param input Raw puzzle input
    @return Sum of all boxes' GPS coordinates after moves *)
let part1 (warehouse, moves) =
  List.fold_left (fun state direction -> 
    move_robot state direction
  ) warehouse moves
  |> calculate_gps_sum



let part2 (warehouse, moves) =
  let expanded = expand_warehouse warehouse in
  List.fold_left (fun state direction ->
    move_scaled_robot state direction
  ) expanded moves
  |> calculate_scaled_gps_sum




(** Main entry point *)
let () =
  In_channel.input_all In_channel.stdin
  |> String.trim
  |> parse
  |> (fun input ->
      let start_time = Unix.gettimeofday () in
      let result1 = part1 input in
      let result2 = part2 input in
      Printf.printf "Part 1: %d\nPart 2: %d\n" result1 result2;
      Unix.gettimeofday () -. start_time)
  |> Printf.printf "Elapsed time: %.4f seconds\n"
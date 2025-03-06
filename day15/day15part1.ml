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



(** Solve part 1: simulate all moves and calculate final GPS sum
    @param input Raw puzzle input
    @return Sum of all boxes' GPS coordinates after moves *)
let part1 (warehouse, moves) =
  List.fold_left (fun state direction -> 
    move_robot state direction
  ) warehouse moves
  |> calculate_gps_sum


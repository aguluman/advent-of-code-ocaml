open Day15part1

type scaled_cell =
  | Scaled_Robot    (** @ - The robot *)
  | BoxLeft  (** [ - Left half of large box *)
  | BoxRight (** ] - Right half of large box *) 
  | Scaled_Wall     (** # - Immovable wall *)
  | Scaled_Empty    (** . - Empty space *)


type scaled_warehouse = {
  scaled_cells: scaled_cell array array;    (** Grid of cells *)
  scaled_robot_pos: int * int;              (** Current robot position (row, col) *)
  scaled_width: int;                        (** Warehouse width *)
  scaled_height: int                        (** Warehouse height *)
}



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
    (* Iterate through all columns where we could find a box's left half *)
    for col = 0 to scaled_warehouse.scaled_width - 2 do 
      (* Check if this is an even column with a BoxLeft + BoxRight pair *)
      if col mod 2 = 0 && 
         scaled_warehouse.scaled_cells.(row).(col) = BoxLeft &&
         scaled_warehouse.scaled_cells.(row).(col + 1) = BoxRight then
        sum := !sum + (100 * row + (col / 2))
    done
  done;
  !sum





let part2 (warehouse, moves) =
  let expanded = expand_warehouse warehouse in
  List.fold_left (fun state direction ->
    move_scaled_robot state direction
  ) expanded moves
  |> calculate_scaled_gps_sum





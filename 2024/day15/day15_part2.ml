open Day15_part1

(** Scaled cell types for part 2 where each cell is represented by 2x2 grid *)
type scaled_cell =
  | ScaledRobot  (** Robot in scaled mode *)
  | BoxL  (** Left half of a box *)
  | BoxR  (** Right half of a box *)
  | ScaledWall  (** Wall in scaled mode *)
  | ScaledEmpty  (** Empty space in scaled mode *)

(* Scaled operations *)

(** [downgrade] converts a scaled_cell type to its corresponding cell type version.
    This function is used to map between the scaled and normal warehouse representations.

    The mapping follows these rules:
    - ScaledRobot -> Robot (robot in normal mode)  
    - BoxL/BoxR -> Box (both halves become a single box 'O')
    - ScaledWall -> Wall (walls remain walls)
    - ScaledEmpty -> Empty (empty spaces remain empty)

    @param cell A scaled cell from the 2x2 grid representation
    @return The corresponding unscaled cell type
    @see [scaled_cell] for the scaled cell types
    @see [cell] for the unscaled cell types *)
let downgrade = function
  | ScaledRobot -> Robot
  | BoxL | BoxR -> Box
  | ScaledWall -> Wall
  | ScaledEmpty -> Empty

(** [find_robot_scaled map] finds the robot's position in a scaled warehouse grid
    by first downgrading the scaled cells to their normal counterparts.

    The function works by:
    1. Converting the scaled grid to normal grid using downgrade
    2. Using the standard find_robot function on the converted grid

    @param map Scaled warehouse grid
    @return Tuple (row, col) of robot position
    @raise Failure if robot not found
    @see [downgrade] for the cell conversion rules
    @see [find_robot] for the base implementation *)
let find_robot_scaled map =
  map |> Array.map (fun row -> Array.map downgrade row) |> find_robot

(** [push_left_scaled i j map] attempts to push a scaled box left from position (i,j)
    in the scaled warehouse grid. A scaled box consists of two cells: BoxL and BoxR.

    The function works by:
    1. Verifying the current position contains BoxR and BoxL
    2. Checking the space to the left:
       - If wall: cannot push
       - If empty: move the box left
       - If another box: recursively try to push that box first
    
    @param i Row index in the scaled grid
    @param j Column index of BoxR in the scaled grid
    @param map Scaled warehouse grid
    @return Some new_map if push successful, None if blocked
    @raise Assert_failure if position doesn't contain a valid box
    @raise Failure if encountering invalid cell types
    @see [scaled_cell] for the cell types in scaled mode *)
let rec push_left_scaled i j map =
  assert (map.(i).(j) = BoxR);
  assert (map.(i).(j - 1) = BoxL);
  match map.(i).(j - 2) with
  | ScaledWall -> None
  | ScaledEmpty ->
      let new_row = map.(i) |> swap (j - 2) (j - 1) |> swap (j - 1) j in
      Some (array_update_at i new_row map)
  | BoxR -> (
      match push_left_scaled i (j - 2) map with
      | None -> None
      | Some new_map ->
          assert (new_map.(i).(j - 2) = ScaledEmpty);
          push_left_scaled i j new_map)
  | _ -> failwith "Invalid scaled cell"

(** [push_up_scaled i j map] attempts to push a scaled box upward from position
    (i,j) in the scaled warehouse grid. A scaled box consists of two adjacent
    cells: BoxL and BoxR.

    The function handles several cases: 1. If starting at BoxL:
    - Verifies BoxR is to the right
    - Checks cells above for: * Walls: Cannot push * Empty spaces: Move box up *
      Other boxes: Recursively push those first 2. If starting at BoxR:
    - Redirects to handle from BoxL position

    Complex pushing scenarios:
    - Single box: Direct push if space available
    - Adjacent boxes: Recursive push of blocking boxes
    - Stacked boxes: Coordinated push of multiple boxes

    @param i Row index in the scaled grid
    @param j Column index in the scaled grid
    @param map Scaled warehouse grid
    @return Some new_map if push successful, None if blocked
    @raise Assert_failure if box configuration is invalid
    @raise Failure
      if encountering invalid cell types
      {[
        @see [scaled_cell] for the cell types in scaled mode
      ]} *)
let rec push_up_scaled i j map =
  match map.(i).(j) with
  | BoxL -> (
      assert (map.(i).(j + 1) = BoxR);
      match (map.(i - 1).(j), map.(i - 1).(j + 1)) with
      | ScaledWall, _ | _, ScaledWall -> None
      | ScaledEmpty, ScaledEmpty ->
          let new_row1 =
            map.(i - 1)
            |> array_update_at j BoxL
            |> array_update_at (j + 1) BoxR
          in
          let new_row2 =
            map.(i)
            |> array_update_at j ScaledEmpty
            |> array_update_at (j + 1) ScaledEmpty
          in
          Some
            (map
            |> array_update_at (i - 1) new_row1
            |> array_update_at i new_row2)
      | BoxL, BoxR | BoxR, ScaledEmpty -> (
          match push_up_scaled (i - 1) j map with
          | None -> None
          | Some new_map ->
              assert (new_map.(i - 1).(j) = ScaledEmpty);
              assert (new_map.(i - 1).(j + 1) = ScaledEmpty);
              push_up_scaled i j new_map)
      | ScaledEmpty, BoxL -> (
          match push_up_scaled (i - 1) (j + 1) map with
          | None -> None
          | Some new_map ->
              assert (new_map.(i - 1).(j) = ScaledEmpty);
              assert (new_map.(i - 1).(j + 1) = ScaledEmpty);
              push_up_scaled i j new_map)
      | BoxR, BoxL -> (
          match push_up_scaled (i - 1) j map with
          | None -> None
          | Some new_map1 -> (
              assert (new_map1.(i - 1).(j - 1) = ScaledEmpty);
              assert (new_map1.(i - 1).(j) = ScaledEmpty);
              match push_up_scaled (i - 1) (j + 1) new_map1 with
              | None -> None
              | Some new_map2 ->
                  assert (new_map2.(i - 1).(j + 1) = ScaledEmpty);
                  assert (new_map2.(i - 1).(j + 2) = ScaledEmpty);
                  push_up_scaled i j new_map2))
      | _, _ -> failwith "Invalid scaled cells")
  | BoxR -> push_up_scaled i (j - 1) map
  | _ -> failwith "Invalid scaled cell"

(** [move_left_scaled map] moves the robot left in a scaled warehouse grid.
    The function handles the following cases:
    1. Wall: Movement blocked, return unchanged map
    2. Empty space: Move robot directly
    3. Box: Attempt to push box using push_left_scaled
    4. Invalid cells: Raise error

    The movement rules maintain the scaled grid invariants where:
    - Robot occupies a single cell
    - Boxes occupy two adjacent cells (BoxL, BoxR)
    - Movement preserves box integrity

    @param map Scaled warehouse grid
    @return Updated warehouse grid with robot moved left if possible
    @raise Assert_failure if movement creates invalid box configuration
    @raise Failure if encountering invalid cell types
    @see find_robot_scaled for robot position detection
    @see [push_left_scaled] for box pushing logic
    @see [scaled_cell] for the cell types in scaled mode *)
let rec move_left_scaled map =
  let ri, rj = find_robot_scaled map in
  match map.(ri).(rj - 1) with
  | ScaledWall -> map
  | ScaledEmpty ->
      let new_row = swap (rj - 1) rj map.(ri) in
      array_update_at ri new_row map
  | BoxR -> (
      match push_left_scaled ri (rj - 1) map with
      | None -> map
      | Some new_map ->
          assert (new_map.(ri).(rj - 1) = ScaledEmpty);
          move_left_scaled new_map)
  | _ -> failwith "Invalid scaled cell"

(** [move_right_scaled map] moves the robot right in a scaled warehouse grid.
    
    Implementation uses horizontal mirroring technique:
    1. Reverse the map horizontally while swapping BoxL/BoxR positions
    2. Apply left movement logic
    3. Reverse back to original orientation

    This approach reuses [move_left_scaled] logic while maintaining
    box integrity in the scaled grid where:
    - BoxL must always be left of BoxR
    - Robot movement and box pushing follow scaled rules

    @param map Scaled warehouse grid
    @return Updated warehouse grid with robot moved right if possible
    @raise Failure if encountering invalid cell types
    @see [move_left_scaled] for core movement logic
    @see [scaled_cell] for valid cell types *)
let move_right_scaled map =
  let reverse map =
    Array.map
      (fun row ->
        Array.map
          (function
            | BoxL -> BoxR
            | BoxR -> BoxL
            | c -> c)
          (array_rev row))
      map
  in
  map |> reverse |> move_left_scaled |> reverse

(** [move_up_scaled map] moves the robot upward in a scaled warehouse grid.
    The function handles three main cases:
    1. Wall above: Movement blocked, return unchanged map
    2. Empty space above: Move robot up by:
       - Placing ScaledRobot in cell above
       - Replacing current position with ScaledEmpty
    3. Box above (BoxL or BoxR): Attempt to push box using push_up_scaled

    Movement maintains scaled grid properties where:
    - Robot occupies a single cell
    - Boxes maintain their BoxL/BoxR pairing
    - Grid boundaries and walls block movement

    @param map Current state of scaled warehouse grid
    @return Updated warehouse grid with robot moved up if possible
    @raise Assert_failure if movement creates invalid configuration
    @raise Failure if encountering invalid cell types
    @see [find_robot_scaled] for robot position detection
    @see [push_up_scaled] for box pushing logic
    @see [scaled_cell] for valid cell types *)
let rec move_up_scaled map =
  let ri, rj = find_robot_scaled map in
  match map.(ri - 1).(rj) with
  | ScaledWall -> map
  | ScaledEmpty ->
      let new_row1 = array_update_at rj ScaledRobot map.(ri - 1) in
      let new_row2 = array_update_at rj ScaledEmpty map.(ri) in
      map |> array_update_at (ri - 1) new_row1 |> array_update_at ri new_row2
  | BoxL | BoxR -> (
      match push_up_scaled (ri - 1) rj map with
      | None -> map
      | Some new_map ->
          assert (new_map.(ri - 1).(rj) = ScaledEmpty);
          move_up_scaled new_map)
  | _ -> failwith "Invalid scaled cell"

(** [move_down_scaled map] moves the robot downward in a scaled warehouse grid.
    
    Implementation uses vertical mirroring technique:
    1. Reverse the map vertically (maintaining box orientation)
    2. Apply upward movement logic
    3. Reverse back to original orientation

    This approach reuses [move_up_scaled] logic while maintaining
    scaled grid properties:
    - Robot position is transformed correctly
    - Box pairs (BoxL/BoxR) maintain their horizontal relationship
    - Walls and empty spaces are mirrored appropriately

    @param map Current state of scaled warehouse grid
    @return Updated warehouse grid with robot moved down if possible
    @see [move_up_scaled] for core movement logic
    @see [array_rev] for vertical mirroring implementation
    @see [scaled_cell] for valid cell types *)
let move_down_scaled map =
  let reverse = array_rev in
  map |> reverse |> move_up_scaled |> reverse

(* Scale up operation *)

(** [scale_up map] converts a normal warehouse grid to a scaled version where each cell
    is expanded horizontally into two cells. The scaling follows these rules:

    For each original cell:
    - Robot -> [ScaledRobot][ScaledEmpty]
    - Box -> [BoxL][BoxR]
    - Wall -> [ScaledWall][ScaledWall]
    - Empty -> [ScaledEmpty][ScaledEmpty]

    This transformation enables more complex movement patterns by:
    - Splitting boxes into left/right components
    - Preserving robot position with empty space
    - Maintaining wall boundaries
    - Keeping grid consistency

    @param map Original warehouse grid using normal cell types
    @return Scaled warehouse grid with doubled horizontal dimension
    @see [scaled_cell] for scaled cell types
    @see [cell] for normal cell types *)
let scale_up map =
  Array.map
    (fun row ->
      Array.init
        (2 * Array.length row)
        (fun j ->
          let orig_idx = j / 2 in
          let is_odd = j mod 2 = 1 in
          match (row.(orig_idx), is_odd) with
          | Robot, false -> ScaledRobot
          | Robot, true -> ScaledEmpty
          | Box, false -> BoxL
          | Box, true -> BoxR
          | Wall, _ -> ScaledWall
          | Empty, _ -> ScaledEmpty))
    map

(** Part 2 Game Logic *)

(** [part2 (map, moves)] solves part 2 with scaled movement mechanics.
    The solution process follows these steps:
    1. Scale up the initial map (1x1 cells become 1x2)
    2. Apply movement sequence using scaled operations
    3. Calculate final score based on box positions

    Scoring rules:
    - Only BoxL positions are counted (left half of boxes)
    - Score = sum of (100 * row + column) for each BoxL
    - Row/column indices are 0-based

    @param map Initial warehouse grid in normal scale
    @param moves Sequence of directional moves (Up/Down/Left/Right)
    @return Final score based on box positions in scaled grid
    @see [scale_up] for grid scaling implementation
    @see [move_up_scaled] and related functions for scaled movement
    @see [scaled_cell] for scaled grid cell types *)
let part2 (map, moves) =
  let scaled_map = scale_up map in
  let final_map =
    Array.fold_left
      (fun acc dir ->
        match dir with
        | Up -> move_up_scaled acc
        | Left -> move_left_scaled acc
        | Down -> move_down_scaled acc
        | Right -> move_right_scaled acc)
      scaled_map (Array.of_seq moves)
  in
  let score = ref 0 in
  for i = 0 to Array.length final_map - 1 do
    for j = 0 to Array.length final_map.(0) - 1 do
      if final_map.(i).(j) = BoxL then score := !score + ((100 * i) + j)
    done
  done;
  !score

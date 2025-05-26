(* Types *)

(** Cell types in the warehouse grid *)
type cell =
  | Robot    (** Robot position '@' *)
  | Box      (** Box 'O' *)
  | Wall     (** Wall '#' *)
  | Empty    (** Empty space '.' *)

(** Movement directions *)
type direction =
  | Up       (** Move up '^' *)
  | Left     (** Move left '<' *)
  | Down     (** Move down 'v' *)
  | Right    (** Move right '>' *)




(** Helper Functions *)

(** [swap i j arr] creates a new array with elements at indices i and j swapped
    @param i First index
    @param j Second index
    @param arr Source array
    @return New array with swapped elements *)
let swap i j arr =
  let arr = Array.copy arr in
  let temp = arr.(i) in
  arr.(i) <- arr.(j);
  arr.(j) <- temp;
  arr


(** [array_update_at idx value arr] creates a new array with element at idx updated
    @param idx Index to update
    @param value New value
    @param arr Source array
    @return New array with updated element *)
let array_update_at idx value arr =
  let new_arr = Array.copy arr in
  new_arr.(idx) <- value;
  new_arr


(** [transpose matrix] transposes a 2D matrix (switches rows and columns)
    @param matrix Source matrix
    @return Transposed matrix *)
let transpose matrix =
  let h = Array.length matrix in
  let w = Array.length matrix.(0) in
  Array.init w (fun i ->
    Array.init h (fun j -> matrix.(j).(i))
  )

  
(** [array_rev arr] reverses the elements of an array.
    Creates a new array with elements in reverse order without modifying
    the original array.

    Example:
    {[
      array_rev [|1; 2; 3|] = [|3; 2; 1|]
    ]}

    @param arr Source array to reverse
    @return New array with elements in reverse order
    @raise Invalid_argument if array is empty *)
let array_rev arr =
  let len = Array.length arr in
  Array.init len (fun i -> arr.(len - 1 - i))


(** [find_robot map] finds the robot's position in the warehouse grid.
    The function scans the grid row by row searching for the Robot cell.

    Search process:
    1. Get grid dimensions (h x w)
    2. Scan each cell using nested loops
    3. Store first Robot position found
    4. Return position or fail if none found

    Grid assumptions:
    - Grid is rectangular (all rows same length)
    - Exactly one Robot exists in the grid
    - Grid indices are 0-based

    @param map 2D array representing warehouse grid
    @return Tuple (row, col) containing robot position
    @raise Failure if no robot found in grid
    @raise Invalid_argument if grid is empty or jagged
    @see [cell] for grid cell types *)
let find_robot map =
  let h = Array.length map in
  let w = Array.length map.(0) in
  let found = ref None in
  for i = 0 to h - 1 do
    for j = 0 to w - 1 do
      if map.(i).(j) = Robot then
        found := Some (i, j)
    done
  done;
  match !found with
  | Some pos -> pos
  | None -> failwith "Robot not found"




(** Basic Movement Functions *)

(** [push_left i j map] attempts to push a box left from position (i,j).
    The function handles these cases:
    1. Wall: Cannot push, returns None
    2. Empty space: Moves box left one position
    3. Another box: Recursively attempts to push both boxes
    4. Invalid cell: Raises error

    Box pushing rules:
    - Boxes can only be pushed into empty spaces
    - Multiple boxes can be pushed in sequence
    - Walls block all pushing attempts
    - Grid boundaries are treated as walls

    @param i Row index in warehouse grid
    @param j Column index of box to push
    @param map Current warehouse grid state
    @return Some new_map if push successful, None if blocked by wall
    @raise Assert_failure if starting position isn't a box
    @raise Failure if encountering invalid cell type
    @see [cell] for grid cell types
    @see [swap] for cell position swapping
    @see [array_update_at] for row updating *)
let rec push_left i j map =
  assert (map.(i).(j) = Box);
  match map.(i).(j-1) with
  | Wall -> None
  | Empty ->
      let new_row = swap (j-1) j map.(i) in
      Some (array_update_at i new_row map)
  | Box ->
      (match push_left i (j-1) map with
       | None -> None
       | Some new_map ->
           assert (new_map.(i).(j-1) = Empty);
           push_left i j new_map)
  | _ -> failwith "Invalid cell"


(** [move_left map] moves the robot left and handles box pushing.
    The function handles four cases:
    1. Wall: Movement blocked, return unchanged map
    2. Empty space: Move robot directly by swapping positions
    3. Box: Attempt to push box using push_left function
    4. Robot: Invalid state (should never occur)

    Movement rules:
    - Robot can move into empty spaces
    - Robot can push boxes if there's space
    - Walls block all movement
    - Multiple boxes can be pushed recursively

    @param map Current state of warehouse grid
    @return Updated warehouse grid with robot moved left if possible
    @raise Assert_failure if movement creates invalid state
    @raise Failure if encountering another robot
    @see [find_robot] for robot position detection
    @see [push_left] for box pushing logic
    @see [cell] for grid cell types *)
let rec move_left map =
  let (ri, rj) = find_robot map in
  match map.(ri).(rj-1) with
  | Wall -> map
  | Empty ->
      let new_row = swap (rj-1) rj map.(ri) in
      array_update_at ri new_row map
  | Box ->
      (match push_left ri (rj-1) map with
       | None -> map
       | Some new_map ->
           assert (new_map.(ri).(rj-1) = Empty);
           move_left new_map)
  | Robot -> failwith "Invalid move"


(** [move_right map] moves the robot right and handles box pushing by:
    1. Reversing the map horizontally
    2. Applying left movement logic
    3. Reversing back to original orientation
    
    This approach reuses the left movement logic by transforming the map,
    avoiding duplicate code.

    @param map Warehouse grid
    @return Updated warehouse grid with robot moved right if possible *)
let move_right map =
  let reverse arr =
    Array.map array_rev arr
  in
  map |> reverse |> move_left |> reverse


(** [move_up map] moves the robot up and handles box pushing by:
    1. Transposing the map
    2. Applying left movement logic
    3. Transposing back to original orientation
    
    This approach reuses the left movement logic by transforming the map.
    
    @param map Warehouse grid
    @return Updated warehouse grid with robot moved up if possible *)
let move_up map =
  map |> transpose |> move_left |> transpose


(** [move_down map] moves the robot down and handles box pushing by:
    1. Transposing the map
    2. Applying right movement logic
    3. Transposing back to original orientation
    
    This approach reuses the right movement logic by transforming the map.
    
    @param map Warehouse grid
    @return Updated warehouse grid with robot moved down if possible *)
let move_down map =
  map |> transpose |> move_right |> transpose


(** Part 1 Game Logic *)

(** [part1 (map, moves)] solves part 1 with basic movement mechanics.
    The solution process follows these steps:
    1. Apply movement sequence in order using basic movement functions
    2. Calculate final score based on box positions

    Scoring rules:
    - Each box contributes to the total score
    - Score per box = (100 * row + column)
    - Row/column indices are 0-based
    - Final score is sum of all box scores

    @param map Initial warehouse grid
    @param moves Sequence of directional moves (Up/Down/Left/Right)
    @return Final score based on box positions
    @see [move_up], [move_down], [move_left], [move_right] for movement implementations
    @see [cell] for grid cell types *)
let part1 (map, moves) =
  let final_map = 
    Array.fold_left (fun acc dir ->
      match dir with
      | Up -> move_up acc
      | Left -> move_left acc
      | Down -> move_down acc
      | Right -> move_right acc
    ) map (Array.of_seq moves)
  in
  let score = ref 0 in
  for i = 0 to Array.length final_map - 1 do
    for j = 0 to Array.length final_map.(0) - 1 do
      if final_map.(i).(j) = Box then
        score := !score + (100 * i + j)
    done
  done;
  !score

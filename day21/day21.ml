(** Solves Advent of Code Day 21 challenge about finding optimal paths between keypad buttons.

    The solution uses dynamic programming with memoization and path optimization techniques.

    {ul
    {- Input: A set of button sequences (codes) that need to be navigated}
    {- Part 1: Calculate the minimum cost to navigate each sequence with a recursion level of 3}
    {- Part 2: Calculate the minimum cost with a much deeper recursion level of 26}
    }

    Both numeric (characters '0'-'9') and directional keypads are supported.

    Directional keypad includes the following characters:
    {ul
    {- '^' (up arrow)}
    {- 'v' (down arrow)}
    {- '<' (left arrow)}
    {- '>' (right arrow)}
    {- 'A' (action button)}
    }

    @see <https://adventofcode.com/2024/day/21> Advent of Code 2024, Day 21
*)


(* Helper Functions*)
let rec make_pairs = function
  | [] | [_] -> []
  | first_button :: second_button :: rest -> (first_button, second_button) :: make_pairs (second_button :: rest)

let flatten list_of_lists =
  let rec aux acc = function
    | [] -> List.rev acc
    | lst :: rest -> aux (List.rev_append lst acc) rest
  in
  aux [] list_of_lists


(** Maps a numeric [button] character to its 2D position on the numeric keypad.

    The numeric keypad layout is as follows:

    {[
    7 8 9
    4 5 6
    1 2 3
      0 A
    ]}

  @raise Failure if the specified [button] is not a valid directional button.
  @param button A character representing a button on the numeric keypad 
  @return A tuple (row, column) representing the button's position *)
let get_door_keypad_position button =
  match button with
  | '7' -> (0, 0)
  | '8' -> (0, 1)
  | '9' -> (0, 2)
  | '4' -> (1, 0)
  | '5' -> (1, 1)
  | '6' -> (1, 2)
  | '1' -> (2, 0)
  | '2' -> (2, 1)
  | '3' -> (2, 2)
  | '0' -> (3, 1)
  | 'A' -> (3, 2)
  | c -> failwith (Printf.sprintf "Invalid door keypad button: %c" c)



(** Retrieves the row and column coordinates for a specified directional keypad button.

    The directional keypad layout looks like this:

    {[
        ^  A
     <  v  >
    ]}

    Accepted characters include '^', 'A', '<', 'v', and '>'. Any unrecognized character
    will cause an exception to be thrown.
    
  @raise Failure if the specified [button] is not a valid directional button.

  @param button A single character representing the directional keypad button.
  @return A tuple (row, column) indicating the button's position in a 2D grid,
    where row 0, column 0 is the top-left corner. *)
let get_directional_keypad_position button =
  match button with
  | '^' -> (0, 1)
  | 'A' -> (0, 2)
  | '<' -> (1, 0)
  | 'v' -> (1, 1)
  | '>' -> (1, 2)
  | c -> failwith (Printf.sprintf "Invalid directional keypad button: %c" c)



(** Calculates possible routes between two numeric buttons on the keypad.

    For most button pairs, there are two possible routes:
    {ol
    {- Moving vertically first, then horizontally (i.e. vertical_first)}
    {- Moving horizontally first, then vertically (i.e. horizontal_first)}
    }

    Special cases exist for certain button combinations where only one route is logical.
    Each route ends with the 'A' button, representing the selection/confirmation.

    @param [source_button] The starting numeric button character 
    @param [target_button] The destination numeric button character 
    @return A list of possible routes, where each route is a list of buttons to press *)
let get_numeric_button_route source_button target_button =
  let (source_row, source_column) = get_door_keypad_position source_button in
  let (target_row, target_column) = get_door_keypad_position target_button in
  let vertical_moves = 
    List.init (abs (source_row - target_row)) 
    (fun _ -> if source_row < target_row then 'v' else '^') 
  in
  let horizontal_moves = 
    List.init (abs (source_column - target_column)) 
    (fun _ -> if source_column < target_column then '>' else '<') 
  in
  let vertical_first = vertical_moves @ horizontal_moves @ ['A'] in
  let horizontal_first = horizontal_moves @ vertical_moves @ ['A'] in

  let bottom_row_buttons = ['0'; 'A'] in
  let left_col_buttons = ['7'; '4'; '1'] in

  match (source_button, target_button) with
  | (src, tgt) when
      bottom_row_buttons
      |> List.map (fun bottom_button -> List.map (fun left_button -> (bottom_button, left_button)) left_col_buttons)
      |> flatten
      |> List.mem (src, tgt) -> [vertical_first]
  | (src, tgt) when
      left_col_buttons
      |> List.map (fun left_button -> List.map (fun bottom_button -> (left_button, bottom_button)) bottom_row_buttons)
      |> flatten
      |> List.mem (src, tgt) -> [horizontal_first]
  | _ -> List.sort_uniq compare [vertical_first; horizontal_first]



(** Calculates possible routes between two directional buttons on the keypad.

    For most button pairs, there are two possible routes:
    {ol
    {- Moving vertically first, then horizontally (vertical_first)}
    {- Moving horizontally first, then vertically (horizontal_first)}
    }

    Special cases exist for certain button combinations where only one route is logical.
    Each route ends with the 'A' button, representing the selection/confirmation.

    @param [source_button] The starting directional button character 
    @param [target_button] The destination directional button character 
    @return A list of possible routes, where each route is a list of buttons to press *)
let get_direction_button_route source_button target_button =
  let (source_row, source_column) = get_directional_keypad_position source_button in
  let (target_row, target_column) = get_directional_keypad_position target_button in
  let vertical_moves = 
    List.init (abs (source_row - target_row)) 
    (fun _ -> if source_row < target_row then 'v' else '^') 
  in
  let horizontal_moves = 
    List.init (abs (source_column - target_column)) 
    (fun _ -> if source_column < target_column then '>' else '<') 
  in
  let vertical_first = vertical_moves @ horizontal_moves @ ['A'] in
  let horizontal_first = horizontal_moves @ vertical_moves @ ['A'] in

  let top_row_buttons = ['^'; 'A'] in
  let left_col_buttons = ['<'] in

    match (source_button, target_button) with
  | (src, tgt) when
      top_row_buttons
      |> List.map (fun top_button ->
           left_col_buttons
           |> List.map (fun left_button -> (top_button, left_button)))
      |> flatten
      |> List.mem (src, tgt) -> [vertical_first]
  | (src, tgt) when
      left_col_buttons
      |> List.map (fun left_button ->
           top_row_buttons
           |> List.map (fun top_button -> (left_button, top_button)))
      |> flatten
      |> List.mem (src, tgt) -> [horizontal_first]
  | _ -> List.sort_uniq compare [vertical_first; horizontal_first]



(** Recursively calculates the minimum cost path between two buttons with a specified recursion level.

    This function uses a recursive approach to find the optimal path between two buttons.
    It considers both numeric and directional keypads, and uses memoization to optimize performance.

    @param recursion_level The maximum recursion depth allowed for the search
    @param source_button The starting button character 
    @param target_button The destination button character 
    @return A list of buttons representing the minimum cost path *)
let rec find_min_cost_path recursion_level source_button target_button =
  if recursion_level = 0 then
    [target_button]
  else
    let is_num_keypad = 
        ('0' <= source_button 
        && source_button <= '9') 
      || 
        ('0' <= target_button 
        && target_button <= '9') 
    in

    let possible_routes = 
      if is_num_keypad 
        then get_numeric_button_route source_button target_button 
        else get_direction_button_route source_button target_button 
    in

    let paths = List.map (fun r ->
      let pairs = ('A' :: r) |> (fun l -> 
        make_pairs l
      ) in
        flatten 
          (List.map (fun (first_button, second_button) -> 
            find_min_cost_path (recursion_level - 1) first_button second_button) pairs)
    ) possible_routes in
    
    List.hd 
      (List.sort 
        (fun first_path second_path -> 
          compare (List.length first_path) (List.length second_path)) paths)




(** Hashtable for memoizing results of the find_min_cost function to avoid redundant calculations. *)
let memo = Hashtbl.create 1000

(** Recursively calculates the minimum cost between two buttons with a specified recursion level.

    This function uses memoization to optimize performance by caching results of previous calculations.
    The recursion level determines how deep the algorithm will analyze possible paths.

    @param recursion_level 
      - The maximum recursion depth allowed for the search [or] 
      - The recursion depth for cost calculation
    @param source_button The starting button character 
    @param target_button The destination button character 
    @return The minimum cost (number of button presses) to navigate from source to target as an Int64 value *)
let rec find_min_cost recursion_level source_button target_button =
  let key = (recursion_level, source_button, target_button) in
  try
    Hashtbl.find memo key
  with Not_found ->
    let value =
      if recursion_level = 0 then
        1L
      else
        let is_num_keypad = 
          ('0' <= source_button 
          && source_button <= '9') 
        || 
          ('0' <= target_button 
          && target_button <= '9') in

        let possible_routes = 
          if is_num_keypad 
            then get_numeric_button_route source_button target_button 
            else get_direction_button_route source_button target_button 
        in

        let costs = List.map (fun r ->
          let pairs = ('A' :: r) |> (fun l -> 
            make_pairs l
          ) in
          List.fold_left 
            (fun acc (first_button, second_button) -> 
              Int64.add acc (find_min_cost (recursion_level - 1) first_button second_button)) 
              0L pairs
        ) possible_routes in
        
        List.hd (List.sort compare costs)
    in
    Hashtbl.add memo key value;
    value




(** Solves part 1 of the keypad navigation challenge.

   - This function calculates the total cost for navigating a sequence of button codes.
   - Using a recursion level of 3 for path calculation.
   - Each code's cost is multiplied by the numeric value of the code (excluding any trailing 'A' characters).

    @param [codes] A sequence of keypad button codes to navigate
    @return The total weighted cost of navigating all codes *)    
let part1 codes =
  Seq.fold_left (fun acc code ->
    let code_list = List.of_seq (String.to_seq code) in
    let navigation_cost =
      let pairs = ('A' :: code_list) |> (fun l -> 
        make_pairs l
      ) in
      let path = flatten (List.map 
        (fun (first_button, second_button) -> 
          find_min_cost_path (2 + 1) first_button second_button) pairs) 
        in
      Int64.of_int (List.length path)
    in
    let num = Int64.of_string 
      (String.sub code 0 (String.length code - (if code.[String.length code - 1] = 'A' then 1 else 0))) 
    in
    Int64.add acc (Int64.mul navigation_cost num)
  ) 0L codes




(** Solves part 2 of the keypad navigation challenge.

   - This function calculates the total cost for navigating a sequence of button codes.
   - Using a recursion level of 26 for path/cost calculation.
   - Each code's cost is multiplied by the numeric value of the code (excluding any trailing 'A' characters).
   - This is a more computationally intensive calculation compared to part 1.
   - The function uses memoization to optimize performance.

    @param [codes] A sequence of keypad button codes to navigate
    @return The total weighted cost of navigating all codes *)
let part2 codes =
  Seq.fold_left (fun acc code ->
    let code_list = List.of_seq (String.to_seq code) in
    let navigation_cost =
      let pairs = ('A' :: code_list) |> (fun l -> 
        make_pairs l
      ) in
      List.fold_left 
        (fun acc (first_button, second_button) -> 
          Int64.add acc (find_min_cost (25 + 1) first_button second_button)) 
          0L pairs
    in
    let num = Int64.of_string 
      (String.sub code 0 (String.length code - (if code.[String.length code - 1] = 'A' then 1 else 0))) 
    in
    Int64.add acc (Int64.mul navigation_cost num)
  ) 0L codes



(** [parse input] parses the input string into a sequence of strings.

    - The input string is split into lines, each line is trimmed for whitespace,
    converted to an array, and then transformed into a sequence.
    
    @param input The input string containing keypad codes separated by newlines.
    @return A sequence of strings, each representing a keypad code. *)
let parse input = 
  String.split_on_char '\n' input |> List.map String.trim |> Array.of_list |> Array.to_seq


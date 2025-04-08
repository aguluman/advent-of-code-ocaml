

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



let get_directional_keypad_position button =
  match button with
  | '^' -> (0, 1)
  | 'A' -> (0, 2)
  | '<' -> (1, 0)
  | 'v' -> (1, 1)
  | '>' -> (1, 2)
  | c -> failwith (Printf.sprintf "Invalid directional keypad button: %c" c)




let get_numeric_button_route source_button target_button =
  let (source_row, source_column) = get_door_keypad_position source_button in
  let (target_row, target_column) = get_door_keypad_position target_button in
  let vertical_moves = List.init (abs (source_row - target_row)) (fun _ -> if source_row < target_row then 'v' else '^') in
  let horizontal_moves = List.init (abs (source_column - target_column)) (fun _ -> if source_column < target_column then '>' else '<') in
  let vertical_first = vertical_moves @ horizontal_moves @ ['A'] in
  let horizontal_first = horizontal_moves @ vertical_moves @ ['A'] in

  let bottom_row_buttons = ['0'; 'A'] in
  let left_col_buttons = ['7'; '4'; '1'] in

  match (source_button, target_button) with
  | (src, tgt) when List.mem (src, tgt) (List.flatten (List.map (fun x -> List.map (fun y -> (x, y)) left_col_buttons) bottom_row_buttons)) -> [vertical_first]
  | (src, tgt) when List.mem (src, tgt) (List.flatten (List.map (fun x -> List.map (fun y -> (x, y)) bottom_row_buttons) left_col_buttons)) -> [horizontal_first]
  | _ -> List.sort_uniq compare [vertical_first; horizontal_first]



let get_direction_button_route source_button target_button =
  let (source_row, source_column) = get_directional_keypad_position source_button in
  let (target_row, target_column) = get_directional_keypad_position target_button in
  let vertical_moves = List.init (abs (source_row - target_row)) (fun _ -> if source_row < target_row then 'v' else '^') in
  let horizontal_moves = List.init (abs (source_column - target_column)) (fun _ -> if source_column < target_column then '>' else '<') in
  let vertical_first = vertical_moves @ horizontal_moves @ ['A'] in
  let horizontal_first = horizontal_moves @ vertical_moves @ ['A'] in

  let top_row_buttons = ['^'; 'A'] in
  let left_col_buttons = ['<'] in

  match (source_button, target_button) with
  | (src, tgt) when List.mem (src, tgt) (List.flatten (List.map (fun x -> List.map (fun y -> (x, y)) left_col_buttons) top_row_buttons)) -> [vertical_first]
  | (src, tgt) when List.mem (src, tgt) (List.flatten (List.map (fun x -> List.map (fun y -> (x, y)) top_row_buttons) left_col_buttons)) -> [horizontal_first]
  | _ -> List.sort_uniq compare [vertical_first; horizontal_first]



let rec find_min_cost_path recursion_level source_button target_button =
  if recursion_level = 0 then
    [target_button]
  else
    let is_num_keypad = ('0' <= source_button && source_button <= '9') || ('0' <= target_button && target_button <= '9') in
    let possible_routes = if is_num_keypad then get_numeric_button_route source_button target_button else get_direction_button_route source_button target_button in

    let paths = List.map (fun r ->
      let pairs = ('A' :: r) |> (fun l -> 
        let rec make_pairs = function
          | [] | [_] -> []
          | x :: y :: rest -> (x, y) :: make_pairs (y :: rest)
        in make_pairs l
      ) in
      List.flatten (List.map (fun (x, y) -> find_min_cost_path (recursion_level - 1) x y) pairs)
    ) possible_routes in
    
    List.hd (List.sort (fun l1 l2 -> compare (List.length l1) (List.length l2)) paths)




let memo = Hashtbl.create 1000

let rec find_min_cost recursion_level source_button target_button =
  let key = (recursion_level, source_button, target_button) in
  try
    Hashtbl.find memo key
  with Not_found ->
    let value =
      if recursion_level = 0 then
        1L
      else
        let is_num_keypad = ('0' <= source_button && source_button <= '9') || ('0' <= target_button && target_button <= '9') in
        let possible_routes = if is_num_keypad then get_numeric_button_route source_button target_button else get_direction_button_route source_button target_button in

        let costs = List.map (fun r ->
          let pairs = ('A' :: r) |> (fun l -> 
            let rec make_pairs = function
              | [] | [_] -> []
              | x :: y :: rest -> (x, y) :: make_pairs (y :: rest)
            in make_pairs l
          ) in
          List.fold_left (fun acc (x, y) -> Int64.add acc (find_min_cost (recursion_level - 1) x y)) 0L pairs
        ) possible_routes in
        
        List.hd (List.sort compare costs)
    in
    Hashtbl.add memo key value;
    value



let part1 codes =
  Seq.fold_left (fun acc code ->
    let code_list = List.of_seq (String.to_seq code) in
    let navigation_cost =
      let pairs = ('A' :: code_list) |> (fun l -> 
        let rec make_pairs = function
          | [] | [_] -> []
          | x :: y :: rest -> (x, y) :: make_pairs (y :: rest)
        in make_pairs l
      ) in
      let path = List.flatten (List.map (fun (x, y) -> find_min_cost_path (2 + 1) x y) pairs) in
      Int64.of_int (List.length path)
    in
    let num = Int64.of_string (String.sub code 0 (String.length code - (if code.[String.length code - 1] = 'A' then 1 else 0))) in
    Int64.add acc (Int64.mul navigation_cost num)
  ) 0L codes



let part2 codes =
  Seq.fold_left (fun acc code ->
    let code_list = List.of_seq (String.to_seq code) in
    let navigation_cost =
      let pairs = ('A' :: code_list) |> (fun l -> 
        let rec make_pairs = function
          | [] | [_] -> []
          | x :: y :: rest -> (x, y) :: make_pairs (y :: rest)
        in make_pairs l
      ) in
      List.fold_left (fun acc (x, y) -> Int64.add acc (find_min_cost (25 + 1) x y)) 0L pairs
    in
    let num = Int64.of_string (String.sub code 0 (String.length code - (if code.[String.length code - 1] = 'A' then 1 else 0))) in
    Int64.add acc (Int64.mul navigation_cost num)
  ) 0L codes



  
let parse input = 
  String.split_on_char '\n' input |> List.map String.trim |> Array.of_list |> Array.to_seq



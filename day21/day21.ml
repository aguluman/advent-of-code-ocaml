

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



let all_pairs list1 list2 =
  list1
  |> List.map (fun elem1 -> 
       list2 
       |> List.map (fun elem2 -> (elem1, elem2))
     )
  |> List.concat


(* Helper function to generate pairs from a list *)
let pairwise lst =
  let rec aux acc = function
    | a :: b :: rest -> aux ((a, b) :: acc) (b :: rest)
    | _ -> List.rev acc
  in
  aux [] lst


let calculate_numeric_button_route source_button target_button =
  let source_row, source_col = get_door_keypad_position source_button in
  let target_row, target_col = get_door_keypad_position target_button in
  
  let vertical_moves =
    List.init (abs (source_row - target_row)) (fun _ -> if source_row < target_row then 'v' else '^') in
  
  let horizontal_moves =
    List.init (abs (source_col - target_col)) (fun _ -> if source_col < target_col then '>' else '<') in
  
  let vertical_first = vertical_moves @ horizontal_moves @ ['A'] in
  let horizontal_first = horizontal_moves @ vertical_moves @ ['A'] in
  
  (* Special cases for bottom row to left column transitions (and vice versa) *)
  let bottom_row_buttons = ['0'; 'A'] in
  let left_col_buttons = ['7'; '4'; '1'] in
  
  let bottom_to_left_pairs = all_pairs bottom_row_buttons left_col_buttons in
  let left_to_bottom_pairs = all_pairs left_col_buttons bottom_row_buttons in
  
  if 
    List.exists (fun (a, b) -> a = source_button && b = target_button) bottom_to_left_pairs 
  then
    [vertical_first]
  else if 
    List.exists (fun (a, b) -> a = source_button && b = target_button) left_to_bottom_pairs 
  then
    [horizontal_first]
  else if 
    vertical_first = horizontal_first 
  then
    [vertical_first]
  else
    [vertical_first; horizontal_first]



let calculate_directional_button_route source_button target_button =
  let source_row, source_col = get_directional_keypad_position source_button in
  let target_row, target_col = get_directional_keypad_position target_button in
  
  let vertical_moves =
    List.init (abs (source_row - target_row)) (fun _ -> 
      if source_row < target_row then 'v' else '^')
  in
  
  let horizontal_moves =
    List.init (abs (source_col - target_col)) (fun _ -> 
      if source_col < target_col then '>' else '<')
  in
  
  let vertical_first = vertical_moves @ horizontal_moves @ ['A'] in
  let horizontal_first = horizontal_moves @ vertical_moves @ ['A'] in
  
  (* Special cases for top row to left column transitions (and vice versa) *)
  let top_row_buttons = ['^'; 'A'] in
  let left_col_buttons = ['<'] in
  
  match source_button, target_button with
  | src, tgt when List.mem src top_row_buttons && List.mem tgt left_col_buttons ->
      [vertical_first]
  | src, tgt when List.mem src left_col_buttons && List.mem tgt top_row_buttons ->
      [horizontal_first]
  | _ -> 
      if vertical_first = horizontal_first 
      then [vertical_first] 
      else [vertical_first; horizontal_first]





let calculate_minimum_cost_path sourceButton targetButton recursionLevel =
  (* Create a memo table to avoid redundant calculations *)
  let memo = Hashtbl.create 100 in
  
  
  (* Tail-recursive function to find the minimum length path from a list of paths *)
  let find_min_path paths =
    let rec aux current_min = function
      | [] -> current_min
      | path :: rest ->
          if List.length path < List.length current_min then
            aux path rest
          else
            aux current_min rest
    in
    match paths with
    | [] -> []
    | first :: rest -> aux first rest
  in
  
  (* Main recursive function using continuation-passing style for tail recursion *)
  let rec aux level src tgt cont =
    (* Check if we've already computed this *)
    let key = (level, src, tgt) in
    match Hashtbl.find_opt memo key with
    | Some result -> cont result
    | None ->
        if level = 0 then
          let result = [tgt] in
          Hashtbl.add memo key result;
          cont result
        else
          (* Determine if we're using the numeric keypad *)
          let isNumericKeypad =
            ('0' <= src && src <= '9')
            || ('0' <= tgt && tgt <= '9')
            || src = 'A' || tgt = 'A'
          in
          
          let possibleRoutes =
            if isNumericKeypad then
              calculate_numeric_button_route src tgt
            else
              calculate_directional_button_route src tgt
          in
          
          (* Process each route with a tail-recursive helper function *)
          let rec process_routes acc = function
            | [] -> 
                let result = find_min_path acc in
                Hashtbl.add memo key result;
                cont result
            | route :: rest_routes ->
                let routeWithA = 'A' :: route in
                let pairs = pairwise routeWithA in
                
                (* Process all pairs in a route *)
                let rec process_pairs path_acc = function
                  | [] -> process_routes (path_acc :: acc) rest_routes
                  | (curr, next) :: rest_pairs ->
                      aux (level - 1) curr next (fun subpath ->
                        process_pairs (path_acc @ subpath) rest_pairs)
                in
                process_pairs [] pairs
          in
          process_routes [] possibleRoutes
  in
  
  (* Start the computation with an identity continuation *)
  aux recursionLevel sourceButton targetButton (fun x -> x)



let calculate_minimum_cost recursion_level source_button target_button =
  (* Create a memo table to avoid redundant calculations *)
  let memo = Hashtbl.create 100 in
  
  
  (* Main recursive function using continuation-passing style for tail recursion *)
  let rec aux level src tgt cont =
    (* Check if we've already computed this *)
    let key = (level, src, tgt) in
    match Hashtbl.find_opt memo key with
    | Some result -> cont result
    | None ->
        if level = 0 then
          (* Base case: cost is 1 for directly pressing a button *)
          let result = 1L in
          Hashtbl.add memo key result;
          cont result
        else
          (* Determine if we're using the numeric keypad based on button characters *)
          let is_numeric_keypad =
            ('0' <= src && src <= '9') ||
            ('0' <= tgt && tgt <= '9') ||
            src = 'A' || tgt = 'A'
          in
          
          let possible_routes =
            if is_numeric_keypad then
              calculate_numeric_button_route src tgt
            else
              calculate_directional_button_route src tgt
          in
          
          (* Process each route with a tail-recursive helper function *)
          let rec process_routes acc_costs = function
            | [] -> 
                (* Find the minimum cost from all routes *)
                let result = 
                  match acc_costs with
                  | [] -> failwith "No routes available"
                  | [single_cost] -> single_cost
                  | first_cost :: rest_costs -> List.fold_left Int64.min first_cost rest_costs
                in
                Hashtbl.add memo key result;
                cont result
            | route :: rest_routes ->
                let route_with_a = 'A' :: route in
                let pairs = pairwise route_with_a in
                
                (* Calculate cost for this route *)
                let rec calculate_route_cost acc = function
                  | [] -> process_routes (acc :: acc_costs) rest_routes
                  | (curr, next) :: rest_pairs ->
                      aux (level - 1) curr next (fun subpath_cost ->
                        calculate_route_cost (Int64.add acc subpath_cost) rest_pairs)
                in
                calculate_route_cost 0L pairs
          in
          process_routes [] possible_routes
  in
  
  (* Start the computation with an identity continuation *)
  aux recursion_level source_button target_button (fun x -> x)



(* Helper function to trim 'A' characters from the end of a string *)
let trim_end_char c s =
  let len = String.length s in
  let rec find_first_non_c i =
    if i < 0 then 0
    else if s.[i] <> c then i + 1
    else find_first_non_c (i - 1)
  in
  String.sub s 0 (find_first_non_c (len - 1))

(* Helper function to convert string to char list *)
let string_to_char_list s =
  let rec aux i acc =
    if i >= String.length s then List.rev acc
    else aux (i+1) (s.[i] :: acc)
  in
  aux 0 []

let part1 key_codes =
  List.fold_left (fun acc key_code ->
    let navigation_cost =
      'A' :: (string_to_char_list key_code) (* Start from the 'A' button *)
      |> pairwise
      |> List.concat_map (fun (current_button, next_button) ->
           calculate_minimum_cost_path current_button next_button 3)
      |> List.length
      |> Int64.of_int
    in
    
    (* Get code value by trimming 'A' characters from the end *)
    let code_value =
      let trimmed = trim_end_char 'A' key_code in
      Int64.of_string trimmed
    in
    
    Int64.add acc (Int64.mul navigation_cost code_value)
  ) 0L key_codes



let part2 key_codes =
  List.fold_left (fun acc key_code ->
    let navigation_cost =
      'A' :: (string_to_char_list key_code) (* Start from the 'A' button *)
      |> pairwise
      |> List.fold_left (fun sum (current_button, next_button) ->
           Int64.add sum (calculate_minimum_cost (25 + 1) current_button next_button)
         ) 0L
    in
    
    (* Get code value by trimming 'A' characters from the end *)
    let code_value =
      let trimmed = trim_end_char 'A' key_code in
      Int64.of_string trimmed
    in
    
    Int64.add acc (Int64.mul navigation_cost code_value)
  ) 0L key_codes


let parse input =
  input 
  |> String.split_on_char '\n'
  |> List.map String.trim
  |> Array.of_list
  |> Array.to_list


let () =
  try
    let input = In_channel.input_all In_channel.stdin |> String.trim in
    let codes = parse input in
    
    let start_time = Unix.gettimeofday () in
    
    codes |> part1 |> Printf.printf "Part 1: %Ld\n";
    codes |> part2 |> Printf.printf "Part 2: %Ld\n";
    
    Unix.gettimeofday () -. start_time
    |> Printf.printf "Elapsed time: %.4f seconds\n"
    
  with
  | Failure msg -> Printf.printf "Error: %s\n" msg
  | e -> Printf.printf "Unexpected error: %s\n" (Printexc.to_string e)
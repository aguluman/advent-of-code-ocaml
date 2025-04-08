

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

let num_route num1 num2 =
  let (xi, xj) = get_door_keypad_position num1 in
  let (yi, yj) = get_door_keypad_position num2 in
  let di = List.init (abs (xi - yi)) (fun _ -> if xi < yi then 'v' else '^') in
  let dj = List.init (abs (xj - yj)) (fun _ -> if xj < yj then '>' else '<') in
  let tate = di @ dj @ ['A'] in
  let yoko = dj @ di @ ['A'] in

  let bottom = ['0'; 'A'] in
  let left = ['7'; '4'; '1'] in

  match (num1, num2) with
  | (num1, num2) when List.mem (num1, num2) (List.flatten (List.map (fun x -> List.map (fun y -> (x, y)) left) bottom)) -> [tate]
  | (num1, num2) when List.mem (num1, num2) (List.flatten (List.map (fun x -> List.map (fun y -> (x, y)) bottom) left)) -> [yoko]
  | _ -> List.sort_uniq compare [tate; yoko]

let dir_route dir1 dir2 =
  let (xi, xj) = get_directional_keypad_position dir1 in
  let (yi, yj) = get_directional_keypad_position dir2 in
  let di = List.init (abs (xi - yi)) (fun _ -> if xi < yi then 'v' else '^') in
  let dj = List.init (abs (xj - yj)) (fun _ -> if xj < yj then '>' else '<') in
  let tate = di @ dj @ ['A'] in
  let yoko = dj @ di @ ['A'] in

  let top = ['^'; 'A'] in
  let left = ['<'] in

  match (dir1, dir2) with
  | (dir1, dir2) when List.mem (dir1, dir2) (List.flatten (List.map (fun x -> List.map (fun y -> (x, y)) left) top)) -> [tate]
  | (dir1, dir2) when List.mem (dir1, dir2) (List.flatten (List.map (fun x -> List.map (fun y -> (x, y)) top) left)) -> [yoko]
  | _ -> List.sort_uniq compare [tate; yoko]

let rec min_cost_path level b1 b2 =
  if level = 0 then
    [b2]
  else
    let num_keypad = ('0' <= b1 && b1 <= '9') || ('0' <= b2 && b2 <= '9') in
    let route = if num_keypad then num_route b1 b2 else dir_route b1 b2 in

    let paths = List.map (fun r ->
      let pairs = ('A' :: r) |> (fun l -> 
        let rec make_pairs = function
          | [] | [_] -> []
          | x :: y :: rest -> (x, y) :: make_pairs (y :: rest)
        in make_pairs l
      ) in
      List.flatten (List.map (fun (x, y) -> min_cost_path (level - 1) x y) pairs)
    ) route in
    
    List.hd (List.sort (fun l1 l2 -> compare (List.length l1) (List.length l2)) paths)

let memo = Hashtbl.create 1000

let rec min_cost level b1 b2 =
  let key = (level, b1, b2) in
  try
    Hashtbl.find memo key
  with Not_found ->
    let value =
      if level = 0 then
        1L
      else
        let num_keypad = ('0' <= b1 && b1 <= '9') || ('0' <= b2 && b2 <= '9') in
        let route = if num_keypad then num_route b1 b2 else dir_route b1 b2 in

        let costs = List.map (fun r ->
          let pairs = ('A' :: r) |> (fun l -> 
            let rec make_pairs = function
              | [] | [_] -> []
              | x :: y :: rest -> (x, y) :: make_pairs (y :: rest)
            in make_pairs l
          ) in
          List.fold_left (fun acc (x, y) -> Int64.add acc (min_cost (level - 1) x y)) 0L pairs
        ) route in
        
        List.hd (List.sort compare costs)
    in
    Hashtbl.add memo key value;
    value

let part1 codes =
  Seq.fold_left (fun acc code ->
    let code_list = List.of_seq (String.to_seq code) in
    let cost =
      let pairs = ('A' :: code_list) |> (fun l -> 
        let rec make_pairs = function
          | [] | [_] -> []
          | x :: y :: rest -> (x, y) :: make_pairs (y :: rest)
        in make_pairs l
      ) in
      let path = List.flatten (List.map (fun (x, y) -> min_cost_path (2 + 1) x y) pairs) in
      Int64.of_int (List.length path)
    in
    let num = Int64.of_string (String.sub code 0 (String.length code - (if code.[String.length code - 1] = 'A' then 1 else 0))) in
    Int64.add acc (Int64.mul cost num)
  ) 0L codes

let part2 codes =
  Seq.fold_left (fun acc code ->
    let code_list = List.of_seq (String.to_seq code) in
    let cost =
      let pairs = ('A' :: code_list) |> (fun l -> 
        let rec make_pairs = function
          | [] | [_] -> []
          | x :: y :: rest -> (x, y) :: make_pairs (y :: rest)
        in make_pairs l
      ) in
      List.fold_left (fun acc (x, y) -> Int64.add acc (min_cost (25 + 1) x y)) 0L pairs
    in
    let num = Int64.of_string (String.sub code 0 (String.length code - (if code.[String.length code - 1] = 'A' then 1 else 0))) in
    Int64.add acc (Int64.mul cost num)
  ) 0L codes

let parse input = 
  String.split_on_char '\n' input |> List.map String.trim |> Array.of_list |> Array.to_seq



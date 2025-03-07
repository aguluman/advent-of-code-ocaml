(* Types *)
type cell =
  | Robot
  | Box
  | Wall
  | Empty


type direction =
  | Up
  | Left
  | Down
  | Right


type scaled_cell =
  | ScaledRobot
  | BoxL
  | BoxR
  | ScaledWall
  | ScaledEmpty


(* Helper functions *)
let swap i j arr =
  let arr = Array.copy arr in
  let temp = arr.(i) in
  arr.(i) <- arr.(j);
  arr.(j) <- temp;
  arr


let array_update_at idx value arr =
  let new_arr = Array.copy arr in
  new_arr.(idx) <- value;
  new_arr


let transpose matrix =
  let h = Array.length matrix in
  let w = Array.length matrix.(0) in
  Array.init w (fun i ->
    Array.init h (fun j -> matrix.(j).(i))
  )

  
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

(* Movement functions *)
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


let array_rev arr =
  let len = Array.length arr in
  Array.init len (fun i -> arr.(len - 1 - i))


let move_right map =
  let reverse arr =
    Array.map array_rev arr
  in
  map |> reverse |> move_left |> reverse

let move_up map =
  map |> transpose |> move_left |> transpose

let move_down map =
  map |> transpose |> move_right |> transpose

(* Scaled operations *)
let downgrade = function
  | ScaledRobot -> Robot
  | BoxL | BoxR -> Box
  | ScaledWall -> Wall
  | ScaledEmpty -> Empty

let find_robot_scaled map =
  map 
  |> Array.map (fun row -> Array.map downgrade row)
  |> find_robot

let rec push_left_scaled i j map =
  assert (map.(i).(j) = BoxR);
  assert (map.(i).(j-1) = BoxL);
  match map.(i).(j-2) with
  | ScaledWall -> None
  | ScaledEmpty ->
      let new_row = map.(i) |> swap (j-2) (j-1) |> swap (j-1) j in
      Some (array_update_at i new_row map)
  | BoxR ->
      (match push_left_scaled i (j-2) map with
       | None -> None
       | Some new_map ->
           assert (new_map.(i).(j-2) = ScaledEmpty);
           push_left_scaled i j new_map)
  | _ -> failwith "Invalid scaled cell"


let rec push_up_scaled i j map =
  match map.(i).(j) with
  | BoxL ->
      assert (map.(i).(j+1) = BoxR);
      (match map.(i-1).(j), map.(i-1).(j+1) with
       | ScaledWall, _ | _, ScaledWall -> None
       | ScaledEmpty, ScaledEmpty ->
           let new_row1 = map.(i-1) 
             |> array_update_at j BoxL 
             |> array_update_at (j+1) BoxR in
           let new_row2 = map.(i)
             |> array_update_at j ScaledEmpty
             |> array_update_at (j+1) ScaledEmpty in
           Some (map 
             |> array_update_at (i-1) new_row1
             |> array_update_at i new_row2)
       | BoxL, BoxR | BoxR, ScaledEmpty ->
           (match push_up_scaled (i-1) j map with
            | None -> None
            | Some new_map ->
                assert (new_map.(i-1).(j) = ScaledEmpty);
                assert (new_map.(i-1).(j+1) = ScaledEmpty);
                push_up_scaled i j new_map)
       | ScaledEmpty, BoxL ->
           (match push_up_scaled (i-1) (j+1) map with
            | None -> None
            | Some new_map ->
                assert (new_map.(i-1).(j) = ScaledEmpty);
                assert (new_map.(i-1).(j+1) = ScaledEmpty);
                push_up_scaled i j new_map)
       | BoxR, BoxL ->
           (match push_up_scaled (i-1) j map with
            | None -> None
            | Some new_map1 ->
                assert (new_map1.(i-1).(j-1) = ScaledEmpty);
                assert (new_map1.(i-1).(j) = ScaledEmpty);
                match push_up_scaled (i-1) (j+1) new_map1 with
                | None -> None
                | Some new_map2 ->
                    assert (new_map2.(i-1).(j) = ScaledEmpty);
                    assert (new_map2.(i-1).(j+1) = ScaledEmpty);
                    push_up_scaled i j new_map2)
       | _, _ -> failwith "Invalid scaled cells")
  | BoxR -> push_up_scaled i (j-1) map
  | _ -> failwith "Invalid scaled cell"

let rec move_left_scaled map =
  let (ri, rj) = find_robot_scaled map in
  match map.(ri).(rj-1) with
  | ScaledWall -> map
  | ScaledEmpty ->
      let new_row = swap (rj-1) rj map.(ri) in
      array_update_at ri new_row map
  | BoxR ->
      (match push_left_scaled ri (rj-1) map with
       | None -> map
       | Some new_map ->
           assert (new_map.(ri).(rj-1) = ScaledEmpty);
           move_left_scaled new_map)
  | _ -> failwith "Invalid scaled cell"

let move_right_scaled map =
  let reverse map =
    Array.map (fun row ->
      Array.map (function
        | BoxL -> BoxR
        | BoxR -> BoxL
        | c -> c) (array_rev row)
    ) map
  in
  map |> reverse |> move_left_scaled |> reverse

let rec move_up_scaled map =
  let (ri, rj) = find_robot_scaled map in
  match map.(ri-1).(rj) with
  | ScaledWall -> map
  | ScaledEmpty ->
      let new_row1 = array_update_at rj ScaledRobot map.(ri-1) in
      let new_row2 = array_update_at rj ScaledEmpty map.(ri) in
      map
      |> array_update_at (ri-1) new_row1
      |> array_update_at ri new_row2
  | BoxL | BoxR ->
      (match push_up_scaled (ri-1) rj map with
       | None -> map
       | Some new_map ->
           assert (new_map.(ri-1).(rj) = ScaledEmpty);
           move_up_scaled new_map)
  | _ -> failwith "Invalid scaled cell"

let move_down_scaled map =
  let reverse = array_rev in
  map |> reverse |> move_up_scaled |> reverse

(* Scale up operation *)
let scale_up map =
  Array.map (fun row ->
    Array.init (2 * Array.length row) (fun j ->
      let orig_idx = j / 2 in
      let is_odd = j mod 2 = 1 in
      match row.(orig_idx), is_odd with
      | Robot, false -> ScaledRobot
      | Robot, true -> ScaledEmpty
      | Box, false -> BoxL
      | Box, true -> BoxR
      | Wall, _ -> ScaledWall
      | Empty, _ -> ScaledEmpty
    )
  ) map

(* Part 1 and Part 2 *)
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

let part2 (map, moves) =
  let scaled_map = scale_up map in
  let final_map = 
    Array.fold_left (fun acc dir ->
      match dir with
      | Up -> move_up_scaled acc
      | Left -> move_left_scaled acc
      | Down -> move_down_scaled acc
      | Right -> move_right_scaled acc
    ) scaled_map (Array.of_seq moves)
  in
  let score = ref 0 in
  for i = 0 to Array.length final_map - 1 do
    for j = 0 to Array.length final_map.(0) - 1 do
      if final_map.(i).(j) = BoxL then
        score := !score + (100 * i + j)
    done
  done;
  !score


let parse_map input =
  let lines = String.split_on_char '\n' input |> List.filter (fun s -> s <> "") in
  if List.length lines = 0 then
    failwith "Empty map input";
  
  let height = List.length lines in
  let width = String.length (List.hd lines) in
  let cells = Array.make_matrix height width Empty in
  
  List.iteri (fun row line ->
    String.iteri (fun col ch ->
      cells.(row).(col) <- match ch with
        | '@' -> Robot
        | 'O' -> Box
        | '#' -> Wall
        | '.' -> Empty
        | c -> failwith (Printf.sprintf "Invalid character in map: %c" c)
    ) line
  ) lines;
  
  cells  (* Return just the 2D array, not a record *)

(** Parse complete input into warehouse and movement sequence
    @param input Raw puzzle input string
    @return Tuple of initial warehouse state and movement directions *)
let parse input =
  let parts = String.split_on_char '\n' input 
              |> List.filter (fun s -> s <> "") 
              |> List.partition (fun s -> 
                   String.length s > 0 && 
                   (String.contains "@O#." s.[0] || 
                    (String.length s > 1 && 
                     (s.[0] = '@' || s.[0] = 'O' || s.[0] = '#' || s.[0] = '.')))) in
  
  match parts with
  | (map_lines, move_lines) ->
      let map_str = String.concat "\n" map_lines in
      Printf.printf "Map lines found: %d\n" (List.length map_lines);
      
      let moves_str = String.concat "" move_lines in
      Printf.printf "Moves string length: %d\n" (String.length moves_str);
      
      let warehouse = parse_map map_str in
      let moves = moves_str
                 |> String.to_seq
                 |> Seq.filter_map (function
                     | '^' -> Some Up
                     | 'v' -> Some Down
                     | '<' -> Some Left
                     | '>' -> Some Right
                     | _ -> None) in
      (warehouse, moves)


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
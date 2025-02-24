(** Types representing cells and movement directions *)
type cell =
  | Robot
  | Box
  | Wall
  | Empty

type dir =
  | Up
  | Left
  | Down
  | Right




(** Move box recursively in a direction *)
let move_with_box (map : cell array array) ri rj di dj =
  let rec move_box_tail ri rj di dj cont =
    let ni, nj = ri + di, rj + dj in
    let h, w = Array.length map, Array.length map.(0) in
    
    if ni < 0 || ni >= h || nj < 0 || nj >= w then
      cont false
    else match map.(ni).(nj) with
      | Wall -> cont false
      | Empty -> 
          map.(ni).(nj) <- Box;
          map.(ri).(rj) <- Empty;
          cont true
      | Box ->
          move_box_tail ni nj di dj (fun result ->
            if result then begin
              map.(ni).(nj) <- Box;
              map.(ri).(rj) <- Empty;
              cont true
            end else
              cont false)
      | Robot -> cont false
  in
  move_box_tail ri rj di dj (fun x -> x)

(** Move robot in a direction *)
let move_robot map ri rj di dj =
  let ni, nj = ri + di, rj + dj in
  let h, w = Array.length map, Array.length map.(0) in
  
  if ni < 0 || ni >= h || nj < 0 || nj >= w then
    (ri, rj)
  else match map.(ni).(nj) with
    | Wall -> (ri, rj)
    | Empty ->
        map.(ni).(nj) <- Robot;
        map.(ri).(rj) <- Empty;
        (ni, nj)
    | Box ->
        if move_with_box map ni nj di dj then begin
          map.(ni).(nj) <- Robot;
          map.(ri).(rj) <- Empty;
          (ni, nj)
        end else
          (ri, rj)
    | Robot -> (ri, rj)

(** Find robot position in map *)
let find_robot map =
  let h, w = Array.length map, Array.length map.(0) in
  let found = ref (-1, -1) in
  for i = 0 to h - 1 do
    for j = 0 to w - 1 do
      if map.(i).(j) = Robot then
        found := (i, j)
    done
  done;
  !found

(** Part 1 solution *)
let part1 (map, moves) =
  let ri, rj = ref (fst (find_robot map)), ref (snd (find_robot map)) in
  
  List.iter (fun move ->
    let di, dj = match move with
      | Up -> (-1, 0)
      | Down -> (1, 0)
      | Left -> (0, -1)
      | Right -> (0, 1)
    in
    let new_ri, new_rj = move_robot map !ri !rj di dj in
    ri := new_ri;
    rj := new_rj
  ) moves;
  
  let sum = ref 0 in
  for i = 0 to Array.length map - 1 do
    for j = 0 to Array.length map.(0) - 1 do
      if map.(i).(j) = Box then
        sum := !sum + (100 * i + j)
    done
  done;
  !sum

(** Parse map from string *)
let parse_map input =
  input
  |> Str.split (Str.regexp "\n")
  |> List.filter (fun s -> String.trim s <> "")
  |> List.map (fun row ->
      Array.init (String.length row) (fun i ->
        match row.[i] with
        | '@' -> Robot
        | 'O' -> Box
        | '#' -> Wall
        | '.' -> Empty
        | c -> failwith (Printf.sprintf "Unexpected map char: %c" c)
      )
    )
  |> Array.of_list

(** Parse input into map and moves *)
let parse input =
  let trimmed = Str.global_replace (Str.regexp "\r") "" input in
  let parts = Str.split_delim (Str.regexp "\n\n") trimmed in
  match parts with
  | [map_part; moves_part] ->
      let map = parse_map map_part in
      let moves =
        moves_part
        |> Str.split (Str.regexp "\n")
        |> List.filter (fun s -> String.trim s <> "")
        |> List.map (fun line ->
            String.to_seq line
            |> List.of_seq
            |> List.map (function
              | '^' -> Up
              | '<' -> Left
              | 'v' -> Down
              | '>' -> Right
              | c -> failwith (Printf.sprintf "Unknown move: %c" c))
          )
        |> List.concat
      in
      (map, moves)
  | _ -> failwith "Invalid input format. Expected map and moves separated by a blank line."



let () =
  In_channel.input_all In_channel.stdin
  |> String.trim
  |> parse
  |> (fun map ->
      let start_time = Unix.gettimeofday () in
      map |> part1 |> Printf.printf "Part 1: %d\n";
      Unix.gettimeofday () -. start_time)
  |> Printf.printf "Elapsed time: %.4f seconds\n"
(** Types representing cells and movement directions. *)
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

(** In-place swap of elements at indices [i] and [j] in [arr]. *)
let swap i j (arr : 'a array) =
  let tmp = arr.(i) in
  arr.(i) <- arr.(j);
  arr.(j) <- tmp

(** Creates a new 2D array that is the transpose of [matrix]. *)
let transpose (matrix : 'a array array) =
  let h = Array.length matrix in
  if h = 0 then [||]
  else
    let w = Array.length matrix.(0) in
    Array.init w (fun col ->
      Array.init h (fun row ->
        matrix.(row).(col)
      )
    )

let array_rev_inplace arr =
  let len = Array.length arr in
  for i = 0 to (len / 2) - 1 do
    let tmp = arr.(i) in
    arr.(i) <- arr.(len - i - 1);
    arr.(len - i - 1) <- tmp
  done



(** Finds the (row, col) of the Robot in [map]. 
    Assumes exactly one Robot is present. 
    Raises Not_found if none is found. *)
let find_robot (map : cell array array) =
  let h = Array.length map in
  let w = Array.length map.(0) in
  let found = ref None in
  for i = 0 to h - 1 do
    for j = 0 to w - 1 do
      if map.(i).(j) = Robot then found := Some (i, j)
    done
  done;
  match !found with
  | Some coords -> coords
  | None -> raise Not_found



(** Attempts to push a Box left from (i, j). 
    Returns [Some map] if successful, or [None] if blocked by a Wall. *)
let rec push_left (i, j) (map : cell array array) =
  let row = map.(i) in
  (* We know map.(i).(j) = Box by caller's contract *)
  match row.(j - 1) with
  | Wall -> None
  | Empty ->
      swap (j - 1) j row; (* box moves left one cell *)
      Some map
  | Box ->
      (* Recursively attempt to push the box at (i, j - 1) further left *)
      begin
        match push_left (i, j - 1) map with
        | None -> None
        | Some new_map ->
            (* Now the position (i, j - 1) should be Empty; so push again. *)
            let row2 = new_map.(i) in
            assert (row2.(j - 1) = Empty);
            (* Move the current box left. *)
            ignore (push_left (i, j) new_map);
            Some new_map
      end
  | _ -> failwith "Unexpected cell while pushing left."

(** Moves the Robot one cell left if possible, 
    possibly pushing Boxes along the way. *)
let rec move_left (map : cell array array) =
  let (ri, rj) = find_robot map in
  let row = map.(ri) in
  match row.(rj - 1) with
  | Wall -> map  (* blocked by a Wall *)
  | Empty ->
      swap (rj - 1) rj row;  (* Robot moves left *)
      map
  | Box ->
      begin
        match push_left (ri, rj - 1) map with
        | Some new_map ->
            let row_check = new_map.(ri) in
            assert (row_check.(rj - 1) = Empty);
            move_left new_map
        | None -> map
      end
  | Robot -> failwith "Unexpected second robot."

(** Moves the Robot one cell right (using [move_left] on a reversed map). *)
let move_right (map : cell array array) =
  let reverse (m : cell array array) =
        Array.map (fun row ->
      let row_copy = Array.copy row in
      array_rev_inplace row_copy;
      row_copy
    ) m
    |> Array.to_list
    |> List.rev
    |> Array.of_list
  in
  let reversed_map = reverse map in
  let updated = move_left reversed_map in
  ignore (reverse updated);
  map

(** Moves the Robot one cell up. *)
let move_up (map : cell array array) =
  (* We reuse move_left by transposing the map, moving, then transposing back. *)
  let transposed = transpose map in
  ignore (move_left transposed);
  transpose transposed

(** Moves the Robot one cell down (using [move_right] on a transposed map). *)
let move_down (map : cell array array) =
  let transposed = transpose map in
  ignore (move_right transposed);
  transpose transposed

(** Part 1: Apply all moves, then compute a score based on the positions of Boxes. *)
let part1 (map, moves) =
  let apply_move m map =
    match m with
    | Up -> move_up map
    | Left -> move_left map
    | Down -> move_down map
    | Right -> move_right map
  in
  (* Apply each move in-place on [map]. *)
  List.iter (fun mv -> ignore (apply_move mv map)) moves;
  (* Compute the sum of 100*i + j for each Box in the final map. *)
  let h = Array.length map
  and w = Array.length map.(0) in
  let sum = ref 0 in
  for i = 0 to h - 1 do
    for j = 0 to w - 1 do
      if map.(i).(j) = Box then sum := !sum + (100 * i + j)
    done
  done;
  !sum

(** Parses a string-based map into a 2D array of [cell]. *)
let parse_map (input : string) =
  let lines = 
    input
    |> Str.global_replace (Str.regexp "\r") ""  (* remove Windows \r if present *)
    |> Str.split (Str.regexp "\n") 
  in
  let lines = List.filter (fun s -> String.trim s <> "") lines in
  let array_of_line line =
    let row = Array.init (String.length line) (fun i ->
      match line.[i] with
      | '@' -> Robot
      | 'O' -> Box
      | '#' -> Wall
      | '.' -> Empty
      | c -> failwith (Printf.sprintf "Unexpected map char: %c" c)
    ) in
    row
  in
  Array.of_list (List.map array_of_line lines)

(** Parses input into a (map, moves) tuple:
    - Map: 2D array of [cell]
    - Moves: list of [dir].
    The input is expected to have two sections separated by a blank line:
    1) Map section
    2) Moves section
*)
let parse (input : string) : (cell array array * dir list) =
  let trimmed = Str.global_replace (Str.regexp "\r") "" input in
  let parts = Str.split_delim (Str.regexp "\n\n") trimmed in
  match parts with
  | [map_part; moves_part] ->
      let map = parse_map map_part in
      let moves_lines = 
        moves_part
        |> Str.split (Str.regexp "\n")
        |> List.map String.trim
        |> List.filter (fun s -> s <> "")
      in
      (* Convert lines like "^<v>" into multiple moves [Up; Left; Down; ...] *)
      let moves =
        moves_lines
        |> List.map (fun line ->
             line
             |> String.to_seq
             |> List.of_seq
             |> List.map (function
                  | '^' -> Up
                  | '<' -> Left
                  | 'v' -> Down
                  | '>' -> Right
                  | c -> failwith (Printf.sprintf "Unknown move: %c" c)
                )
           )
        |> List.flatten
      in
      (map, moves)
  | _ -> failwith "Invalid input format. Expected map and moves separated by a blank line."

(** Example main function. In a real puzzle, you might read from stdin and print results. *)
let () =
  In_channel.input_all In_channel.stdin
  |> String.trim
  |> parse
  |> (fun map ->
  
      let start_time = Unix.gettimeofday () in
      
      map |> part1 |> Printf.printf "Part 1: %d\n";
      
      Unix.gettimeofday () -. start_time)
  |> Printf.printf "Elapsed time: %.4f seconds\n"
  
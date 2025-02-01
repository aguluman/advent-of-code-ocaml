(** Generate a list of integers from [start] to [end_inclusive] *)
let generate_range_sequence start end_inclusive =
  let rec build_sequence current accumulator =
    if current > end_inclusive then List.rev accumulator
    else build_sequence (current + 1) (current :: accumulator)
  in build_sequence start []

(** Create [all possible ordered pairs] between [elements of two lists] *)
let create_ordered_pairs list1 list2 =
  List.concat (List.map (fun x -> List.map (fun y -> (x, y)) list2) list1)

(** Implement unfold to generate a list from state transitions *)
let unfold generator initial_state =
  let rec accumulate generated current_state =
    match generator current_state with
    | None -> List.rev generated
    | Some (value, new_state) -> accumulate (value :: generated) new_state
  in accumulate [] initial_state

(** Calculate positions [along the line from start] to [end within grid bounds] *)
let calculate_antinode_positions grid_size (start_row, start_col) (end_row, end_col) =
  if (start_row, start_col) = (end_row, end_col) then []
  else
    let row_step = end_row - start_row 
  in
    let col_step = end_col - start_col 
  in
    unfold (fun (current_row, current_col) ->
      let next_row = current_row + row_step 
    in
      let next_col = current_col + col_step 
    in
      if next_row >= 0 && next_row < grid_size && next_col >= 0 && next_col < grid_size
      then 
        Some ((next_row, next_col), (next_row, next_col))
      else 
        None)
 (end_row, end_col)

(** [Core solver] function for both problem parts *)
let solve_grid grid position_mapper =
  let grid_size = Array.length grid in
  assert (Array.for_all (fun row -> Array.length row = grid_size) grid);
  
  let valid_chars = 
    List.append 
      (generate_range_sequence (Char.code '0') (Char.code '9'))
      (List.append 
         (generate_range_sequence (Char.code 'a') (Char.code 'z'))
         (generate_range_sequence (Char.code 'A') (Char.code 'Z')))
    |> List.map Char.chr
  in
  
  let find_char_positions char =
    create_ordered_pairs 
      (generate_range_sequence 0 (grid_size - 1)) 
      (generate_range_sequence 0 (grid_size - 1))
    |> List.filter (fun (row, col) -> grid.(row).(col) = char)
  in
  
  valid_chars
  |> List.map (fun char ->
      let char_locations = find_char_positions char 
    in
      let mapped_points = 
        create_ordered_pairs char_locations char_locations
        |> List.concat_map (fun ((r1, c1), (r2, c2)) -> 
            position_mapper grid_size (r1, c1) (r2, c2))
      in
      List.sort_uniq compare mapped_points)
  |> List.concat
  |> List.sort_uniq compare
  |> List.length

(** [Part 1]: Consider first valid antinode position *)
let part1 grid =
  solve_grid grid (fun size (r1, c1) (r2, c2) ->
    match calculate_antinode_positions size (r1, c1) (r2, c2) with
    | [] -> []
    | first_pos :: _ -> [first_pos])

(** [Part 2]: Include endpoint and all valid antinodes *)
let part2 grid =
  solve_grid grid (fun size (r1, c1) (r2, c2) ->
    (r2, c2) :: calculate_antinode_positions size (r1, c1) (r2, c2))

(** Convert input string to 2D character array *)
let parse input =
  input
  |> String.split_on_char '\n'
  |> List.filter (fun line -> String.trim line <> "")
  |> List.map (fun row -> 
      Array.of_list (List.init (String.length row) (String.get row)))
  |> Array.of_list

(** Main execution *)
let () =
  let input = In_channel.input_all In_channel.stdin |> String.trim in
  let grid = parse input in

  let timer_start = Unix.gettimeofday () in

  grid |> part1 |> Printf.printf "Part 1: %d\n";
  grid |> part2 |> Printf.printf "Part 2: %d\n";

  let timer_end = Unix.gettimeofday () in

  let elapsed = timer_end -. timer_start in

  Printf.printf "Elapsed time: %.4f seconds\n" (elapsed)
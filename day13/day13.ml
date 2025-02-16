(** Types for representing game machines and their components *)
type button = {
  add_x: int;
  add_y: int
}

type prize = {
  x: int;
  y: int
}

type machine = {
  button_a: button;
  button_b: button;
  prize: prize
}




(** Find minimum value in a list, returning None for empty lists
    @param lst Input list of values
    @return Option containing minimum value *)
let try_min lst = 
  match lst with
  | [] -> None
  | xs -> Some (List.fold_left min (List.hd xs) (List.tl xs))




(** Calculate total cost for part 1
    @param machines Sequence of game machines
    @return Sum of minimum costs for each machine *)
let part1 machines =
  machines
  |> List.fold_left (fun acc { button_a; button_b; prize } ->
    let costs =
      List.init 101 (fun i ->
        List.init 101 (fun j ->
          let x = button_a.add_x * i + button_b.add_x * j in
          let y = button_a.add_y * i + button_b.add_y * j in
          if x = prize.x && y = prize.y then
            Some (i * 3 + j)
          else None))
      |> List.concat
      |> List.filter_map (fun x -> x)
    in
    match try_min costs with
    | None -> acc
    | Some min_cost -> acc + min_cost
  ) 0



  
(** Calculate total cost for part 2 using determinant method
    @param machines List of game machines
    @return Sum of costs calculated with large number offset *)
let part2 machines =
  machines
  |> List.fold_left (fun acc { button_a; button_b; prize } ->
    (* Offset prize coordinates by large number *)
    let px = Int64.(add (of_int prize.x) 10000000000000L) in
    let py = Int64.(add (of_int prize.y) 10000000000000L) in
    
    (* Calculate determinant *)
    let det = Int64.(
      sub 
        (mul (of_int button_a.add_x) (of_int button_b.add_y))
        (mul (of_int button_b.add_x) (of_int button_a.add_y))
    ) in
    
    if Int64.equal det 0L then
      acc
    else
      (* Calculate numerators for i and j using 64-bit integers throughout *)
      let num_i = Int64.(
        sub
          (mul (of_int button_b.add_y) px)
          (mul (of_int button_b.add_x) py)
      ) in
      let num_j = Int64.(
        sub
          (mul (of_int (-button_a.add_y)) px)
          (mul (of_int (-button_a.add_x)) py)
      ) in
      
      (* Check if solution exists *)
      if Int64.(equal (rem num_i det) 0L && equal (rem num_j det) 0L) then
        let i = Int64.div num_i det in
        let j = Int64.div num_j det in
        Int64.(add acc (add (mul i 3L) j))
      else
        acc
  ) 0L





(** Parse input string into list of machines
    @param input Raw input string
    @return List of parsed machine records *)
let parse input =
    let parse_button line expected_button =
    (* Printf.printf "Debug: Parsing button line '%s' with pattern '%s'\n" line expected_button; *)
    let pattern = Printf.sprintf "^%s: X\\+\\([0-9]+\\), Y\\+\\([0-9]+\\)$" expected_button 
  in
    let regexp = Str.regexp pattern 
  in
    if Str.string_match regexp line 0 then
      let button = { 
        add_x = int_of_string (Str.matched_group 1 line);
        add_y = int_of_string (Str.matched_group 2 line) 
      } 
    in
      (* Printf.printf "Debug: Successfully parsed button: x=%d, y=%d\n" button.add_x button.add_y; *)
      button
    else
      failwith (Printf.sprintf "Invalid button format for %s: %s" expected_button line)
  in

  let parse_prize line =
    let pattern = "^Prize: X=\\([0-9]+\\), Y=\\([0-9]+\\)$" 
  in
    let regexp = Str.regexp pattern in
    if Str.string_match regexp line 0 then
      { x = int_of_string (Str.matched_group 1 line);
        y = int_of_string (Str.matched_group 2 line) }
    else
      failwith (Printf.sprintf "Invalid prize format: %s" line)
  in

  input
  |> Str.split (Str.regexp "\n\n")
  |> List.map (fun section ->
      let lines = 
        section
        |> Str.split (Str.regexp "\n")
        |> List.map String.trim
        |> List.filter (fun s -> String.length s > 0)
      in
      match lines with
      | [button_a_line; button_b_line; prize_line] ->
          { button_a = parse_button button_a_line "Button A";
            button_b = parse_button button_b_line "Button B";
            prize = parse_prize prize_line }
      | _ -> 
          Printf.printf "Debug: Found %d lines in section: %s\n" (List.length lines) section;
          failwith (Printf.sprintf "Invalid section format: Expected exactly 3 lines, got %d" (List.length lines)))



(** Main program entry point *)
let () =
  let input = In_channel.input_all In_channel.stdin |> String.trim in
  let machines = parse input in
  
  let start_time = Unix.gettimeofday () in

  machines |> part1 |> Printf.printf "Part 1: %d\n";
  machines |> part2 |> Printf.printf "Part 2: %Ld\n";

  Unix.gettimeofday () -. start_time
  |> Printf.printf "Elapsed time: %.4f seconds\n"
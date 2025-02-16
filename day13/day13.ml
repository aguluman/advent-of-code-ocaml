(** Day 13: Claw Contraption
    Solving a puzzle about claw machines where:
    - Each machine has two buttons (A and B) that move the claw in X,Y directions
    - Button A costs 3 tokens, Button B costs 1 token
    - Need to position claw exactly above prizes
    - Part 1: Find minimum tokens needed to win all possible prizes
    - Part 2: Same but with prize coordinates offset by 10^13 *)



(** Represents a button's movement pattern. *)
type claw_button = {
  move_x: int;  (* Units to move along X axis *)
  move_y: int   (* Units to move along Y axis *)
}

(** Represents prize coordinates. *)
type prize_location = {
  target_x: int;
  target_y: int
}

(** Represents a complete claw machine configuration. *)
type claw_machine = {
  button_a: claw_button;
  button_b: claw_button;
  prize: prize_location
}





(** Find [minimum value] in a list safely.
    @param values Input list of token costs.
    @return Option containing minimum cost [Some] or [None] if no solution exists. *)
let find_minimum_cost values = 
  match values with
  | [] -> None
  | costs -> Some (List.fold_left min (List.hd costs) (List.tl costs))




(** Calculate minimum tokens needed for Part 1.
    Tries combinations of button presses (max 100 each).
    @param machines List of claw machines to solve.
    @return Sum of minimum token costs for winnable prizes. *)
let calculate_minimum_tokens machines =
  machines
  |> List.fold_left (fun total_tokens { button_a; button_b; prize } ->
    let possible_costs =
      List.init 101 (fun press_a ->
        List.init 101 (fun press_b ->
          let final_x = button_a.move_x * press_a + button_b.move_x * press_b 
        in
          let final_y = button_a.move_y * press_a + button_b.move_y * press_b 
        in
          if 
            final_x = prize.target_x && final_y = prize.target_y 
          then
            Some (press_a * 3 + press_b)  (* Cost calculation: A=3 tokens, B=1 token *)
          else 
            None))
      |> List.concat
      |> List.filter_map (fun x -> x)
    in
    match find_minimum_cost possible_costs with
    | None -> total_tokens        (* Machine is unsolvable *)
    | Some cost -> total_tokens + cost
  ) 0
  


(** Calculate minimum tokens for Part 2 using linear equation solving.
    For large coordinates (offset by 10^13), we solve the system:
    
    button_a.move_x * press_a + button_b.move_x * press_b = prize.target_x + 10^13
    button_a.move_y * press_a + button_b.move_y * press_b = prize.target_y + 10^13
    
    Using Cramer's rule where determinant is:
    det = (button_a.move_x * button_b.move_y) - (button_b.move_x * button_a.move_y)
    
    @param machines List of claw machines to solve
    @return Sum of minimum token costs (as Int64) for winnable prizes *)
let calculate_large_coordinate_tokens machines =
  machines
  |> List.fold_left (fun total_tokens { button_a; button_b; prize } ->
    (* Step 1:  Offset prize coordinates by 10^13 *)
    let offset_x = Int64.(add (of_int prize.target_x) 10000000000000L) 
  in
    let offset_y = Int64.(add (of_int prize.target_y) 10000000000000L) 
  in
    
    (* Step 2: Calculate determinant using Cramer's rule
       |button_a.move_x  button_b.move_x|
       |button_a.move_y  button_b.move_y| *)
    (* Cross-Multiply, then becomes :
       det = (button_a.move_x * button_b.move_y) - (button_b.move_x * button_a.move_y) *)
    let determinant = Int64.(
      sub 
        (mul (of_int button_a.move_x) (of_int button_b.move_y))
        (mul (of_int button_b.move_x) (of_int button_a.move_y))
    ) 
  in  
    
    (* Step 3: If determinant is 0, system has no unique solution *)
    if 
      Int64.equal determinant 0L 
    then
      total_tokens
    else
      (* Step 4: Calculate press counts using Cramer's rule *)
      let press_a_numerator = Int64.(
        sub
          (mul (of_int button_b.move_y) offset_x)
          (mul (of_int button_b.move_x) offset_y)
      ) 
    in
      let press_b_numerator = Int64.(
        sub
          (mul (of_int (-button_a.move_y)) offset_x)
          (mul (of_int (-button_a.move_x)) offset_y)
      ) 
    in
      
      (* Step 5: Check if solution has integer press counts *)
      if 
        Int64.(equal (rem press_a_numerator determinant) 0L && 
                equal (rem press_b_numerator determinant) 0L) 
      then
        let press_a = Int64.div press_a_numerator determinant 
      in
        let press_b = Int64.div press_b_numerator determinant 
      in
        (* Step 6: Calculate total cost: press_a * 3 + press_b * 1 *)
        Int64.(add total_tokens (add (mul press_a 3L) press_b))
      else
        total_tokens
  ) 0L



  
(** Parse input string into list of machines. 
    Format example:
    Button A: X+94, Y+34
    Button B: X+22, Y+67
    Prize: X=8400, Y=5400
    @param input Raw input string with machine configurations.
    @return List of parsed claw_machine records. *)
let parse input =
  let parse_button line expected_button =
    (* Printf.printf "Debug: Parsing button line '%s' with pattern '%s'\n" line expected_button; *)
    let pattern = Printf.sprintf "^%s: X\\+\\([0-9]+\\), Y\\+\\([0-9]+\\)$" expected_button 
  in
    let regexp = Str.regexp pattern 
  in
    if 
      Str.string_match regexp line 0 
    then
      { 
        move_x = int_of_string (Str.matched_group 1 line);
        move_y = int_of_string (Str.matched_group 2 line) 
      }
    (* Printf.printf "Debug: Successfully parsed button: x=%d, y=%d\n" button.add_x button.add_y; *)  
    else
      failwith (Printf.sprintf "Invalid button format for %s: %s" expected_button line)
  in

  let parse_prize line =
    let pattern = "^Prize: X=\\([0-9]+\\), Y=\\([0-9]+\\)$" 
  in
    let regexp = Str.regexp pattern 
  in
    if 
      Str.string_match regexp line 0 
    then
      { 
        target_x = int_of_string (Str.matched_group 1 line);
        target_y = int_of_string (Str.matched_group 2 line) 
      }
    else
      failwith (Printf.sprintf "Invalid prize format for Prize: %s" line)
  in

  input
  |> Str.split (Str.regexp "\n\n")
  |> List.map (fun machine_config ->
      let lines = 
        machine_config
        |> Str.split (Str.regexp "\n")
        |> List.map String.trim
        |> List.filter (fun s -> String.length s > 0)
      in
      match lines with
      | [button_a_line; button_b_line; prize_line] ->
          { 
            button_a = parse_button button_a_line "Button A";
            button_b = parse_button button_b_line "Button B";
            prize = parse_prize prize_line 
          }
      | _ -> 
          Printf.printf "\nDebug: Found %d lines in section: %s\n" 
            (List.length lines) machine_config;
          failwith (Printf.sprintf "Invalid machine configuration: Expected 3 lines, got %d" 
            (List.length lines)))



            
(** Main program entry point *)
let () =
  In_channel.input_all In_channel.stdin
  |> String.trim
  |> parse
  |> (fun machines ->
  
      let start_time = Unix.gettimeofday () in
      
      machines |> calculate_minimum_tokens |> Printf.printf "Part 1: %d\n";
      machines |> calculate_large_coordinate_tokens |> Printf.printf "Part 2: %Ld\n";
      
      Unix.gettimeofday () -. start_time)
  |> Printf.printf "Elapsed time: %.4f seconds\n"
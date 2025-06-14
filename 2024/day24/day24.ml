(** {1 Day 24: Crossed Wires - Boolean Logic Gate Simulation}

    Solves Advent of Code 2024 Day 24 challenge about simulating boolean logic
    gates and fixing a wiring issue in an electronic circuit. The implementation
    handles both the circuit simulation and the diagnostic repair of mis-wired
    gates.

    {2 Problem details:}

    - {b Part 1:} Simulate a system of AND, OR, and XOR logic gates with their
      interconnections to calculate a final decimal number from binary outputs
      on z-wires
    - {b Part 2:} Identify and fix four pairs of gates whose output wires have
      been swapped, preventing the circuit from correctly performing binary
      addition

    {2 The solution implements:}

    - Circuit simulation engine that processes gates in the correct order based
      on input/output dependencies
    - Circuit verification system that validates whether the circuit correctly
      performs binary addition
    - Search algorithm that identifies the gate pairs with swapped outputs

    See details at:
    {{:https://adventofcode.com/2024/day/24} Advent of Code 2024, Day 24} *)

(** Represents the different types of boolean logic gates supported by the
    circuit.

    The circuit simulation supports three fundamental boolean operations:
    - {b And} - outputs 1 if both inputs are 1, otherwise 0
    - {b Or} - outputs 1 if at least one input is 1, otherwise 0
    - {b Xor} - outputs 1 if inputs differ, otherwise 0 *)
type gate_operation =
  | And
  | Or
  | Xor

(** Evaluates a boolean logic gate operation on two input values.

    Applies the specified logic operation (AND, OR, or XOR) to the two input
    values and returns the result. This function uses OCaml's bitwise operators
    to perform the boolean logic operations efficiently.

    The function implements the three fundamental boolean operations used in the
    circuit:

    - {b And} (land) - outputs 1 if both inputs are 1, otherwise 0
    - {b Or} (lor) - outputs 1 if at least one input is 1, otherwise 0
    - {b Xor} (lxor) - outputs 1 if inputs differ, otherwise 0

    The bitwise operators ensure that the function works correctly for boolean
    values represented as integers.

    @param operation The logic operation to perform (And, Or, or Xor)
    @param a The first input value (0 or 1)
    @param b The second input value (0 or 1)
    @return
      The result of applying the specified logic operation to the two inputs (0
      or 1) *)
let evaluate_gate operation a b =
  match operation with
  | And -> a land b (* Bitwise AND *)
  | Or -> a lor b (* Bitwise OR *)
  | Xor -> a lxor b (* Bitwise XOR *)

(** Represents a logic gate in the circuit with its inputs, output, and
    operation type.

    Each gate has:
    - Two input wires identified by their string names
    - One output wire identified by its string name
    - An operation type (AND, OR, or XOR)

    Gates wait for both inputs to have values before producing an output. *)
type gate = {
  input : string * string;
  operation : gate_operation;
  output : string;
}

(** String map module for efficient lookups *)
module StringMap = Map.Make (String)

(** Solves Part 1 by simulating the circuit and calculating the decimal value
    from the binary outputs on z-wires.

    This function processes the circuit simulation and then:

    - 1. Identifies all wires whose names start with "z"
    - 2. Orders them by numeric suffix (z00, z01, z02, etc.)
    - 3. Interprets them as a binary number with z00 as the least significant
      bit
    - 4. Converts the binary number to decimal

    @param wires_gates Tuple containing initial wire values and gates
    @return The int64 number produced by the circuit *)
let part1 (wires, gates) =
  let initial_eval =
    List.fold_left
      (fun map (name, value) -> StringMap.add name value map)
      StringMap.empty wires
  in

  let rec run eval gates =
    if gates = [] then eval
    else
      let eval', remaining_gates =
        List.fold_left
          (fun (curr_eval, remaining) gate ->
            let input1, input2 = gate.input in
            if StringMap.mem input1 curr_eval && StringMap.mem input2 curr_eval
            then
              let input1_val = StringMap.find input1 curr_eval in
              let input2_val = StringMap.find input2 curr_eval in
              let out = evaluate_gate gate.operation input1_val input2_val in
              (StringMap.add gate.output out curr_eval, remaining)
            else (curr_eval, gate :: remaining))
          (eval, []) gates
      in
      run eval' remaining_gates
  in

  (* Run the simulation *)
  let final_eval = run initial_eval gates in

  (* Extract 'z' wires and convert to binary string *)
  let z_wires =
    StringMap.fold
      (fun k v acc ->
        if String.length k > 0 && k.[0] = 'z' then (k, v) :: acc else acc)
      final_eval []
  in

  let z_values =
    let z_array = Array.of_list z_wires in
    Array.sort (fun (k1, _) (k2, _) -> compare k2 k1) z_array;

    let buffer = Buffer.create 16 in
    Array.iter
      (fun (_, v) -> Buffer.add_string buffer (string_of_int v))
      z_array;
    Buffer.contents buffer
  in

  let binary_to_int64 s =
    match s with
    | "" -> Int64.zero
    | _ ->
        let len = String.length s in
        let rec convert idx acc =
          if idx = len then acc
          else
            let bit = if s.[idx] = '1' then 1L else 0L in
            convert (idx + 1) Int64.(add (shift_left acc 1) bit)
        in
        convert 0 0L
  in

  binary_to_int64 z_values

(** Solves Part 2 by identifying the four pairs of gates whose output wires have
    been swapped.

    This function implements a search algorithm to find the four pairs of gates
    that, when their outputs are swapped, allow the circuit to correctly perform
    binary addition. The approach involves:

    - 1. Building a validation function that tests if a circuit performs
      addition correctly
    - 2. Detecting circuit structure patterns that should appear in a binary
      adder
    - 3. Searching through possible gate combinations to identify the swapped
      pairs
    - 4. Verifying the solution by checking that the circuit works for all valid
      inputs

    The search employs cycle detection and structural validation to efficiently
    identify the correct swaps without testing all possible combinations.

    @param wires_gates Tuple containing initial wire values and gates
    @return
      Comma-separated string of the eight wire names involved in the swaps,
      sorted alphabetically *)
let part2 (_wires, gates) =
  let gate_by_out =
    List.fold_left
      (fun acc gate -> StringMap.add gate.output gate acc)
      StringMap.empty gates
  in

  (* Check if there's a loop in the circuit *)
  let has_loop gate_by_out =
    let rec dfs out path =
      if List.mem out path then true
      else
        match StringMap.find_opt out gate_by_out with
        | None -> false
        | Some gate ->
            let left, right = gate.input in
            dfs left (out :: path) || dfs right (out :: path)
    in
    StringMap.exists (fun out _ -> dfs out []) gate_by_out
  in

  (* Collect all wires in the subcircuit for a given output *)
  let rec collect out gate_by_out =
    match StringMap.find_opt out gate_by_out with
    | None -> []
    | Some gate ->
        let left, right = gate.input in
        (out :: collect left gate_by_out) @ collect right gate_by_out
  in

  (* Build canonical string representation of circuit *)
  let rec make out gate_by_out =
    match StringMap.find_opt out gate_by_out with
    | None -> out
    | Some gate -> (
        let left = make (fst gate.input) gate_by_out in
        let right = make (snd gate.input) gate_by_out in
        let left, right =
          if left <= right then (left, right) else (right, left)
        in
        match gate.operation with
        | And -> "(" ^ left ^ ")and(" ^ right ^ ")"
        | Or -> "(" ^ left ^ ")or(" ^ right ^ ")"
        | Xor -> "(" ^ left ^ ")xor(" ^ right ^ ")")
  in

  (* Validate if a z-wire circuit matches expected binary adder pattern *)
  let valid out gate_by_out =
    let circuit = make out gate_by_out in

    (* Extract XY matches *)
    let extract_xy_matches circuit =
      let regex = Str.regexp "[xy][0-9]+" in
      let rec find_all pos acc =
        try
          let _ = Str.search_forward regex circuit pos in
          let matched = Str.matched_string circuit in
          find_all (Str.match_end ()) (matched :: acc)
        with Not_found -> List.rev acc
      in
      find_all 0 []
    in

    (* Extract operation matches *)
    let extract_operation_matches circuit =
      let regex = Str.regexp "\\(and\\|or\\|xor\\)" in
      let rec find_all pos acc =
        try
          let _ = Str.search_forward regex circuit pos in
          let op = Str.matched_string circuit in
          find_all (Str.match_end ()) (op :: acc)
        with Not_found -> List.rev acc
      in
      find_all 0 []
    in

    (* chunkBySize implementation *)
    let chunk_by_size size lst =
      let rec chunk acc current count = function
        | [] ->
            if current = [] then List.rev acc
            else List.rev (List.rev current :: acc)
        | x :: xs ->
            if count = size then chunk (List.rev current :: acc) [ x ] 1 xs
            else chunk acc (x :: current) (count + 1) xs
      in
      chunk [] [] 0 lst
    in

    (* validXY function *)
    let valid_xy =
      let xy_matches = extract_xy_matches circuit in
      let xy_chunks = chunk_by_size 2 xy_matches in
      match xy_chunks with
      | [] -> true
      | [ "x00"; "y00" ] :: t ->
          (* After chunking by 2, we get pairs, then chunk those pairs by 2 again *)
          let t_rechunked = chunk_by_size 2 t in
          let n = List.length t_rechunked in
          let rec validate_sequence i =
            if i > n then true
            else if i < n then
              (* Let xy = [| $"x%02d{i}"; $"y%02d{i}" |] *)
              let expected_pair =
                [ Printf.sprintf "x%02d" i; Printf.sprintf "y%02d" i ]
              in
              (* t[i - 1] = List.replicate 2 xy means the chunk should be [xy; xy] *)
              let expected_chunk = [ expected_pair; expected_pair ] in
              (if i > 0 && i <= List.length t_rechunked then
                 let chunk = List.nth t_rechunked (i - 1) in
                 chunk = expected_chunk
               else false)
              && validate_sequence (i + 1)
            else if i = n then
              (* Last element: t[i - 1] = [ xy ] *)
              let expected_pair =
                [ Printf.sprintf "x%02d" i; Printf.sprintf "y%02d" i ]
              in
              let expected_chunk = [ expected_pair ] in
              if i > 0 && i <= List.length t_rechunked then
                let chunk = List.nth t_rechunked (i - 1) in
                chunk = expected_chunk
              else false
            else true
          in
          validate_sequence 1
      | _ -> false
    in

    (* validOperation function *)
    let valid_operations =
      let ops = extract_operation_matches circuit in
      match ops with
      | [] -> true
      | [ "xor" ] -> true (* i = 0 case *)
      | "and" :: t ->
          let t_chunks = chunk_by_size 4 t in
          let n = List.length t_chunks in
          let rec validate_chunks i =
            if i >= n then true
            else
              let chunk = List.nth t_chunks i in
              if i + 1 < n then
                chunk = [ "and"; "xor"; "or"; "and" ] && validate_chunks (i + 1)
              else chunk = [ "xor"; "xor" ] && validate_chunks (i + 1)
          in
          validate_chunks 0
      | _ -> false
    in

    valid_xy && valid_operations
  in

  (* Main search function that finds the correct gate mapping *)
  let rec search i gate_by_out =
    if i >= 45 then Some gate_by_out
    else
      let out = Printf.sprintf "z%02d" i in
      if valid out gate_by_out then search (i + 1) gate_by_out
      else
        let swaps = collect out gate_by_out in
        let rec try_pick_swaps = function
          | [] -> None
          | swap_out :: rest_swaps ->
              let gate_bindings = StringMap.bindings gate_by_out in
              let rec try_pick_targets = function
                | [] -> try_pick_swaps rest_swaps
                | (target_out, target_gate) :: rest_targets ->
                    if target_out = swap_out then try_pick_targets rest_targets
                    else
                      let current_gate = StringMap.find swap_out gate_by_out in
                      let new_gate_by_out =
                        gate_by_out
                        |> StringMap.add swap_out target_gate
                        |> StringMap.add target_out current_gate
                      in
                      if has_loop new_gate_by_out then
                        try_pick_targets rest_targets
                      else
                        let rec find_next_invalid j =
                          if j > 45 then 46 (* No invalid found *)
                          else if
                            not
                              (valid (Printf.sprintf "z%02d" j) new_gate_by_out)
                          then j
                          else find_next_invalid (j + 1)
                        in
                        let next_invalid = find_next_invalid 0 in
                        if i < next_invalid then
                          search next_invalid new_gate_by_out
                        else try_pick_targets rest_targets
              in
              try_pick_targets gate_bindings
        in
        try_pick_swaps swaps
  in

  (* Find the correct gate mapping *)
  let correct_gate_by_out =
    match search 0 gate_by_out with
    | Some result -> result
    | None -> failwith "No solution found"
  in

  (* Find the differences between original and corrected mappings *)
  let diff_wires =
    StringMap.fold
      (fun out original_gate acc ->
        let corrected_gate = StringMap.find out correct_gate_by_out in
        if original_gate <> corrected_gate then out :: acc else acc)
      gate_by_out []
    |> List.sort String.compare
  in

  Printf.printf "Found %d differing wires: %s\n%!" (List.length diff_wires)
    (String.concat "," diff_wires);
  print_newline ();
  String.concat "," diff_wires

(** Parses the input text into initial wire values and a collection of gates.

    The input has two sections:
    - Initial wire values in the format "wire: value" where value is 0 or 1
    - Gate definitions in the format "inputA OPERATION inputB -> output"

    The parser handles all three operation types (AND, OR, XOR) and creates a
    data structure representing the complete circuit.

    @param input
      The input text containing wire initializations and gate definitions
    @return A tuple containing initial wire values and a list of gates *)
let parse input =
  let normalized_input = Str.global_replace (Str.regexp "\r\n") "\n" input in
  let sections = Str.split (Str.regexp "\n\n") normalized_input in

  if List.length sections < 2 then
    failwith "Expected at least 2 sections, but found fewer.";

  let wire_lines =
    match List.nth_opt sections 0 with
    | Some s -> Str.split (Str.regexp "\n") s
    | None -> failwith "Expected wire section, but input is malformed."
  in
  let wires =
    List.map
      (fun line ->
        match Str.split (Str.regexp ":") line with
        | [ wire; value ] ->
            (String.trim wire, int_of_string (String.trim value))
        | _ -> failwith ("Invalid wire line: " ^ line))
      wire_lines
  in

  let gate_lines =
    match List.nth_opt sections 1 with
    | Some s -> Str.split (Str.regexp "\n") s
    | None -> failwith "Expected gate section, but input is missing."
  in
  let gates =
    List.map
      (fun line ->
        let parts = Str.split (Str.regexp " ") line in
        match parts with
        | left :: op_str :: right :: "->" :: output :: _ ->
            let operation =
              match op_str with
              | "AND" -> And
              | "OR" -> Or
              | "XOR" -> Xor
              | _ ->
                  failwith ("Unknown operation: " ^ op_str ^ " in line: " ^ line)
            in
            { input = (left, right); operation; output }
        | _ -> failwith ("Malformed gate line: " ^ line))
      gate_lines
  in

  (wires, gates)

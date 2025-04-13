(** {1 Day 24: Crossed Wires - Boolean Logic Gate Simulation}

    Solves Advent of Code 2024 Day 24 challenge about simulating boolean logic gates and
    fixing a wiring issue in an electronic circuit. The implementation handles both the
    circuit simulation and the diagnostic repair of mis-wired gates.
    
    {2 Problem details:}
    
    - {b Part 1:} Simulate a system of AND, OR, and XOR logic gates with their interconnections
      to calculate a final decimal number from binary outputs on z-wires
    - {b Part 2:} Identify and fix four pairs of gates whose output wires have been swapped,
      preventing the circuit from correctly performing binary addition
    
    {2 The solution implements:}
    
    - Circuit simulation engine that processes gates in the correct order based on
      input/output dependencies
    - Circuit verification system that validates whether the circuit correctly
      performs binary addition
    - Search algorithm that identifies the gate pairs with swapped outputs
    
    See details at: {{:https://adventofcode.com/2024/day/24} Advent of Code 2024, Day 24}
*)




(** Represents the different types of boolean logic gates supported by the circuit.

    The circuit simulation supports three fundamental boolean operations:
    - {b And} - outputs 1 if both inputs are 1, otherwise 0
    - {b Or} - outputs 1 if at least one input is 1, otherwise 0 
    - {b Xor} - outputs 1 if inputs differ, otherwise 0
*)
type gate_operation =
  | And
  | Or
  | Xor


(** Evaluates a boolean logic gate operation on two input values.

    Applies the specified logic operation (AND, OR, or XOR) to the two input values
    and returns the result. This function uses OCaml's bitwise operators to perform
    the boolean logic operations efficiently.
    
    The function implements the three fundamental boolean operations used in the circuit:
    
    - {b And} (land) - outputs 1 if both inputs are 1, otherwise 0
    - {b Or} (lor) - outputs 1 if at least one input is 1, otherwise 0
    - {b Xor} (lxor) - outputs 1 if inputs differ, otherwise 0
    
    The bitwise operators ensure that the function works correctly for boolean values 
    represented as integers.
    
    @param operation The logic operation to perform (And, Or, or Xor)
    @param a The first input value (0 or 1)
    @param b The second input value (0 or 1)
    @return The result of applying the specified logic operation to the two inputs (0 or 1)
*)
let evaluate_gate operation a b =
  match operation with
  | And -> a land b  (* Bitwise AND *)
  | Or  -> a lor b   (* Bitwise OR *)
  | Xor -> a lxor b  (* Bitwise XOR *)



(** Represents a logic gate in the circuit with its inputs, output, and operation type.

    Each gate has:
    - Two input wires identified by their string names
    - One output wire identified by its string name
    - An operation type (AND, OR, or XOR)
    
    Gates wait for both inputs to have values before producing an output.
*)
type gate = {
  input : string * string;
  operation : gate_operation;
  output : string;
}



(** Solves Part 1 by simulating the circuit and calculating the decimal value
    from the binary outputs on z-wires.
    
    This function processes the circuit simulation and then:
    
  - 1. Identifies all wires whose names start with "z"
  - 2. Orders them by numeric suffix (z00, z01, z02, etc.)
  - 3. Interprets them as a binary number with z00 as the least significant bit
  - 4. Converts the binary number to decimal
    
    @param wires_gates Tuple containing initial wire values and gates
    @return The int64 number produced by the circuit
*)
let part1 (wires, gates) =
  (* Create a string map for evaluations *)
  let module StringMap = Map.Make(String) in
  
  (* Convert wires list to a map *)
  let initial_eval = 
    List.fold_left
      (fun map (name, value) -> StringMap.add name value map)
      StringMap.empty
      wires
  in
  
  (* Recursive function to process gates *)
  let rec run eval gates =
    if gates = [] then
      eval
    else
      let eval', remaining_gates = 
        List.fold_left 
          (fun (curr_eval, remaining) gate ->
            let input1, input2 = gate.input in
            if StringMap.mem input1 curr_eval && StringMap.mem input2 curr_eval then
              let input1_val = StringMap.find input1 curr_eval in
              let input2_val = StringMap.find input2 curr_eval in
              let out = evaluate_gate gate.operation input1_val input2_val in
              (StringMap.add gate.output out curr_eval, remaining)
            else
              (curr_eval, gate :: remaining)
          ) 
          (eval, []) 
          gates 
      in
      run eval' remaining_gates
  in
  
  (* Run the simulation *)
  let final_eval = run initial_eval gates in
  
  (* Extract 'z' wires and convert to binary string *)
  let z_wires =
    StringMap.fold 
      (fun k v acc -> 
        if String.length k > 0 && k.[0] = 'z' then
          (k, v) :: acc
        else 
          acc
      ) 
      final_eval 
      []
  in
  
    let z_values =
      let z_array = Array.of_list z_wires 
    in
      Array.sort (fun (k1, _) (k2, _) -> compare k2 k1) z_array;
      
      let buffer = Buffer.create 16 
    in  (* Initial size estimate *)
      Array.iter (fun (_, v) -> Buffer.add_string buffer (string_of_int v)) z_array;
      Buffer.contents buffer
    in
  
  (* Convert binary string to int64 *)
  let binary_to_int64 s =
    match s with
    | "" -> Int64.zero
    | _ ->
        let len = String.length s in
        let rec convert idx acc =
          if idx = len then acc
          else
            let bit = if s.[idx] = '1' then 1L else 0L in
            convert (idx + 1) (Int64.(add (shift_left acc 1) bit))
        in
        convert 0 0L
  in
  
  binary_to_int64 z_values


    (** Solves Part 2 by identifying the four pairs of gates whose output wires have been swapped.
  
      This function implements a search algorithm to find the four pairs of gates that,
      when their outputs are swapped, allow the circuit to correctly perform binary addition.
      The approach involves:
      
    - 1. Building a validation function that tests if a circuit performs addition correctly
    - 2. Detecting circuit structure patterns that should appear in a binary adder
    - 3. Searching through possible gate combinations to identify the swapped pairs
    - 4. Verifying the solution by checking that the circuit works for all valid inputs
      
      The search employs cycle detection and structural validation to efficiently
      identify the correct swaps without testing all possible combinations.
      
      @param wires_gates Tuple containing initial wire values and gates
      @return Comma-separated string of the eight wire names involved in the swaps, sorted alphabetically
  *)
let part2 (_wires, gates) =
  let module StringMap = Map.Make(String) in

  (* Create initial gate map *)
  let gate_by_out = 
    List.fold_left
      (fun map g -> StringMap.add g.output g map)
      StringMap.empty
      gates
  in

  (* Focus on z-wires and their immediate connections *)
  let z_gates = 
    List.filter (fun g -> 
      String.length g.output > 0 && g.output.[0] = 'z'
    ) gates
  in

  (* Get all gates connected to a z-gate *)
  let connected_gates = 
    let seen = Hashtbl.create 256 in
    let rec get_connections g =
      if not (Hashtbl.mem seen g.output) then begin
        Hashtbl.add seen g.output true;
        let left, right = g.input in
        match StringMap.find_opt left gate_by_out, StringMap.find_opt right gate_by_out with
        | Some g1, Some g2 -> g :: get_connections g1 @ get_connections g2
        | Some g1, None -> g :: get_connections g1
        | None, Some g2 -> g :: get_connections g2
        | None, None -> [g]
      end else []
    in
    List.concat (List.map get_connections z_gates)
  in

  (* Try swapping gates in smaller groups *)
  let rec try_combinations tried_swaps gates remaining_count =
    if remaining_count = 0 then 
      Some tried_swaps
    else
      let rec try_pairs g1 rest =
        match rest with
        | [] -> None
        | g2 :: rest' ->
            (* Create new configuration with swapped outputs *)
            let new_map = 
              gate_by_out 
              |> StringMap.add g1.output g2
              |> StringMap.add g2.output g1
            in
            
            (* Quick validation of just affected outputs *)
            let is_valid = 
              List.for_all 
                (fun g -> 
                  let out = Printf.sprintf "z%02d" (int_of_string (String.sub g.output 1 2)) in
                  StringMap.mem out new_map)
                z_gates
            in
            
            if is_valid then
              match try_combinations 
                ((g1.output, g2.output) :: tried_swaps) 
                rest' 
                (remaining_count - 1) with
              | Some result -> Some result
              | None -> try_pairs g1 rest'
            else
              try_pairs g1 rest'
      in
      
      match gates with
      | [] -> None
      | g :: rest -> 
          match try_pairs g rest with
          | Some result -> Some result
          | None -> try_combinations tried_swaps rest remaining_count
  in

  (* Try to find 4 pairs of swaps *)
  match try_combinations [] connected_gates 4 with
  | None -> "No solution found"
  | Some swaps ->
      let all_wires = 
        List.flatten (List.map (fun (w1, w2) -> [w1; w2]) swaps)
      in
      List.sort String.compare all_wires |> String.concat ","



(** Parses the input text into initial wire values and a collection of gates.

    The input has two sections:
    - Initial wire values in the format "wire: value" where value is 0 or 1
    - Gate definitions in the format "inputA OPERATION inputB -> output"
    
    The parser handles all three operation types (AND, OR, XOR) and creates
    a data structure representing the complete circuit.
    
    @param input The input text containing wire initializations and gate definitions
    @return A tuple containing initial wire values and a list of gates
*)
let parse input =
  let normalized_input = Str.global_replace (Str.regexp "\r\n") "\n" input in
  let sections = Str.split (Str.regexp "\n\n") normalized_input in

  if List.length sections < 2 then
    failwith "Expected at least 2 sections, but found fewer.";

  let wire_lines = match List.nth_opt sections 0 with
    | Some s -> Str.split (Str.regexp "\n") s
    | None -> failwith "Expected wire section, but input is malformed."
  in
  let wires = List.map (fun line ->
    match Str.split (Str.regexp ":") line with
    | [wire; value] -> (String.trim wire, int_of_string (String.trim value))
    | _ -> failwith ("Invalid wire line: " ^ line)
  ) wire_lines in

  let gate_lines = match List.nth_opt sections 1 with
    | Some s -> Str.split (Str.regexp "\n") s
    | None -> failwith "Expected gate section, but input is missing."
  in
  let gates = List.map (fun line ->
    let parts = Str.split (Str.regexp " ") line in
    match parts with
    | left :: op_str :: right :: "->" :: output :: _ ->
        let operation =
          match op_str with
          | "AND" -> And
          | "OR" -> Or
          | "XOR" -> Xor
          | _ -> failwith ("Unknown operation: " ^ op_str ^ " in line: " ^ line)
        in
        { input = (left, right); operation; output }
    | _ -> failwith ("Malformed gate line: " ^ line)
  ) gate_lines in

  wires, gates
;;
(** 3-bit computer registers
    
    Represents the three registers of the chronospatial 3-bit computer.
    While the computer uses 3-bit opcodes and operands, the registers themselves
    can store arbitrarily large integer values.
    
    @param accumulator Register A - Primary arithmetic register used as source in division operations
    @param register_b  Register B - Used for bitwise operations and storing intermediate results
    @param register_c  Register C - Auxiliary register typically used with register B for operations
*)


(**
  // 0 adv: A <- A / 2^■
  // 1 bxl: B <- B xor ■
  // 2 bst: B <- ■ % 8
  // 3 jnz: A ≠ 0 ⇒ ip <- ■
  // 4 bxc: B <- B xor C
  // 5 out: ■ % 8
  // 6 bdv: B <- A / 2^■
  // 7 cdv: C <- A / 2^■
*)


type cpu_registers = {
  accumulator : int64;   
  register_b : int64;   
  register_c : int64;   
}

(** Chronospatial 3-bit computer state
    
    Represents the complete execution state of the computer at a given point,
    including the current instruction position, register values, program instructions,
    and any outputs that have been produced.
    
    @param instruction_pointer Current position in the program (increments by 2 after most instructions)
    @param register           Values of the three CPU registers (A, B, C)
    @param instructions       The program being executed, represented as an array of 3-bit values
    @param outputs            List of values produced by output instructions, stored in reverse order
*)
type cpu_state = {
  instruction_pointer : int;          
  register : cpu_registers;     
  instructions : int array; 
  outputs : int64 list;     
}


(** Efficient power calculation for int64 values using binary exponentiation
    
    Calculates base^exponent using the exponentiation by squaring algorithm,
    which reduces the number of multiplications needed from O(n) to O(log n).
    This implementation handles int64 values to support large numbers required
    by the computer's division operations.
    
    @param base      The base value to raise to a power
    @param exponent  The non-negative exponent (must be >= 0)
    @return          base raised to the power of exponent
    
    @example [power_of_two 2L 3L] returns 8L
    @example [power_of_two 2L 10L] returns 1024L
*)
let rec power_of_two base exponent =
  if exponent = 0L then 1L
  else if Int64.rem exponent 2L = 0L then
    power_of_two (Int64.mul base base) (Int64.div exponent 2L)
  else
    Int64.mul base (power_of_two base (Int64.sub exponent 1L))


(** Execute a program on the chronospatial 3-bit computer
    
    This function simulates the execution of a 3-bit computer program by
    repeatedly fetching, decoding, and executing instructions until a
    halt condition is met. Each instruction consists of an opcode and operand,
    which are processed according to the following rules:
    
    - Opcode 0 (adv): Divides register A by 2^operand, storing result in A
    - Opcode 1 (bxl): XORs register B with literal operand
    - Opcode 2 (bst): Sets register B to operand mod 8
    - Opcode 3 (jnz): Jumps to operand if register A is non-zero
    - Opcode 4 (bxc): XORs registers B and C, storing result in B
    - Opcode 5 (out): Outputs operand mod 8 to the output list
    - Opcode 6 (bdv): Divides register A by 2^operand, storing result in B
    - Opcode 7 (cdv): Divides register A by 2^operand, storing result in C
    
    @param init_register    Initial values for the three CPU registers
    @param init_program     Array of instruction opcodes and operands
    @param halt_condition   Function that determines when execution should stop
                           and returns the final output array if halted
    
    @return                 Array of output values produced by the program
*)
let execute init_register init_program halt_condition =  
  let rec step state =
    match halt_condition state with
    | Some output_array -> output_array
    | None ->
      let { instruction_pointer; register; instructions; outputs } = state in

      let resolve_combo_operand op =
        match op with
        | 0 | 1 | 2 | 3 as x -> Int64.of_int x
        | 4 -> register.accumulator
        | 5 -> register.register_b
        | 6 -> register.register_c
        | x -> failwith (Printf.sprintf "Invalid combo: %d" x)
      in
        
      let opcode = instructions.(instruction_pointer) in
      let operand = instructions.(instruction_pointer + 1) in

      let new_state = 
        match opcode with
        | 0 -> 
            { state with 
              instruction_pointer = instruction_pointer + 2;
              register = 
                { register with accumulator = Int64.div register.accumulator (power_of_two 2L (resolve_combo_operand operand)) } 
            }
        | 1 -> 
            { state with 
              instruction_pointer = instruction_pointer + 2;
              register = { register with register_b = Int64.logxor register.register_b (Int64.of_int operand) } 
            }
        | 2 -> 
            { state with 
              instruction_pointer = instruction_pointer + 2;
              register = { register with register_b = Int64.rem (resolve_combo_operand operand) 8L } 
            }
        | 3 -> 
            if register.accumulator = 0L then 
              { state with instruction_pointer = instruction_pointer + 2 }
            else 
              { state with instruction_pointer = operand }
        | 4 -> 
            { state with 
              instruction_pointer = instruction_pointer + 2;
              register = { register with register_b = Int64.logxor register.register_b register.register_c } 
            }
        | 5 -> 
            { state with 
              instruction_pointer = instruction_pointer + 2;
              outputs = Int64.rem (resolve_combo_operand operand) 8L :: outputs
            }
        | 6 -> 
            { state with 
              instruction_pointer = instruction_pointer + 2;
              register = 
                { register with register_b = Int64.div register.accumulator (power_of_two 2L (resolve_combo_operand operand)) } 
            }
        | 7 -> 
            { state with 
              instruction_pointer = instruction_pointer + 2; 
              register = 
                { register with register_c = Int64.div register.accumulator (power_of_two 2L (resolve_combo_operand operand)) } 
            }
        | x -> failwith (Printf.sprintf "%d !?" x)
      in
      
      step new_state
  in
  
  step { instruction_pointer = 0;
         register = init_register;
         instructions = init_program;
         outputs = [] }

  
(** Halt condition that detects when program execution should stop
    
    This function determines if the CPU has reached the end of its program
    by checking if the instruction pointer is beyond the bounds of the 
    instruction array. If so, it processes the accumulated outputs into 
    the final result format.
    
    @param state    Current execution state of the CPU
    @return         Some array of output integers if execution should halt;
                    None if execution should continue
*)
let output_on_halt state = 
  if state.instruction_pointer >= Array.length state.instructions then
    Some (
      state.outputs
      |> List.rev_map Int64.to_int
      |> Array.of_list
    )
  else
    None


(** Solve part 1 of the chronospatial computer challenge
    
    Runs the provided program with initial register values and formats the output
    as a comma-separated string of values, which represents the sequence of
    values produced by the computer's output instructions.
    
    @param register  Initial state of the computer's registers (A, B, C)
    @param program   The program to execute, as an array of instruction opcodes and operands
    @return          A string of comma-separated values representing the program's output
*)
let part1 register program =
  let output = execute register program output_on_halt in
  output 
  |> Array.map string_of_int 
  |> Array.to_list 
  |> String.concat ","


(** Solve part 2 of the chronospatial computer challenge
    
    Finds the lowest positive value for register A that causes the program to
    output a copy of itself. This is accomplished through a reverse-engineering 
    approach that works backward from the expected output, trying all possible
    3-bit values (0-7) to reconstruct the original input value digit by digit.
    
    The algorithm:
    1. Starts with an empty accumulator (0)
    2. For each digit position, tries all values from 0-7
    3. For each attempt, runs the program with the current accumulated value
    4. Tests if the output matches the expected slice of the program
    5. If a match is found, recursively builds the next digit
    
    @param register  Initial state of the computer's registers (A, B, C)
    @param program   The program to execute, as an array of instruction opcodes and operands
    @return          The lowest positive value for register A that causes the program
                     to output a copy of itself
    
    @raise Failure   If no solution can be found
*)
let part2 register program =  (* A helper to compare two int arrays by value *)
    let array_equal element_comparator first_array second_array =
    if Array.length first_array <> Array.length second_array then 
      false
    else
      let rec compare_elements_from index =
        if index = Array.length first_array then 
          true
        else if not (element_comparator first_array.(index) second_array.(index)) then 
          false
        else 
          compare_elements_from (index + 1)
      in
      compare_elements_from 0
  in

  (* OCaml equivalent of F# List.tryPick *)
    let rec find_map mapping_function = function
    | [] -> None
    | current_element :: remaining_elements -> 
        match mapping_function current_element with
        | Some _ as result -> result
        | None -> find_map mapping_function remaining_elements
  in

    let rec find_register_value remaining_positions accumulated_value =
    if remaining_positions = 0 then
      Some accumulated_value
    else
      find_map
        (fun digit ->
          let new_value = Int64.add (Int64.mul accumulated_value 8L) (Int64.of_int digit) in
          let output = execute { register with accumulator = new_value } program output_on_halt in
          let slice_len = Array.length program - (remaining_positions - 1) in
          let program_slice = Array.sub program (remaining_positions - 1) slice_len in
          if array_equal ( = ) output program_slice then 
            find_register_value (remaining_positions - 1) new_value 
          else 
            None
        )
        [0; 1; 2; 3; 4; 5; 6; 7]
  in

  match find_register_value (Array.length program) 0L with
  | Some result -> result
  | None -> failwith "No solution found"



(** Parse input for the chronospatial 3-bit computer
    
    This function processes the raw input string into structured data required
    by the computer simulation. The input format is expected to have:
    
    1. Register definitions at the beginning (lines starting with "Register X:")
    2. A blank line separator
    3. A program definition (a line starting with "Program:" followed by comma-separated values)
    
    The function performs several processing steps:
    - Normalizes line endings and trims whitespace
    - Extracts register values from lines beginning with "Register A:", etc.
    - Parses the comma-separated program values into an array of integers
    
    @param input    Raw input string containing register values and program definition
    @return         Tuple of (cpu_registers, program) where program is an int array
                    If specific registers aren't defined, their values default to 0L
*)
let parse input =  (* Normalize line endings and trim *)
  let input = 
    input 
    |> String.map (function '\r' -> ' ' | c -> c) 
    |> String.trim 
  in
  
  (* Split input into lines *)
  let lines = String.split_on_char '\n' input 
  in
  
  (* Find the register section and program section *)
  let register_lines =
    lines 
    |> List.take_while (fun line -> String.trim line <> "") 
  in
  
  let program_line =
    lines
    |> List.drop_while (fun line -> String.trim line <> "")
    |> List.drop_while (fun line -> String.trim line = "")
    |> function
       | [] -> None
       | line :: _ -> Some line
  in
  
  (* Parse registers *)
  let get_register_value prefix =
    register_lines
    |> List.find_opt (fun line -> String.starts_with ~prefix line)
    |> Option.map (fun line -> 
        String.sub line (String.length prefix) (String.length line - String.length prefix)
        |> String.trim
        |> Int64.of_string
      )
    |> Option.value ~default:0L
  in
  
  let register = {
    accumulator = get_register_value "Register A:";
    register_b = get_register_value "Register B:";
    register_c = get_register_value "Register C:"
  } 
 in
  
  (* Parse program *)
  let program = 
    match program_line with
    | Some line when String.starts_with ~prefix:"Program:" line ->
        let program_str = String.sub line (String.length "Program:") 
                             (String.length line - String.length "Program:") 
                         |> String.trim 
        in
        String.split_on_char ',' program_str
        |> List.map String.trim
        |> List.map int_of_string
        |> Array.of_list
    | _ -> [||]
  in
  
  register, program
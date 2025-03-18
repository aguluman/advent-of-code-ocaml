open OUnit2
open Day17

(** Example inputs from the challenge *)
let example_input_1 = "Register A: 729
Register B: 0
Register C: 0

Program: 0,1,5,4,3,0"

let example_input_2 = "Register A: 2024
Register B: 0
Register C: 0

Program: 0,3,5,4,3,0"


(** Helper function to create test cases for part1 *)
let make_part1_test name input expected =
  name >:: (fun _ ->
    let register, program = parse input in
    assert_equal expected (part1 register program)
      ~printer:(fun s -> s))


(** Helper function to create test cases for part2 *)
let make_part2_test name input expected =
  name >:: (fun _ ->
    let register, program = parse input in
    assert_equal expected (part2 register program |> Int64.to_int)
      ~printer:string_of_int)


(** Helper to test individual register execution - checks register values *)
let make_register_value_test name register_a register_b register_c program expected_reg_a expected_reg_b expected_reg_c =
  name >:: (fun _ ->
    let register = { 
      accumulator = Int64.of_int register_a;
      register_b = Int64.of_int register_b;
      register_c = Int64.of_int register_c;
    } in
    let program = Array.of_list program in
    
    (* Create a custom halt condition that captures the final register state *)
    let final_registers = ref None in
    let halt_with_registers state = 
      if state.instruction_pointer >= Array.length state.instructions then begin
        final_registers := Some state.register;
        Some [||]  (* Return empty output just to complete execution *)
      end else
        None
    in
    
    let _ = execute register program halt_with_registers in
    match !final_registers with
    | Some regs -> 
        assert_equal expected_reg_a (Int64.to_int regs.accumulator) ~printer:string_of_int ~msg:"Register A";
        assert_equal expected_reg_b (Int64.to_int regs.register_b) ~printer:string_of_int ~msg:"Register B";
        assert_equal expected_reg_c (Int64.to_int regs.register_c) ~printer:string_of_int ~msg:"Register C"
    | None -> 
        assert_failure "Program didn't halt properly"
  )


(** Helper to test output-producing programs *)
let make_output_test name register_a register_b register_c program expected_output =
  name >:: (fun _ ->
    let register = { 
      accumulator = Int64.of_int register_a;
      register_b = Int64.of_int register_b;
      register_c = Int64.of_int register_c;
    } in
    let program = Array.of_list program in
    let output = execute register program output_on_halt in
    assert_equal expected_output (Array.to_list output) ~printer:(fun list ->
      list |> List.map string_of_int |> String.concat ","))



(** Test cases for specific register operations *)
let register_tests = [
  make_register_value_test "C=9, program 2,6 sets B to 1" 0 0 9 [2; 6] 0 1 9;
  make_register_value_test "B=29, program 1,7 sets B to 26" 0 29 0 [1; 7] 0 26 0;
  make_register_value_test "B=2024, C=43690, program 4,0 sets B to 44354" 0 2024 43690 [4; 0] 0 44354 43690;
  make_output_test "A=10, program 5,0,5,1,5,4 outputs 0,1,2" 10 0 0 [5; 0; 5; 1; 5; 4] [0; 1; 2];
  make_output_test "A=2024, program 0,1,5,4,3,0 outputs sequence" 2024 0 0 [0; 1; 5; 4; 3; 0] [4; 2; 5; 6; 7; 7; 7; 7; 3; 1; 0];
]



(** Additional instruction tests based on challenge descriptions *)
let instruction_tests = [
  (* adv instruction divides register A by 2^operand *)
  make_register_value_test "adv instruction divides register A by 2^operand" 
    16 0 0 [0; 2] 4 0 0;  (* A = 16 / 2^2 = 16/4 = 4 *)
  
  (* bxl instruction performs XOR on register B with literal *)
  make_register_value_test "bxl instruction performs XOR on register B with literal" 
    0 10 0 [1; 7] 0 (10 lxor 7) 0;
  
  (* bst instruction sets register B to operand mod 8 *)
  make_register_value_test "bst instruction sets register B to operand mod 8" 
    0 0 0 [2; 3] 0 3 0;  (* 3 % 8 = 3 *)
  
  (* jnz instruction jumps when A is not zero *)
  make_register_value_test "jnz instruction jumps when A is not zero" 
    1 0 0 [3; 2; 5; 0; 2; 3] 1 3 0;  (* Should jump to the "bst" and set B=3 *)
  
  (* jnz instruction doesn't jump when A is zero *)
  make_register_value_test "jnz instruction doesn't jump when A is zero" 
    0 0 0 [3; 2; 2; 3] 0 3 0;  (* Should execute the "bst" after jnz and set B=3 *)
  
  (* bxc instruction performs XOR on register B with register C *)
  make_register_value_test "bxc instruction performs XOR on register B with register C" 
    0 10 7 [4; 0] 0 (10 lxor 7) 7;
  
  (* bdv instruction divides A by 2^operand and stores result in B *)
  make_register_value_test "bdv instruction divides A by 2^operand and stores in B" 
    16 0 0 [6; 2] 16 4 0;  (* B = 16 / 2^2 = 16/4 = 4, A remains 16 *)
  
  (* cdv instruction divides A by 2^operand and stores result in C *)
  make_register_value_test "cdv instruction divides A by 2^operand and stores in C" 
    16 0 0 [7; 2] 16 0 4;  (* C = 16 / 2^2 = 16/4 = 4, A remains 16 *)
]


(** Part 1 test cases *)
let part1_tests = [
  make_part1_test "example_1_part1" example_input_1 "4,6,3,5,6,3,5,2,1,0";
]


(** Part 2 test cases *)
let part2_tests = [
  make_part2_test "example_2_part2" example_input_2 117440;
]


(** Register/CPU state inspection *)
let register_inspection =
  "register_inspection" >:: (fun _ ->
    let register, program = parse example_input_1 in
    Printf.printf "\nInitial register state:\n";
    Printf.printf "  Accumulator: %Ld\n" register.accumulator;
    Printf.printf "  Register B: %Ld\n" register.register_b;
    Printf.printf "  Register C: %Ld\n" register.register_c;
    Printf.printf "Program length: %d\n" (Array.length program);
    Printf.printf "Program content: ";
    Array.iter (fun op -> Printf.printf "%d " op) program;
    Printf.printf "\n")



(** Complete test suite - updated with instruction tests *)
let suite = "Day17 Test Suite" >::: [
  "Register Operation Tests" >::: register_tests;
  "Instruction Tests" >::: instruction_tests;
  "Part 1 Tests" >::: part1_tests;
  "Part 2 Tests" >::: part2_tests;
  register_inspection;
]

(** Run the tests *)
let () = run_test_tt_main suite



(** Main Entry Point for the chronospatial 3-bit computer simulation
    
    This function orchestrates the entire solution workflow:
    1. Reads the input data from standard input
    2. Parses it into register values and program instructions
    3. Solves part 1 (determining program output with given register values)
    4. Solves part 2 (finding the register A value that causes the program to output itself)
    5. Measures and displays the execution time
    
    Input is expected to be provided via stdin in the format described in the parse function.
    Results are printed to stdout, including:
    - Part 1 result: A comma-separated string of output values
    - Part 2 result: The numeric value needed for register A
    - Performance metrics showing total execution time in seconds
*)
let () =
  try
    let input = In_channel.input_all In_channel.stdin |> String.trim in
    let register, program = parse input in
    
    let start_time = Unix.gettimeofday () in
    
    part1 register program |> Printf.printf "\nPart 1: %s\n";
    part2 register program |> Int64.to_int |> Printf.printf "Part 2: %d\n";
    
    Unix.gettimeofday () -. start_time
    |> Printf.printf "Elapsed time: %.8f seconds\n"
    
  with
  | Failure msg -> Printf.printf "Error: %s\n" msg
  | e -> Printf.printf "Unexpected error: %s\n" (Printexc.to_string e)
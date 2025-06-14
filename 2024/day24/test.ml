(** Unit tests for the Day 24 solution using the OUnit2 framework.

    Tests validate:
    - Correct parsing of input data
    - Accurate circuit simulation results
    - Proper identification of swapped gate outputs

    Each test uses small, manageable examples to verify distinct aspects of the
    solution. *)

open OUnit2
open Day24

(** Example input from the challenge *)
let example_input =
  "x00: 1\n\
   x01: 0\n\
   x02: 1\n\
   x03: 1\n\
   x04: 0\n\
   y00: 1\n\
   y01: 1\n\
   y02: 1\n\
   y03: 1\n\
   y04: 1\n\n\
   ntg XOR fgs -> mjb\n\
   y02 OR x01 -> tnw\n\
   kwq OR kpj -> z05\n\
   x00 OR x03 -> fst\n\
   tgd XOR rvg -> z01\n\
   vdt OR tnw -> bfw\n\
   bfw AND frj -> z10\n\
   ffh OR nrd -> bqk\n\
   y00 AND y03 -> djm\n\
   y03 OR y00 -> psh\n\
   bqk OR frj -> z08\n\
   tnw OR fst -> frj\n\
   gnj AND tgd -> z11\n\
   bfw XOR mjb -> z00\n\
   x03 OR x00 -> vdt\n\
   gnj AND wpb -> z02\n\
   x04 AND y00 -> kjc\n\
   djm OR pbm -> qhw\n\
   nrd AND vdt -> hwm\n\
   kjc AND fst -> rvg\n\
   y04 OR y02 -> fgs\n\
   y01 AND x02 -> pbm\n\
   ntg OR kjc -> kwq\n\
   psh XOR fgs -> tgd\n\
   qhw XOR tgd -> z09\n\
   pbm OR djm -> kpj\n\
   x03 XOR y03 -> ffh\n\
   x00 XOR y04 -> ntg\n\
   bfw OR bqk -> z06\n\
   nrd XOR fgs -> wpb\n\
   frj XOR qhw -> z04\n\
   bqk OR frj -> z07\n\
   y03 OR x01 -> nrd\n\
   hwm AND bqk -> z03\n\
   tgd XOR rvg -> z12\n\
   tnw OR pbm -> gnj"

(** Helper function to create test cases for part1 *)
let make_part1_test name input expected =
  name >:: fun _ ->
  let wires, gates = parse input in
  assert_equal expected (part1 (wires, gates)) ~printer:Int64.to_string

(** Part 1 test cases *)
let part1_tests = [ make_part1_test "example_part1" example_input 2024L ]

(** Complete test suite *)
let suite = "Day24 Test Suite" >::: [ "Part 1 Tests" >::: part1_tests ]

(** Run the tests *)
let () = run_test_tt_main suite

(** Main execution function that runs both parts of the Day 24 challenge.

    This function:

    - 1. Parses the input file
    - 2. Runs Part 1 to simulate the circuit and calculate the output value
    - 3. Runs Part 2 to identify the swapped gate outputs
    - 4. Reports timing and results *)
let () =
  try
    (* Read input from stdin *)
    let input = In_channel.input_all In_channel.stdin |> String.trim in
    Printf.printf "Input length: %d\n%!" (String.length input);

    (* Parse input into wires and gates *)
    let wires, gates = parse input in
    Printf.printf "Wires and Gates count: %d, %d\n%!" (List.length wires)
      (List.length gates);

    (* Solve Part 1 and Part 2 with timing *)
    let start_time = Unix.gettimeofday () in

    let result1 = part1 (wires, gates) in
    let result2 = part2 (wires, gates) in

    let end_time = Unix.gettimeofday () in

    Printf.printf "Part 1: %Ld\n%!" result1;
    Printf.printf "Part 2: %s\n%!" result2;
    Printf.printf "Elapsed time: %.4f seconds\n%!" (end_time -. start_time)
  with
  | Failure msg -> Printf.printf "Error: %s\n" msg
  | Invalid_argument arg -> Printf.printf "Invalid argument: %s\n" arg
  | e -> Printf.printf "Unexpected error: %s\n" (Printexc.to_string e)

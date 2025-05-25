open OUnit2
open Day07

(* Example input *)
let example_input = "190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20"

(* Helper function to create part1 tests *)
let make_part1_test name expected_output input =
  name >:: (fun _ ->
    let equations = parse input in
    assert_equal expected_output (part1 equations) ~printer:Int64.to_string)

(* Helper function to create part2 tests *)
let make_part2_test name expected_output input =
  name >:: (fun _ ->
    let equations = parse input in
    assert_equal expected_output (part2 equations) ~printer:Int64.to_string)

(* Part1 and Part2 test cases *)
let part1_tests = [
  make_part1_test "example_part1" 3749L example_input;
]

let part2_tests = [
  make_part2_test "example_part2" 11387L example_input;
]

(* Main test suite *)
let suite = "Day07 Test Suite" >::: [
  "part1 tests" >::: part1_tests;
  "part2 tests" >::: part2_tests;
]

let () = run_test_tt_main suite


let () =
  let input = 
    In_channel.input_all In_channel.stdin 
    |> String.trim
  in
  let equations = parse input in

  let start_time = Unix.gettimeofday () in

  equations |> part1 |> Printf.printf "Part 1: %Ld\n";

  let part2_result = part2 equations in
  Printf.printf "Part 2: %Ld\n" part2_result;

  let end_time = Unix.gettimeofday () in
  let elapsed = end_time -. start_time in
  Printf.printf "Elapsed time: %.8f seconds\n" elapsed
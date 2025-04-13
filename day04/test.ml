open OUnit2
open Day04

(* Example input for tests *)
let example_input =
  "MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX"

(* Helper function to create part1 tests *)
let make_part1_test name expected_output input =
  name >:: (fun _ ->
    let parsed = parse input in
    let result = part1 parsed in
    assert_equal expected_output result ~printer:string_of_int)

(* Helper function to create part2 tests *)
let make_part2_test name expected_output input =
  name >:: (fun _ ->
    let parsed = parse input in
    let result = part2 parsed in
    assert_equal expected_output result ~printer:string_of_int)

(* Part1 and Part2 test cases *)
let part1_tests = [
  make_part1_test "example_part1" 18 example_input;
]

let part2_tests = [
  make_part2_test "example_part2" 9 example_input;
]

(* Main test suite *)
let suite = "Day04 Test Suite" >::: [
  "part1 tests" >::: part1_tests;
  "part2 tests" >::: part2_tests;
]

(* Run the tests *)
let () = run_test_tt_main suite


let () =
  let input = In_channel.input_all In_channel.stdin |> String.trim in
  let start_time = Unix.gettimeofday () in


  let chars = parse input in

  chars |> part1 |> Printf.printf "Part 1: %d\n";
  chars |> part2 |> Printf.printf "Part 2: %d\n";

  let end_time = Unix.gettimeofday () in
  Printf.printf "Elapsed time: %.4f seconds\n%!" (end_time -. start_time);


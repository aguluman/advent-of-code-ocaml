open OUnit2
open Day06

(* Example input *)
let example_input = "....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#..."

(* Helper function to create part1 tests *)
let make_part1_test name expected_output input =
  name >:: (fun _ ->
    let map = parse input in
    assert_equal expected_output (part1 map) ~printer:string_of_int)

(* Helper function to create part2 tests *)
let make_part2_test name expected_output input =
  name >:: (fun _ ->
    let map = parse input in
    assert_equal expected_output (part2 map) ~printer:string_of_int)

(* Part1 and Part2 test cases *)
let part1_tests = [
  make_part1_test "example_part1" 41 example_input;
]

let part2_tests = [
  make_part2_test "example_part2" 6 example_input;
]

(* Main test suite *)
let suite = "Day06 Test Suite" >::: [
  "part1 tests" >::: part1_tests;
  "part2 tests" >::: part2_tests;
]

let () = run_test_tt_main suite

(** Main entry point *)
let () =
  let input = In_channel.input_all In_channel.stdin |> parse in

  let start_time = Unix.gettimeofday () in

  let part1_result = part1 input in
  Printf.printf "Part 1: %d\n" part1_result;

  let part2_result = part2 input in
  Printf.printf "Part 2: %d\n" part2_result;

  let end_time = Unix.gettimeofday () in
  Printf.printf "Elapsed time: %f seconds\n" (end_time -. start_time)
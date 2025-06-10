open OUnit2
open Day01

(* Example input *)
let example_input = "3   4\n 4   3\n 2   5\n 1   3\n 3   9\n 3   3"

(* Helper function to create part1 tests *)
let make_part1_test name expected_output input =
  name >:: fun _ ->
  let location_ids = parse input in
  assert_equal expected_output (part1 location_ids) ~printer:string_of_int

(* Helper function to create part2 tests *)
let make_part2_test name expected_output input =
  name >:: fun _ ->
  let location_ids = parse input in
  assert_equal expected_output (part2 location_ids) ~printer:Int64.to_string

(* Part1 and Part2 test cases *)
let part1_tests = [ make_part1_test "example_part1" 11 example_input ]

let part2_tests =
  [ make_part2_test "example_part2" (Int64.of_int 31) example_input ]

(* Main test suite *)
let suite =
  "Day01 Test Suite"
  >::: [ "part1 tests" >::: part1_tests; "part2 tests" >::: part2_tests ]

let () = run_test_tt_main suite

let () =
  (* Read input from stdin *)
  let input = In_channel.input_all In_channel.stdin |> String.trim in

  let location_ids = parse input in

  let start_time = Unix.gettimeofday () in

  location_ids |> part1 |> Printf.printf "Part 1: %d\n";
  location_ids |> part2 |> Printf.printf "Part 2: %Ld\n";

  let end_time = Unix.gettimeofday () in
  Printf.printf "Elapsed time: %.4f seconds\n%!" (end_time -. start_time)

open OUnit2
open Day02

(* Example input *)
let example_input =
  "7 6 4 2 1\n1 2 7 8 9\n9 7 6 2 1\n1 3 2 4 5\n8 6 4 4 1\n1 3 6 7 9"

(* Helper function to create part1 tests *)
let make_part1_test name expected_output input =
  name >:: fun _ ->
  let reports = parse input in
  assert_equal expected_output (part1 reports) ~printer:string_of_int

(* Helper function to create part2 tests *)
let make_part2_test name expected_output input =
  name >:: fun _ ->
  let reports = parse input in
  assert_equal expected_output (part2 reports) ~printer:string_of_int

(* Part1 and Part2 test cases *)
let part1_tests = [ make_part1_test "example_part1" 2 example_input ]
let part2_tests = [ make_part2_test "example_part2" 4 example_input ]

(* Main test suite *)
let suite =
  "Day02 Test Suite"
  >::: [ "part1 tests" >::: part1_tests; "part2 tests" >::: part2_tests ]

let () = run_test_tt_main suite

let () =
  let has_input =
    try
      let _ = Unix.select [ Unix.stdin ] [] [] 0.0 in
      true
    with Unix.Unix_error _ -> false
  in

  if has_input then
    let input = In_channel.input_all In_channel.stdin |> String.trim in
    if String.length input > 0 then (
      let start_time = Unix.gettimeofday () in

      let reports = parse input in
      Printf.printf "Part 1: %d\n" (part1 reports);
      Printf.printf "Part 2: %d\n" (part2 reports);

      let end_time = Unix.gettimeofday () in
      Printf.printf "Elapsed time: %.4f seconds\n%!" (end_time -. start_time))

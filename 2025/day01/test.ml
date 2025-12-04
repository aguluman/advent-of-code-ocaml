open OUnit2
open Day01

(** Example input from the challenge *)
let example_input = "L68\nL30\nR48\nL5\nR60\nL55\nL1\nL99\nR14\nL82"

(** Helper function to create test cases for part1 *)
let make_part1_test name expected_output input =
  name >:: fun _ ->
  assert_equal expected_output (part1 input) ~printer:string_of_int

(** Helper function to create test cases for part2 *)
let make_part2_test name expected_output input =
  name >:: fun _ ->
  assert_equal expected_output (part2 input) ~printer:string_of_int

(** Part 1 test cases *)
let part1_tests = [ make_part1_test "example_part1" 3 example_input ]

(** Part 2 test cases *)
let part2_tests = [ make_part2_test "example_part2" 6 example_input ]

(** Complete test suite *)
let suite =
  "Day01 Test Suite"
  >::: [ "Part 1 Tests" >::: part1_tests; "Part 2 Tests" >::: part2_tests ]

(** Run the tests *)
let () = run_test_tt_main suite

(** Main entry point - reads input, runs both function parts and prints results
    with timing *)
let () =
  try
    let has_input =
      try
        let _ = Unix.select [ Unix.stdin ] [] [] 0.0 in
        true
      with Unix.Unix_error _ -> false
    in

    if has_input then
      let input = In_channel.input_all In_channel.stdin |> String.trim in
      if String.length input > 0 then (
        let timer_start = Unix.gettimeofday () in

        input |> part1 |> Printf.printf "Part 1: %d\n%!";
        input |> part2 |> Printf.printf "Part 2: %d\n%!";

        Unix.gettimeofday () -. timer_start
        |> Printf.printf "Elapsed time: %.4f seconds\n%!")
  with
  | Failure msg -> Printf.printf "Error: %s\n%!" msg
  | e -> Printf.printf "Unexpected error: %s\n%!" (Printexc.to_string e)

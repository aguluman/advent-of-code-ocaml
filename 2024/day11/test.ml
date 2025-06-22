open OUnit2
open Day11

let example_input = "125 17"

let make_part1_test name expected_output input =
  name >:: fun _ ->
  assert_equal expected_output (part1 input) ~printer:string_of_int

let make_part2_test name expected_output input =
  name >:: fun _ ->
  assert_equal expected_output (part2 input) ~printer:Int64.to_string

let part1_tests = [ make_part1_test "part1 test" 55312 example_input ]
let part2_tests = [ make_part2_test "part2 test" 65601038650482L example_input ]

let suite =
  "Day11 Test Suite"
  >::: [ "Part 1 Tests" >::: part1_tests; "Part 2 Tests" >::: part2_tests ]

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
        input |> part2 |> Printf.printf "Part 2: %Ld\n%!";

        Unix.gettimeofday () -. timer_start
        |> Printf.printf "Elapsed time: %.4f seconds\n%!")
  with
  | Failure msg -> Printf.printf "Error: %s\n%!" msg
  | e -> Printf.printf "Unexpected error: %s\n%!" (Printexc.to_string e)

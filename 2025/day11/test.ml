open OUnit2
open Day11

(** Example input for Part 1 *)
let example_input_part1 =
  "aaa: you hhh\n\
   you: bbb ccc\n\
   bbb: ddd eee\n\
   ccc: ddd eee fff\n\
   ddd: ggg\n\
   eee: out\n\
   fff: out\n\
   ggg: out\n\
   hhh: ccc fff iii\n\
   iii: out\n"

(** Example input for Part 2 *)
let example_input_part2 =
  "svr: aaa bbb\n\
   aaa: fft\n\
   fft: ccc\n\
   bbb: tty\n\
   tty: ccc\n\
   ccc: ddd eee\n\
   ddd: hub\n\
   hub: fff\n\
   eee: dac\n\
   dac: fff\n\
   fff: ggg hhh\n\
   ggg: out\n\
   hhh: out\n"

(** Helper function to create test cases for part1 *)
let make_part1_test name expected_output input =
  name >:: fun _ ->
  assert_equal expected_output (part1 input) ~printer:string_of_int

(** Helper function to create test cases for part2 *)
let make_part2_test name expected_output input =
  name >:: fun _ ->
  assert_equal expected_output (part2 input) ~printer:Int64.to_string

(** Part 1 test cases *)
let part1_tests = [ make_part1_test "example_part1" 5 example_input_part1 ]

(** Part 2 test cases *)
let part2_tests = [ make_part2_test "example_part2" 2L example_input_part2 ]

(** Complete test suite *)
let suite =
  "Day11 Test Suite"
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
        input |> part2 |> Printf.printf "Part 2: %Ld\n%!";

        Unix.gettimeofday () -. timer_start
        |> Printf.printf "Elapsed time: %.4f seconds\n%!")
  with
  | Failure msg -> Printf.printf "Error: %s\n%!" msg
  | e -> Printf.printf "Unexpected error: %s\n%!" (Printexc.to_string e)

(** Test suite for Day 19: Linen Layout Tests the solution for both parts of the
    challenge using examples from the problem description. *)
open OUnit2

open Day19

(** Example input from the challenge description *)
let example_input =
  "r, wr, b, g, bwu, rb, gb, br\n\n\
   brwrr\n\
   bggr\n\
   gbbr\n\
   rrbgbr\n\
   ubwu\n\
   bwurrg\n\
   brgr\n\
   bbrgwb"

(** Helper function to create part1 tests *)
let make_part1_test name expected_output input =
  name >:: fun _ ->
  let patterns, designs = parse input in
  assert_equal expected_output
    (part1 (patterns, designs))
    ~printer:string_of_int

(** Part1 test cases *)
let part1_tests = [ make_part1_test "example_part1" 6 example_input ]

(** Helper function to create part2 tests *)
let make_part2_test name expected_output input =
  name >:: fun _ ->
  let patterns, designs = parse input in
  assert_equal expected_output
    (part2 (patterns, designs))
    ~printer:Int64.to_string

(** Part2 test cases *)
let part2_tests = [ make_part2_test "example_part2" 16L example_input ]

(** Main test suite *)
let suite =
  "Day19 Test Suite"
  >::: [ "part1 tests" >::: part1_tests; "part2 tests" >::: part2_tests ]

(** Run the tests *)
let () = run_test_tt_main suite

(** Main entry point for solution *)
let () =
  try
    (* Read input from stdin *)
    let has_input =
      try
        let _ = Unix.select [ Unix.stdin ] [] [] 0.0 in
        true
      with Unix.Unix_error _ -> false
    in

    if has_input then
      let input = In_channel.input_all In_channel.stdin |> String.trim in
      if String.length input > 0 then (
        Printf.printf "Input length: %d\n%!" (String.length input);

        (* Parse input into patterns and designs *)
        let patterns, designs = parse input in
        Printf.printf "Parsed %d patterns and %d designs\n%!"
          (List.length patterns) (List.length designs);

        (* Time the execution *)
        let start_time = Unix.gettimeofday () in

        (* Solve and print Part 1 *)
        part1 (patterns, designs)
        |> string_of_int
        |> Printf.printf "Part 1: %s\n%!";

        (* Solve and print Part 2 *)
        part2 (patterns, designs)
        |> Int64.to_string
        |> Printf.printf "Part 2: %s\n%!";

        (* Print elapsed time *)
        Unix.gettimeofday () -. start_time
        |> Printf.printf "Elapsed time: %.4f seconds\n")
  with
  | Failure msg -> Printf.printf "Error: %s\n" msg
  | e -> Printf.printf "Unexpected error: %s\n" (Printexc.to_string e)

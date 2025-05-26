open OUnit2
open Day09

(** Example input from the challenge *)
let example_input = "2333133121414131402"

(** Helper function to create test cases for part1 *)
let make_part1_test name expected_output input = 
  name >:: (fun _ -> 
    assert_equal expected_output (part1 input) ~printer:Int64.to_string)

(** Helper function to create test cases for part2 *)
let make_part2_test name expected_output input = 
  name >:: (fun _ -> 
    assert_equal expected_output (part2 input) ~printer:Int64.to_string)

(** Part 1 test cases *)
let part1_tests = [
  make_part1_test "example_part1" 1928L example_input;
]

(** Part 2 test cases *)
let part2_tests = [
  make_part2_test "example_part2" 2858L example_input;
]

(** Complete test suite *)
let suite = "Day09 Test Suite" >::: [
  "part1 tests" >::: part1_tests;
  "part2 tests" >::: part2_tests;
]

(** Run the tests *)
let () = run_test_tt_main suite

(** Main entry point - reads input, runs both function parts and prints results with timing *)
let () =
  try
    stdin
    |> input_line
    |> fun input ->
        let timer_start = Unix.gettimeofday () in
        
        input |> part1 |> Printf.printf "Part 1: %Ld\n";
        input |> part2 |> Printf.printf "Part 2: %Ld\n";
      
        Unix.gettimeofday () -. timer_start

    |> Printf.printf "Elapsed time: %.4f seconds\n"


  with
  | Failure msg -> Printf.printf "Error: %s\n" msg
  | e -> Printf.printf "Unexpected error: %s\n" (Printexc.to_string e)
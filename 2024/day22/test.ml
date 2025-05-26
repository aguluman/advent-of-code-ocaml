(** Test suite for Day 22: Monkey Market
    Tests the secret number trading challenge using example cases.
*)
open OUnit2
open Day22

(** Example inputs from the challenge *)
let example_input1 = "1
10
100
2024"

let example_input2 = "1
2
3
2024"



(** Helper function to create test cases for part1 *)
let make_part1_test name input expected =
  name >:: (fun _ ->
    let initial_secrets = parse input in
    assert_equal expected (part1 initial_secrets)
      ~printer:Int64.to_string)

    

(** Helper function to create test cases for part2 *)
let make_part2_test name input expected =
  name >:: (fun _ ->
    let initial_secrets = parse input in
    assert_equal expected (part2 initial_secrets)
      ~printer:Int64.to_string)



(** Tests the solution for part 1 of the Monkey Market challenge.

    Verifies that our algorithm correctly calculates the sum of the 2000th 
    secret number for each buyer in the example data.

    The expected result is {b 37327623}, representing the combined secrets from
    the buyers with initial secrets of:
    
    - 1
    - 10
    - 100  
    - 2024
*)let part1_tests = [
  make_part1_test "example_part1" example_input1 37327623L;
]



(** Tests the solution for part 2 of the Monkey Market challenge.

    Validates that our algorithm correctly identifies the optimal
    sequence of four price changes that maximizes the total number
    of bananas received from all buyers.

    The expected result is 23, which represents the sum of bananas
    obtained by following the pattern {b -2,1,-1,3} when selling
    hiding spots to buyers with initial secrets of:
    
    - 1
    - 2
    - 3
    - 2024
*)
let part2_tests = [
  make_part2_test "example_part2" example_input2 23L;
]

(** Complete test suite *)
let suite = "Day22 Test Suite" >::: [
  "Part 1 Tests" >::: part1_tests;
  "Part 2 Tests" >::: part2_tests;
]

(** Run the tests *)
let () = run_test_tt_main suite



(** Main entry point for solution *)
let () =
  try
    (* Read input from stdin *)
    let input = In_channel.input_all In_channel.stdin |> String.trim in
    Printf.printf "Input length: %d\n%!" (String.length input);
    
    (* Parse input into initial secrets *)
    let initial_secrets = parse input in
    Printf.printf "Number of initial secrets: %d\n%!" (Array.length initial_secrets);
    
    (* Solve Part 1 with timing *)
    let start_time = Unix.gettimeofday () in
    
    initial_secrets 
    |> part1 
    |> Printf.printf "\nPart 1: %Ld\n%!";
    
    
    (* Solve Part 2 with timing *)
    initial_secrets
    |> part2
    |> Printf.printf "Part 2: %Ld\n%!";
    
    Unix.gettimeofday () -. start_time
    |> Printf.printf "Elapsed time: %.4f seconds\n"

  with
  | Failure msg -> Printf.printf "Error: %s\n" msg
  | e -> Printf.printf "Unexpected error: %s\n" (Printexc.to_string e)
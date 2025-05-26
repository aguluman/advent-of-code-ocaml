open OUnit2
open Day21  

let example_input =
        "029A
980A
179A
456A
379A"


(** Helper function to create test cases for part1 *)
let make_part1_test name input expected =
  name >:: (fun _ ->
    let codes = parse input in
    let actual_result = part1 codes in
    assert_equal expected actual_result ~printer:Int64.to_string)

(** Part 1 test cases *)
let part1_tests = [
  make_part1_test "example_part1" example_input 126384L;
]

let suite =
  "Day21 Test Suite" >::: [
    "Part 1 Tests" >::: part1_tests;
  ]

(* Run the tests *)
let () = run_test_tt_main suite


(* Main entry point for solution *)
let () =
  try
    let input = In_channel.input_all In_channel.stdin |> String.trim in
    let codes = parse input in
    
    let start_time = Unix.gettimeofday () in
    
    codes |> part1 |> Printf.printf "\nPart 1: %Ld\n";
    codes |> part2 |> Printf.printf "Part 2: %Ld\n";
    
    Unix.gettimeofday () -. start_time
    |> Printf.printf "Elapsed time: %.4f seconds\n"
    
  with
  | Failure msg -> Printf.printf "Error: %s\n" msg
  | e -> Printf.printf "Unexpected error: %s\n" (Printexc.to_string e)  
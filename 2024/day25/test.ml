(** Test suite for Day 25: Schematic Pattern Matching
    Tests the solution using example schematic inputs.
*)
open OUnit2
open Day25

(** Example input from the challenge *)
let example_input = "#####
.####
.####
.####
.#.#.
.#...
.....

#####
##.##
.#.##
...##
...#.
...#.
.....

.....
#....
#....
#...#
#.#.#
#.###
#####

.....
.....
#.#..
###..
###.#
###.#
#####

.....
.....
.....
#....
#.#..
#.#.#
#####"


(** Helper function to create test cases for part1 *)
let make_part1_test name input expected =
  name >:: (fun _ ->
    let schematics = parse input in
    (* Convert char array list to char array array for each schematic *)
    let converted_schematics = 
      List.map (fun schematic -> Array.of_list schematic) schematics in
    assert_equal expected (part1 converted_schematics) ~printer:string_of_int)


(** Part 1 test cases *)
let part1_tests = [
  make_part1_test "example_part1" example_input 3;
]

(** Complete test suite *)
let suite = "Day25 Test Suite" >::: [
  "Part 1 Tests" >::: part1_tests;
]

(** Run the tests *)
let () = run_test_tt_main suite

(** Main entry point for solution *)
let () =
  try
    (* Read input from stdin *)
    let input = In_channel.input_all In_channel.stdin |> String.trim in
    Printf.printf "Input length: %d\n%!" (String.length input);
    
    (* Parse input into schematics *)
    let schematics = parse input in
    Printf.printf "Number of schematics: %d\n%!" (List.length schematics);
    
    (* Solve Part 1 and Part 2 with timing *)
    let start_time = Unix.gettimeofday () in
    
    let converted_schematics = 
      List.map (fun schematic -> Array.of_list schematic) schematics in
    let result1 = part1 converted_schematics in
    
    let end_time = Unix.gettimeofday () in
    
    Printf.printf "Part 1: %d\n%!" result1;
    Printf.printf "Elapsed time: %.4f seconds\n%!" (end_time -. start_time);
    
  with
  | Failure msg -> Printf.printf "Error: %s\n" msg
  | Invalid_argument arg -> Printf.printf "Invalid argument: %s\n" arg
  | e -> Printf.printf "Unexpected error: %s\n" (Printexc.to_string e)
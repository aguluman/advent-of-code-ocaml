(** Test suite for Day 20: Maze Optimization
    Tests the solution for both parts of the challenge using examples.
*)
open OUnit2
open Day20

(** Example maze from the challenge description *)
let example_input = "###############
#...#...#.....#
#.#.#.#.#.###.#
#S#...#.#.#...#
#######.#.#.###
#######.#.#...#
#######.#.###.#
###..E#...#...#
###.#######.###
#...###...#...#
#.#####.#.###.#
#.#...#.#.#...#
#.#.#.#.#.#.###
#...#...#...###
###############"

(** Helper function to check if results contain expected values *)
let contains_all expected actual =
  let found = ref true in
  List.iter (fun (exp_val, exp_count) ->
    let actual_count = try 
      List.assoc exp_val actual 
    with Not_found -> 0 in
    if actual_count < exp_count then
      found := false
  ) expected;
  !found

(** Helper to create an assertion that checks if results contain all expected values *)
let assert_superset_of expected actual =
  if not (contains_all expected actual) then
    assert_failure (Printf.sprintf "Expected results to contain all of %s but got %s" 
      (String.concat ", " (List.map (fun (value, count) -> Printf.sprintf "(%d,%d)" value count) expected))
      (String.concat ", " (List.map (fun (value, count) -> Printf.sprintf "(%d,%d)" value count) actual)))

(** Helper function to create part1 tests *)
let make_part1_test name expected_values input =
  name >:: (fun _ ->
    let maze = parse input in
    let results = part1 maze in
    assert_superset_of expected_values results)

(** Helper function to create part2 tests *)
let make_part2_test name expected_values input =
  name >:: (fun _ ->
    let maze = parse input in
    let results = part2 maze in
    assert_superset_of expected_values results)

(** Part1 test cases *)
let part1_tests = [
  make_part1_test "example_part1" 
    [ (2, 14); (4, 14); (6, 2); (8, 4); (10, 2); (12, 3);
      (20, 1); (36, 1); (38, 1); (40, 1); (64, 1) ]
    example_input;
]    

(** Part2 test cases *)
let part2_tests = [
  make_part2_test "example_part2" 
    [ (50, 32); (52, 31); (54, 29); (56, 39); (58, 25);
      (60, 23); (62, 20); (64, 19); (66, 12); (68, 14);
      (70, 12); (72, 22); (74, 4); (76, 3) ]
    example_input;
]

(** Main test suite *)
let suite = "Day20 Test Suite" >::: [
  "part1 tests" >::: part1_tests;
  "part2 tests" >::: part2_tests;
]

(** Run the tests *)
let () = run_test_tt_main suite


(** Main entry point for solution *)
let () =
  try
    (* Read input from stdin *)
    let input = In_channel.input_all In_channel.stdin |> String.trim in
    Printf.printf "Input length: %d\n%!" (String.length input);
    
    (* Parse input into maze *)
    let maze = parse input in
    Printf.printf "Parsed maze dimensions: %dx%d\n%!" 
      (Array.length maze) 
      (if Array.length maze > 0 then Array.length maze.(0) else 0);
    
    (* Solve Part 1 with timing *)
    let start_time = Unix.gettimeofday () in
    
    maze
    |> part1
    |> List.fold_left (fun acc (improvement, count) -> 
        if improvement >= 100 then acc + count else acc) 0
    |> Printf.printf "\nPart 1: %d\n%!";
    
    let mid_time = Unix.gettimeofday () in
    Printf.printf "Elapsed time: %.4f seconds\n%!" (mid_time -. start_time);
    
    (* Solve Part 2 with timing *)
    maze
    |> part2
    |> List.fold_left (fun acc (improvement, count) -> 
        if improvement >= 100 then acc + count else acc) 0
    |> Printf.printf "Part 2: %d\n%!";
    
    let end_time = Unix.gettimeofday () in
    Printf.printf "Elapsed time: %.4f seconds\n%!" (end_time -. mid_time);
    
  with
  | Failure msg -> Printf.printf "Error: %s\n" msg
  | e -> Printf.printf "Unexpected error: %s\n" (Printexc.to_string e)

open OUnit2
open Day18

(** Example input from the challenge *)
let example_input = "5,4
4,2
4,5
3,0
2,1
6,3
2,4
1,5
0,6
3,3
2,6
5,1
1,2
5,5
2,5
6,5
1,4
0,4
6,4
1,1
6,1
1,0
0,5
1,6
2,0"



(** Test for shortest path finding function *)
let shortest_path_tests = [
  "find_shortest_path with simple scenario" >:: (fun _ ->
    let positions = [(1, 1); (2, 2)] in
    let result = find_shortest_path 3 positions in
    assert_equal (Some 6) result ~printer:(function 
      | None -> "None" 
      | Some v -> string_of_int v))
]


(** Helper function to create test cases for calculate_min_steps (Part 1) *)
let make_min_steps_test name grid_size bytes expected =
  name >:: (fun _ ->
    let positions = parse_byte_positions example_input 
  in
    assert_equal expected (calculate_min_steps (grid_size, bytes, positions))
      ~printer:string_of_int)


(** Part 1 test cases *)
let part1_tests = [
  make_min_steps_test "example_part1" 6 12 22;
]



(** Helper function to create test cases for find_blocking_byte (Part 2) *)
let make_blocking_byte_test name grid_size expected_x expected_y =
  name >:: (fun _ ->
    let positions = parse_byte_positions example_input 
  in
    let (x, y) = find_blocking_byte (grid_size, positions) 
  in
    assert_equal expected_x x ~printer:string_of_int ~msg:"X coordinate";
    assert_equal expected_y y ~printer:string_of_int ~msg:"Y coordinate")

    
(** Part 2 test cases *)
let part2_tests = [
  make_blocking_byte_test "example_part2" 6 6 1;
]



(** Grid visualization helper *)
let grid_visualization =
  "grid_visualization" >:: (fun _ ->
    let positions = [(1, 1); (2, 2); (0, 3)] in
    let grid = create_memory_grid 4 positions in
    Printf.printf "\nMemory grid representation (# = corrupted, . = safe):\n";
    Array.iter (fun row ->
      Array.iter (fun cell -> Printf.printf "%c" (if cell then '#' else '.')) row;
      Printf.printf "\n"
    ) grid)



(** Run algorithm on sample data with step-by-step logging *)
let algorithm_tracing = 
  "algorithm_tracing" >:: (fun _ ->
    Printf.printf "\nTracing first few bytes falling:\n";
    let positions = parse_byte_positions example_input |> Seq.take 5 |> List.of_seq 
  in
    Printf.printf "Considering positions: ";
    List.iter (fun (x, y) -> Printf.printf "(%d,%d) " x y) positions;
    Printf.printf "\n";
    
    let result = find_shortest_path 6 positions in
    match result with
    | Some dist -> Printf.printf "Shortest path length: %d\n" dist
    | None -> Printf.printf "No path exists\n")



(** Complete test suite *)
let suite = "Day18 Test Suite" >::: [
  "Shortest Path Tests" >::: shortest_path_tests;
  "Part 1 Tests" >::: part1_tests;
  "Part 2 Tests" >::: part2_tests;
  grid_visualization;
  algorithm_tracing;
]

(** Run the tests *)
let () = run_test_tt_main suite


(** Main execution function that solves both parts of the challenge. *)
let () =
  try
    let input = In_channel.input_all In_channel.stdin |> String.trim in
    let positions = parse_byte_positions input in
    
    let start_time = Unix.gettimeofday () in
    
    let part1_answer = calculate_min_steps (70, 1024, positions) in
    Printf.printf "\nPart 1: %d\n" part1_answer;
    
    let (blocking_x, blocking_y) = find_blocking_byte (70, positions) in
    Printf.printf "Part 2: (%d,%d)\n" blocking_x blocking_y;
    
    Unix.gettimeofday () -. start_time
    |> Printf.printf "Elapsed time: %.8f seconds\n"
    
  with
  | Failure msg -> Printf.printf "Error: %s\n" msg
  | e -> Printf.printf "Unexpected error: %s\n" (Printexc.to_string e)
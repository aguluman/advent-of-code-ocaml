open OUnit2
open Day16

(** Example maze inputs from the challenge *)
let example_maze_1 = "###############
#.......#....E#
#.#.###.#.###.#
#.....#.#...#.#
#.###.#####.#.#
#.#.#.......#.#
#.#.#####.###.#
#...........#.#
###.#.#####.#.#
#...#.....#.#.#
#.#.#.###.#.#.#
#.....#...#.#.#
#.###.#.#.#.#.#
#S..#.....#...#
###############"

let example_maze_2 = "#################
#...#...#...#..E#
#.#.#.#.#.#.#.#.#
#.#.#.#...#...#.#
#.#.#.#.###.#.#.#
#...#.#.#.....#.#
#.#.#.#.#.#####.#
#.#...#.#.#.....#
#.#.#####.#.###.#
#.#.#.......#...#
#.#.###.#####.###
#.#.#...#.....#.#
#.#.#.#####.###.#
#.#.#.........#.#
#.#.#.#########.#
#S#.............#
#################"

(** Helper function to create test cases for part1 *)
let make_part1_test name input expected =
  name >:: (fun _ ->
    let maze = parse_input input in
    assert_equal expected (solve_part1 maze)
      ~printer:string_of_int)

(** Helper function to create test cases for part2 *)
let make_part2_test name input expected =
  name >:: (fun _ ->
    let maze = parse_input input in
    assert_equal expected (solve_part2 maze)
      ~printer:string_of_int)

(** Part 1 test cases *)
let part1_tests = [
  make_part1_test "example_1_part1" example_maze_1 7036;
  make_part1_test "example_2_part1" example_maze_2 11048;
]

(** Part 2 test cases *)
let part2_tests = [
  make_part2_test "example_1_part2" example_maze_1 45;
  make_part2_test "example_2_part2" example_maze_2 64;
]

(** Visual verification helper *)
let print_maze maze =
  "maze_visualization" >:: (fun _ ->
    Printf.printf "\nMaze representation:\n";
    Array.iter (fun row ->
      Array.iter (fun cell -> Printf.printf "%c" cell) row;
      Printf.printf "\n"
    ) maze)

(** Complete test suite *)
let suite = "Day16 Test Suite" >::: [
  "Part 1 Tests" >::: part1_tests;
  "Part 2 Tests" >::: part2_tests;
  print_maze (parse_input example_maze_1);
]

(** Run the tests *)
let () = run_test_tt_main suite

(** Main entry point for solution *)
let () =
  try
    let input = In_channel.input_all In_channel.stdin |> String.trim in
    let maze = parse_input input in
    
    let start_time = Unix.gettimeofday () in
    
    maze |> solve_part1 |> Printf.printf "Part 1: %d\n";
    maze |> solve_part2 |> Printf.printf "Part 2: %d\n";
    
    Unix.gettimeofday () -. start_time
    |> Printf.printf "Elapsed time: %.4f seconds\n"
    
  with
  | Failure msg -> Printf.printf "Error: %s\n" msg
  | e -> Printf.printf "Unexpected error: %s\n" (Printexc.to_string e)
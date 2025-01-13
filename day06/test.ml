open OUnit2
open Day06

let example_input = "....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#..."

(* Test specific movement scenarios *)
let movement_tests = [
  "initial_up_movement" >:: (fun _ ->
    let map = parse "...\n.^.\n..." in
    assert_equal (1,1) (find_start map 3));

  "turn_right_at_obstacle" >:: (fun _ ->
    let map = parse "..#\n.^.\n..." in
    let pos = (1,1) in
    assert_equal true (is_blocked map 3 pos U));
]

(* Main test cases *)
let part1_tests = "test suite for part1" >::: [
  "example_part1" >:: (fun _ ->
    let map = parse example_input in
    assert_equal 41 (part1 map) ~printer:string_of_int);
]

let suite = "all tests" >::: [
  "movement" >::: movement_tests;
  part1_tests;
]

let () = run_test_tt_main suite
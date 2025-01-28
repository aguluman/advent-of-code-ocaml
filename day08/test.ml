open OUnit2
open Day08

let example_input = "............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............"

let make_part1_test name expected_output input = 
  name >:: (fun _ -> 
    let equations = parse input in
    assert_equal expected_output (part1 equations) ~printer:string_of_int)

let make_part2_test name expected_output input = 
  name >:: (fun _ -> 
    let equations = parse input in 
    assert_equal expected_output (part2 equations) ~printer:string_of_int)

let part1_tests = [
  make_part1_test "example_part1" 14 example_input;
]

let part2_tests = [
  make_part2_test "example_part2" 34 example_input;
]

let suite = "Day08 Test Suite" >::: [
  "part1 tests" >::: part1_tests;
  "part2 tests" >::: part2_tests;
]

let () = run_test_tt_main suite
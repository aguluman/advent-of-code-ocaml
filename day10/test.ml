open OUnit2
open Day10

let example_input = "89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732"

let make_part1_test name expected_output input = 
  name >:: (fun _ -> 
    let map = parse input in 
    assert_equal expected_output (part1 map) ~printer:string_of_int)

let make_part2_test name expected_output input = 
  name >:: (fun _ -> 
    let map = parse input in 
    assert_equal expected_output (part2 map) ~printer:string_of_int)

let part1_test = [
  make_part1_test "part1 test" 36 example_input
] 

let part2_test = [
  make_part2_test "part2 test" 81 example_input
]

let suite = "Day10 Test Suite" >::: [
  "part1 tests" >::: part1_test;
  "part2 tests" >::: part2_test;
]

let () = run_test_tt_main suite
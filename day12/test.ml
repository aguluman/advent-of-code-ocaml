open OUnit2
open Day12

let small_input = "AAAA
BBCD
BBCC
EEEC"

let ox_input = "OOOOO
OXOXO
OOOOO
OXOXO
OOOOO"

let ex_input = "EEEEE
EXXXX
EEEEE
EXXXX
EEEEE"

let ab_input = "AAAAAA
AAABBA
AAABBA
ABBAAA
ABBAAA
AAAAAA"

let large_input = "RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE"

let make_part1_test name expected_output input = 
  name >:: (fun _ -> 
    let garden_map = parse input in
    assert_equal expected_output (part1 garden_map) ~printer:string_of_int)

let make_part2_test name expected_output input = 
  name >:: (fun _ -> 
    let garden_map = parse input in
    assert_equal expected_output (part2 garden_map) ~printer:string_of_int)

let part1_tests = [
  make_part1_test "small example part1" 140 small_input;
  make_part1_test "ox example part1" 772 ox_input;
  make_part1_test "large example part1" 1930 large_input;
]

let part2_tests = [
  make_part2_test "small example part2" 80 small_input;
  make_part2_test "ox example part2" 436 ox_input;
  make_part2_test "ex example part2" 236 ex_input;
  make_part2_test "ab example part2" 368 ab_input;
  make_part2_test "large example part2" 1206 large_input;
]

let suite = "Day12 Test Suite" >::: [
  "part1 tests" >::: part1_tests;
  "part2 tests" >::: part2_tests;
]

let () = run_test_tt_main suite
open OUnit2
open Day05

let example_input =  "47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47"
 

let make_part1_test name expected_output input =
  name >:: (fun _ -> 
    let rules, updates = parse input in
    assert_equal expected_output (part1 rules updates) ~printer:string_of_int)

let make_part2_test name expected_output input =
  name >:: (fun _ -> 
    let rules, updates = parse input in
    assert_equal expected_output (part2 rules updates) ~printer:string_of_int)

let part1_tests = "test suite for part1" >::: [
  make_part1_test "example part1" 143 example_input;
]

let part2_tests = "test suite for part2" >::: [
  make_part2_test "example part2" 123 example_input;
]

let suite = "all tests" >::: [
  part1_tests;
  part2_tests;
]

let () = run_test_tt_main suite

let () =
  let input = In_channel.input_all In_channel.stdin |> String.trim in
  let start_time = Unix.gettimeofday () in

  let (rules, updates) = parse input in

  part1 rules updates |> Printf.printf "Part 1: %d\n";
  part2 rules updates |> Printf.printf "Part 2: %d\n";

  let end_time = Unix.gettimeofday () in
  Printf.printf "Elapsed time: %.4f seconds\n%!" (end_time -. start_time)
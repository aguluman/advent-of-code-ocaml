open OUnit2
open Day14

let example_input = "p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3"

let make_part1_test name expected_output (robots, width, height) = 
  name >:: (fun _ -> 
    assert_equal expected_output (part1 (robots, width, height)) 
      ~printer:string_of_int)

let part1_tests = [
  make_part1_test "example_part1" 12 (parse example_input, 7, 11);
]

let suite = "Day14 Test Suite" >::: [
  "part1 tests" >::: part1_tests;
]

let () = run_test_tt_main suite

let () =
  let input = In_channel.input_all In_channel.stdin |> String.trim in
  let robots = parse input in
  let start_time = Unix.gettimeofday () in

  let result = part1 (robots, 101, 103) in
  result |> Printf.printf "Part 1: %d\n";

  let end_time = Unix.gettimeofday () in
  Printf.printf "Elapsed time: %.4f seconds\n" (end_time -. start_time);
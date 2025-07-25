open OUnit2
open Day14

let example_input =
  "p=0,4 v=3,-3\n\
   p=6,3 v=-1,-3\n\
   p=10,3 v=-1,2\n\
   p=2,0 v=2,-1\n\
   p=0,0 v=1,3\n\
   p=3,0 v=-2,-2\n\
   p=7,6 v=-1,-3\n\
   p=3,0 v=-1,-2\n\
   p=9,3 v=2,3\n\
   p=7,3 v=-1,2\n\
   p=2,4 v=2,-3\n\
   p=9,5 v=-3,-3"

let make_part1_test name expected_output (robots, width, height) =
  name >:: fun _ ->
  assert_equal expected_output
    (part1 (robots, width, height))
    ~printer:string_of_int

let part1_tests =
  [ make_part1_test "example_part1" 12 (example_input |> parse, 7, 11) ]

let suite = "Day14 Test Suite" >::: [ "part1 tests" >::: part1_tests ]
let () = run_test_tt_main suite

(* Main entry for day 14 algorithm *)
let () =
  let has_input =
    try
      let _ = Unix.select [ Unix.stdin ] [] [] 0.0 in
      true
    with Unix.Unix_error _ -> false
  in

  if has_input then
    let input = In_channel.input_all In_channel.stdin |> String.trim in
    if String.length input > 0 then (
      let robots = parse input in
      let start_time = Unix.gettimeofday () in

      (robots, 101, 103) |> part1 |> Printf.printf "Part 1: %d\n";

      let christmas_tree_time = (robots, 101, 103) |> part2 in
      Printf.printf "Part 2: %d\n" christmas_tree_time;

      visualize_at_time robots 101 103 christmas_tree_time;

      let end_time = Unix.gettimeofday () in
      Printf.printf "Elapsed time: %.4f seconds\n" (end_time -. start_time))

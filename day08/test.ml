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


(** Main execution *)
let () =
  let input = In_channel.input_all In_channel.stdin |> String.trim in
  let grid = parse input in

  let timer_start = Unix.gettimeofday () in

  grid |> part1 |> Printf.printf "Part 1: %d\n";
  grid |> part2 |> Printf.printf "Part 2: %d\n";

  let timer_end = Unix.gettimeofday () in

  let elapsed = timer_end -. timer_start in

  Printf.printf "Elapsed time: %.4f seconds\n" (elapsed)
open OUnit2
open Day12

let small_input = "AAAA\nBBCD\nBBCC\nEEEC"
let ox_input = "OOOOO\nOXOXO\nOOOOO\nOXOXO\nOOOOO"
let ex_input = "EEEEE\nEXXXX\nEEEEE\nEXXXX\nEEEEE"
let ab_input = "AAAAAA\nAAABBA\nAAABBA\nABBAAA\nABBAAA\nAAAAAA"

let large_input =
  "RRRRIICCFF\n\
   RRRRIICCCF\n\
   VVRRRCCFFF\n\
   VVRCCCJFFF\n\
   VVVVCJJCFE\n\
   VVIVCCJJEE\n\
   VVIIICJJEE\n\
   MIIIIIJJEE\n\
   MIIISIJEEE\n\
   MMMISSJEEE"

let make_part1_test name expected_output input =
  name >:: fun _ ->
  assert_equal expected_output (part1 input) ~printer:string_of_int

let make_part2_test name expected_output input =
  name >:: fun _ ->
  assert_equal expected_output (part2 input) ~printer:string_of_int

let part1_tests =
  [
    make_part1_test "small example part1" 140 small_input;
    make_part1_test "ox example part1" 772 ox_input;
    make_part1_test "large example part1" 1930 large_input;
  ]

let part2_tests =
  [
    make_part2_test "small example part2" 80 small_input;
    make_part2_test "ox example part2" 436 ox_input;
    make_part2_test "ex example part2" 236 ex_input;
    make_part2_test "ab example part2" 368 ab_input;
    make_part2_test "large example part2" 1206 large_input;
  ]

let suite =
  "Day12 Test Suite"
  >::: [ "Part 1 Tests" >::: part1_tests; "Part 2 Tests" >::: part2_tests ]

let () = run_test_tt_main suite

(** Main entry point - reads input, runs both function parts and prints results
    with timing *)
let () =
  try
    In_channel.input_all In_channel.stdin |> String.trim |> fun input ->
    let timer_start = Unix.gettimeofday () in

    input |> part1 |> Printf.printf "Part 1: %d\n%!";
    input |> part2 |> Printf.printf "Part 2: %d\n%!";

    Unix.gettimeofday () -. timer_start
    |> Printf.printf "Elapsed time: %.4f seconds\n%!"
  with
  | Failure msg -> Printf.printf "Error: %s\n%!" msg
  | e -> Printf.printf "Unexpected error: %s\n%!" (Printexc.to_string e)

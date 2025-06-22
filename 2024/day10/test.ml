open OUnit2
open Day10

let example_input =
  "89010123\n\
   78121874\n\
   87430965\n\
   96549874\n\
   45678903\n\
   32019012\n\
   01329801\n\
   10456732"

let make_part1_test name expected_output input =
  name >:: fun _ ->
  let map = parse input in
  assert_equal expected_output (part1 map) ~printer:string_of_int

let make_part2_test name expected_output input =
  name >:: fun _ ->
  let map = parse input in
  assert_equal expected_output (part2 map) ~printer:string_of_int

let part1_test = [ make_part1_test "part1 test" 36 example_input ]
let part2_test = [ make_part2_test "part2 test" 81 example_input ]

let suite =
  "Day10 Test Suite"
  >::: [ "part1 tests" >::: part1_test; "part2 tests" >::: part2_test ]

let () = run_test_tt_main suite

(** Main execution *)
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
      let height_map = parse input in

      let start_time = Unix.gettimeofday () in

      height_map |> part1 |> Printf.printf "Part 1: %d\n";
      height_map |> part2 |> Printf.printf "Part 2: %d\n";

      let end_time = Unix.gettimeofday () in
      Printf.printf "Elapsed time: %.4f seconds\n" (end_time -. start_time))

(** Test suite for Day 23: Network Triangle Challenge Tests the solution using
    example network graph inputs. *)

open OUnit2
open Day23

(** Example input from the challenge *)
let example_input =
  "kh-tc\n\
   qp-kh\n\
   de-cg\n\
   ka-co\n\
   yn-aq\n\
   qp-ub\n\
   cg-tb\n\
   vc-aq\n\
   tb-ka\n\
   wh-tc\n\
   yn-cg\n\
   kh-ub\n\
   ta-co\n\
   de-co\n\
   tc-td\n\
   tb-wq\n\
   wh-td\n\
   ta-ka\n\
   td-qp\n\
   aq-cg\n\
   wq-ub\n\
   ub-vc\n\
   de-ta\n\
   wq-aq\n\
   wq-vc\n\
   wh-yn\n\
   ka-de\n\
   kh-ta\n\
   co-tc\n\
   wh-qp\n\
   tb-vc\n\
   td-yn"

(** Helper function to create test cases for part1 *)
let make_part1_test name input expected =
  name >:: fun _ ->
  let connections = input |> parse |> to_optimized_dict in
  assert_equal expected (part1 connections) ~printer:string_of_int

(** Helper function to create test cases for part2 *)
let make_part2_test name input expected =
  name >:: fun _ ->
  let connections = input |> parse |> to_optimized_dict in
  assert_equal expected (part2 connections) ~printer:(fun s -> s)

(** Part 1 test cases *)
let part1_tests = [ make_part1_test "example_part1" example_input 7 ]

(** Part 2 test cases *)
let part2_tests =
  [ make_part2_test "example_part2" example_input "co,de,ka,ta" ]

(** Complete test suite *)
let suite =
  "Day23 Test Suite"
  >::: [ "Part 1 Tests" >::: part1_tests; "Part 2 Tests" >::: part2_tests ]

(** Run the tests *)
let () = run_test_tt_main suite

(** Main entry point for solution *)
let () =
  try
    (* Read input from stdin *)
    let has_input =
      try
        let _ = Unix.select [ Unix.stdin ] [] [] 0.0 in
        true
      with Unix.Unix_error _ -> false
    in

    if has_input then
      let input = In_channel.input_all In_channel.stdin |> String.trim in
      if String.length input > 0 then (
        Printf.printf "Input length: %d\n%!" (String.length input);

        (* Parse input into connections *)
        let connections = parse input |> to_optimized_dict in
        Printf.printf "Connections count: %d\n%!"
          (StringMap.cardinal connections);

        (* Solve Part 1 and Part 2 with timing *)
        let start_time = Unix.gettimeofday () in

        let result1 = part1 connections in
        let result2 = part2 connections in

        let end_time = Unix.gettimeofday () in

        Printf.printf "Part 1: %d\n%!" result1;
        Printf.printf "Part 2: %s\n%!" result2;
        Printf.printf "Clique size: %d\n%!"
          (String.split_on_char ',' result2 |> List.length);
        Printf.printf "Elapsed time: %.4f seconds\n%!" (end_time -. start_time))
  with
  | Failure msg -> Printf.printf "Error: %s\n" msg
  | e -> Printf.printf "Unexpected error: %s\n" (Printexc.to_string e)

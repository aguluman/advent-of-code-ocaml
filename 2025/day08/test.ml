open OUnit2
open Day08

(** Example input from the challenge *)
let example_input =
  "162,817,812\n\
   57,618,57\n\
   906,360,560\n\
   592,479,940\n\
   352,342,300\n\
   466,668,158\n\
   542,29,236\n\
   431,825,988\n\
   739,650,466\n\
   52,470,668\n\
   216,146,977\n\
   819,987,18\n\
   117,168,530\n\
   805,96,715\n\
   346,949,466\n\
   970,615,88\n\
   941,993,340\n\
   862,61,35\n\
   984,92,344\n\
   425,690,689\n"

(** Helper function to create test cases for part1 *)
let make_part1_test name expected_output input =
  name >:: fun _ ->
  assert_equal expected_output (part1 input) ~printer:string_of_int

(** Helper function to create test cases for part2 *)
let make_part2_test name expected_output input =
  name >:: fun _ ->
  assert_equal expected_output (part2 input) ~printer:Int64.to_string

(** Part 1 test cases *)
let part1_tests = [ make_part1_test "example_part1" 40 example_input ]

(** Part 2 test cases *)
let part2_tests = [ make_part2_test "example_part2" 25272L example_input ]

(** Complete test suite *)
let suite =
  "Day08 Test Suite"
  >::: [ "Part 1 Tests" >::: part1_tests; "Part 2 Tests" >::: part2_tests ]

(** Run the tests *)
let () = run_test_tt_main suite

(** Main entry point - reads input, runs both function parts and prints results
    with timing *)
let () =
  try
    let has_input =
      try
        let _ = Unix.select [ Unix.stdin ] [] [] 0.0 in
        true
      with Unix.Unix_error _ -> false
    in

    if has_input then
      let input = In_channel.input_all In_channel.stdin |> String.trim in
      if String.length input > 0 then (
        let timer_start = Unix.gettimeofday () in

        input |> part1 |> Printf.printf "Part 1: %d\n%!";
        input |> part2 |> Printf.printf "Part 2: %Ld\n%!";

        Unix.gettimeofday () -. timer_start
        |> Printf.printf "Elapsed time: %.4f seconds\n%!")
  with
  | Failure msg -> Printf.printf "Error: %s\n%!" msg
  | e -> Printf.printf "Unexpected error: %s\n%!" (Printexc.to_string e)

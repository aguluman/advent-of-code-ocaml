open OUnit2
open Day03

(* Example inputs *)
let input1 =
  "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"

let input2 = 
  "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"

(* Helper function for parse1 tests *)
let make_parse1_test name expected_output input =
  name >:: (fun _ ->
    let result = parse1 input in
    assert_equal expected_output result ~printer:(fun lst -> 
      "[" ^ (String.concat "; " (List.map (fun (x, y) -> 
        "(" ^ string_of_int x ^ ", " ^ string_of_int y ^ ")") lst)) ^ "]"))

(* Helper function for part1 tests *)
let make_part1_test name expected_output input =
  name >:: (fun _ ->
    let parsed = parse1 input in
    assert_equal expected_output (part1 parsed) ~printer:string_of_int)

(* Helper function for parse2 tests *)
let make_parse2_test name expected_output input =
  name >:: (fun _ ->
    let result = parse2 input in
    assert_equal expected_output result ~printer:(fun lst -> 
      "[" ^ (String.concat "; " (List.map (function 
        | Multiply(x, y) -> "Multiply(" ^ string_of_int x ^ ", " ^ string_of_int y ^ ")"
        | Enable -> "Enable"
        | Disable -> "Disable") lst)) ^ "]"))

(* Helper function for part2 tests *)
let make_part2_test name expected_output input =
  name >:: (fun _ ->
    let parsed = parse2 input in
    assert_equal expected_output (part2 parsed) ~printer:string_of_int)

(* Test cases *)
let parse1_tests = [
  make_parse1_test "testParse1" [(2, 4); (5, 5); (11, 8); (8, 5)] input1;
]

let part1_tests = [
  make_part1_test "testPart1" 161 input1;
]

let parse2_tests = [
  make_parse2_test "testParse2" 
    [Multiply(2, 4); Disable; Multiply(5, 5); Multiply(11, 8); Enable; Multiply(8, 5)] 
    input2;
]

let part2_tests = [
  make_part2_test "testPart2" 48 input2;
]

(* Main test suite *)
let suite = "Day03 Test Suite" >::: [
  "parse1 tests" >::: parse1_tests;
  "part1 tests" >::: part1_tests;
  "parse2 tests" >::: parse2_tests;
  "part2 tests" >::: part2_tests;
]

let () = run_test_tt_main suite

let () =
  let input = In_channel.input_all In_channel.stdin |> String.trim in

  let start_time = Unix.gettimeofday () in

  input 
  |> parse1 
  |> part1 
  |> Printf.printf "Part 1: %d\n";

  input 
  |> parse2 
  |> part2 
  |> Printf.printf "Part 2: %d\n";

  let end_time = Unix.gettimeofday () in
  Printf.printf "Elapsed time: %.4f seconds\n%!" (end_time -. start_time);



open OUnit2
open Day13

let example_input = "Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279"

let make_part1_test name expected_output input = 
  name >:: (fun _ -> 
    let machines = parse input in
    assert_equal expected_output (calculate_minimum_tokens machines) 
    ~printer:string_of_int)

let part1_tests = [
  make_part1_test "example_part1" 480 example_input;
]

let suite = "Day13 Test Suite" >::: [
  "part1 tests" >::: part1_tests;
]

let () = run_test_tt_main suite


(** Main day13.ml program entry point *)
let () =
  In_channel.input_all In_channel.stdin
  |> String.trim
  |> parse
  |> (fun machines ->
  
      let start_time = Unix.gettimeofday () in
      
      machines |> calculate_minimum_tokens |> Printf.printf "Part 1: %d\n";
      machines |> calculate_large_coordinate_tokens |> Printf.printf "Part 2: %Ld\n";
      
      Unix.gettimeofday () -. start_time)
  |> Printf.printf "Elapsed time: %.4f seconds\n"
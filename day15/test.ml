open OUnit2
open Day15
open Day15_part1
open Day15_part2


(** Common print function for grid visualization *)
let print_grid label state =
  Printf.printf "\n%s:\n" label;
  Array.iter (fun row ->
    Array.iter (function
      | Robot -> print_char '@'
      | Box -> print_char 'O'
      | Wall -> print_char '#'
      | Empty -> print_char '.'
    ) row;
    print_newline ()
  ) state

(** Movement print functions with shared visualization logic *)
let move_right_print map =
  let reverse = Array.map array_rev in
  map 
  |> (fun m -> print_grid "Before reverse" m; reverse m)
  |> (fun m -> print_grid "After first reverse" m; move_left m)
  |> (fun m -> print_grid "After move_left" m; reverse m)
  |> (fun m -> print_grid "Final" m; m)

let move_down_print map =
  map 
  |> (fun m -> print_grid "Initial" m; transpose m)
  |> (fun m -> print_grid "After transpose" m; move_right m)
  |> (fun m -> print_grid "After move_left" m; transpose m)
  |> (fun m -> print_grid "Final" m; m)
  

(** Test input data *)
let example_input = "########
#..O.O.#
##@.O..#
#...O..#
#.#.O..#
#...O..#
#......#
########

<^^>>>vv<v>>v<<"

let example_input_large = "##########
#..O..O.O#
#......O.#
#.OO..O.O#
#..O@..O.#
#O#..O...#
#O..O..O.#
#.OO.O.OO#
#....O...#
##########

<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^"


(** Helper function to create movement tests *)
let make_move_test name input movement expected = 
  name >:: (fun _ -> 
    let map = parse_map input in
    let moved = movement map in
    let expected_map = parse_map expected in (* Use existing parse_map *)
    assert_equal expected_map moved ~printer:(fun m -> 
      m |> Array.map (fun row ->
        row |> Array.map (function
          | Robot -> "@"
          | Box -> "O"
          | Wall -> "#"
          | Empty -> "."
        ) |> Array.to_list |> String.concat ""
      ) 
      |> Array.to_list 
      |> String.concat "\n"))


(** Basic movement tests *)
let movement_tests = [
  make_move_test "move_left test" 
  "#..OO@." 
  move_left 
  "#.OO@..";
  
  make_move_test "move_right test" 
  "#.@OO.." 
  move_right_print
  "#..@OO.";

  (* Adjust expectations for vertical tests to match actual behavior *)
  make_move_test "move_up test" 
    "######\n#....#\n#.OO@#\n######"
    move_up 
    "######\n#...@#\n#.OO.#\n######";  (* Robot moves up but boxes stay *)
    
  make_move_test "move_down test" 
    "######\n#.@OO#\n#....#\n######"
    move_down_print 
    "######\n#..OO#\n#.@..#\n######";  (* Robot moves down but boxes stay *)
]

(** Helper function for direct string comparison tests *)
let make_direct_scale_up_test name input movement expected_str = 
  name >:: (fun _ -> 
    let map = parse_map input |> scale_up in
    let moved = movement map in
    let actual_str = moved |> Array.map (fun row ->
      row |> Array.map (function
        | ScaledRobot -> "@" 
        | BoxL -> "[" 
        | BoxR -> "]"
        | ScaledWall -> "#" 
        | ScaledEmpty -> "."
      ) |> Array.to_list |> String.concat ""
    ) |> Array.to_list |> String.concat "\n" in
    assert_equal expected_str actual_str)


(** Helper function for scaled movement tests *)
let make_scaled_move_test name input movement expected = 
  name >:: (fun _ -> 
    let map = parse_map input |> scale_up in
    let moved = movement map in
    let expected_map = parse_map expected |> scale_up in
    assert_equal expected_map moved ~printer:(fun m -> 
      m |> Array.map (fun row ->
        row |> Array.map (function
          | ScaledRobot -> "@"
          | BoxL -> "["
          | BoxR -> "]"
          | ScaledWall -> "#"
          | ScaledEmpty -> "."
        ) |> Array.to_list |> String.concat ""
      ) 
      |> Array.to_list 
      |> String.concat "\n"))


(** Scaled movement tests - using direct string comparison for exact matching *)
let scaled_movement_tests = [
  (* Keep inspection test for reference *)
  "scaled_output_inspection" >:: (fun _ ->
    let map = parse_map "#..OO@." |> scale_up in
    Printf.printf "\nScaled map representation:\n%s\n" 
      (map |> Array.map (fun row ->
        row |> Array.map (function
          | ScaledRobot -> "@" 
          | BoxL -> "[" 
          | BoxR -> "]"
          | ScaledWall -> "#" 
          | ScaledEmpty -> "."
        ) |> Array.to_list |> String.concat ""
      ) |> Array.to_list |> String.concat "\n"));

    
  (* Use direct string comparison for horizontal movements *)
  make_direct_scale_up_test "move_left_scaled test"
    "######\n#....#\n#.OO@#\n######"
    move_left_scaled
    "############\n##........##\n##.[][]@..##\n############";

  make_direct_scale_up_test "move_right_scaled test"
    "#######\n#.....#\n#.OO@.#\n#######"
    move_right_scaled
    "##############\n##..........##\n##..[][].@..##\n##############";

  
  (* Update expectations for vertical movements *)
  make_scaled_move_test "move_up_scaled test"
    "######\n#....#\n#.OO@#\n######"
    move_up_scaled
    "######\n#...@#\n#.OO.#\n######";
    
  make_scaled_move_test "move_down_scaled test"
    "######\n#.@OO#\n#....#\n######"
    move_down_scaled
    "######\n#..OO#\n#.@..#\n######";
]


(** Full Game Logic Tests *)
let game_tests = [
  "part1 example" >:: (fun _ ->
    let (map, moves) = parse example_input in
    assert_equal 2028 (part1 (map, moves)));

  "part2 example" >:: (fun _ ->
    let (map, moves) = parse example_input_large in
    assert_equal 9021 (part2 (map, moves)));
]

(** Complete test suite *)
let suite = "Day15 Test Suite" >::: [
  "Movement Tests" >::: movement_tests;
  "Scaled Movement Tests" >::: scaled_movement_tests;
  "Game Tests" >::: game_tests;
]

(** Run the tests *)
let () = run_test_tt_main suite


(** Main entry point *)
let () =
  In_channel.input_all In_channel.stdin
  |> String.trim
  |> parse
  |> (fun input ->

      let start_time = Unix.gettimeofday () in

      input |> part1 |> Printf.printf "\nPart 1: %d\n";
      input |> part2 |> Printf.printf "Part 2: %d\n";

      Unix.gettimeofday () -. start_time)
  |> Printf.printf "Elapsed time: %.4f seconds\n"
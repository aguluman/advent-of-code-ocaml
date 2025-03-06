open Day15part1
open Day15part2

(** Parse warehouse map from string input
    @param input String containing warehouse layout
    @return Warehouse state with initial positions *)
let parse_map input =
  let lines = String.split_on_char '\n' input |> List.filter (fun s -> s <> "") in
  let height = List.length lines in
  let width = String.length (List.hd lines) in
  let cells = Array.make_matrix height width Empty in
  let robot_pos = ref (0, 0) in
  
  List.iteri (fun row line ->
    String.iteri (fun col ch ->
      cells.(row).(col) <- match ch with
        | '@' -> robot_pos := (row, col); Robot
        | 'O' -> Box
        | '#' -> Wall
        | '.' -> Empty
        | c -> failwith (Printf.sprintf "Invalid character in map: %c" c)
    ) line
  ) lines;
  
  { cells; robot_pos = !robot_pos; width; height }

(** Parse complete input into warehouse and movement sequence
    @param input Raw puzzle input string
    @return Tuple of initial warehouse state and movement directions *)
let parse input =
  let parts = String.split_on_char '\n' input 
              |> List.filter (fun s -> s <> "") 
              |> List.partition (fun s -> 
                   String.length s > 0 && String.contains "#@O." s.[0]) in
  match parts with
  | (map_lines, move_lines) ->
      let warehouse = parse_map (String.concat "\n" map_lines) in
      let moves = String.concat "" move_lines
                 |> String.to_seq
                 |> List.of_seq
                 |> List.filter_map (function
                     | '^' -> Some Up
                     | 'v' -> Some Down
                     | '<' -> Some Left
                     | '>' -> Some Right
                     | _ -> None) in
      (warehouse, moves)


(** Main entry point *)
let () =
  In_channel.input_all In_channel.stdin
  |> String.trim
  |> parse
  |> (fun input ->
      let start_time = Unix.gettimeofday () in
      let result1 = part1 input in
      let result2 = part2 input in
      Printf.printf "Part 1: %d\nPart 2: %d\n" result1 result2;
      Unix.gettimeofday () -. start_time)
  |> Printf.printf "Elapsed time: %.4f seconds\n"
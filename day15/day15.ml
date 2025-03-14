(** Day 15: Push Box Game Implementation
    This module implements a Sokoban-like box pushing game where a robot needs to
    push boxes around a warehouse. The game supports both normal and scaled modes.
    
    The warehouse is represented as a 2D grid where:
    - @ represents the robot
    - O represents boxes
    - # represents walls
    - . represents empty spaces

    Movement can be in four directions (Up, Down, Left, Right) and boxes can be
    pushed if there's empty space behind them.

    @see <https://adventofcode.com/2024/day/15> Problem Description
*)

open Day15_part1


(** Input Parsing *)

(** [parse_map input] parses the warehouse grid from string input
    @param input Multiline string containing the warehouse layout
    @return 2D array representing the warehouse
    @raise Failure if input is empty or contains invalid characters *)
let parse_map input =
  let lines = String.split_on_char '\n' input |> List.filter (fun s -> s <> "") in
  if List.length lines = 0 then
    failwith "Empty map input";
  
  let height = List.length lines in
  let width = String.length (List.hd lines) in
  let cells = Array.make_matrix height width Empty in
  
  List.iteri (fun row line ->
    String.iteri (fun col ch ->
      cells.(row).(col) <- match ch with
        | '@' -> Robot
        | 'O' -> Box
        | '#' -> Wall
        | '.' -> Empty
        | c -> failwith (Printf.sprintf "Invalid character in map: %c" c)
    ) line
  ) lines;
  
  cells  (* Return just the 2D array, not a record *)


  
(** [parse input] Parse complete input into warehouse and movement sequence
    @param input Raw puzzle input string
    @return Tuple of initial warehouse state and movement directions *)
let parse input =
  let parts = String.split_on_char '\n' input 
              |> List.filter (fun s -> s <> "") 
              |> List.partition (fun s -> 
                   String.length s > 0 && 
                   (String.contains "@O#." s.[0] || 
                    (String.length s > 1 && 
                     (s.[0] = '@' || s.[0] = 'O' || s.[0] = '#' || s.[0] = '.')))) in
  
  match parts with
  | (map_lines, move_lines) ->
      let map_str = String.concat "\n" map_lines in
      Printf.printf "\nMap lines found: %d\n" (List.length map_lines);
      
      let moves_str = String.concat "" move_lines in
      Printf.printf "Moves string length: %d\n" (String.length moves_str);
      
      let warehouse = parse_map map_str in
      let moves = moves_str
                 |> String.to_seq
                 |> Seq.filter_map (function
                     | '^' -> Some Up
                     | 'v' -> Some Down
                     | '<' -> Some Left
                     | '>' -> Some Right
                     | _ -> None) in
      (warehouse, moves)



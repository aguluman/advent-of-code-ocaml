(** Day 12: Christmas Tree Farm

    Determine how many regions under Christmas trees can fit all required
    presents. Presents are polyomino-like shapes (3x3 grids with '#' cells) that
    must be placed without overlap. For large grids, checking if total shape
    area fits within the region area gives the correct answer.

    {2 Problem Summary:}
    - {b Part 1:} Count the number of regions where presents can fit.
    - {b Part 2:} Final day - no part 2!

    See details at:
    {{:https://adventofcode.com/2025/day/12} Advent of Code 2025, Day 12} *)

let pair_of_list = function
  | [ a; b ] -> (a, b)
  | _ -> failwith "pair_of_list"

(** Helper function to parse input string into a structured data format

    @param input Raw input string from the puzzle
    @return Parsed data structure *)
let parse input =
  let lines =
    String.split_on_char '\n' input
    |> List.filter (fun s -> String.trim s <> "")
  in
  let problems, tiles =
    List.partition
      (fun line -> String.index_opt line 'x' |> Option.is_some)
      lines
  in
  let tile_sizes =
    let rec loop = function
      | [] -> []
      | a :: b :: c :: tl ->
          String.fold_left
            (fun acc ch -> if ch = '#' then acc + 1 else acc)
            0
            (a ^ b ^ c)
          :: loop tl
      | _ -> []
    in
    List.filter (fun line -> String.length line = 3) tiles
    |> loop |> Array.of_list
  in
  (problems, tile_sizes)

(** [solve_problem tile_sizes problem] checks if a region can fit all required
    tiles.

    Parses a problem line "WxH: n0 n1 n2 ..." and compares the region area (W*H)
    against the total area of all required tiles (sum of ni * tile_size_i).

    @param tile_sizes Array of tile sizes (number of '#' cells per tile)
    @param problem A problem line string in format "WxH: n0 n1 n2 ..."
    @return [true] if total tile area fits within region area, [false] otherwise
*)
let solve_problem tile_sizes problem =
  let chunks =
    String.split_on_char ' ' problem |> List.filter (fun s -> s <> "")
  in
  let hd = List.hd chunks in
  let w, h =
    String.sub hd 0 (String.length hd - 1)
    |> String.split_on_char 'x' |> List.map int_of_string |> pair_of_list
  in
  let area = w * h in
  let tile_area =
    List.tl chunks
    |> List.mapi (fun i n -> tile_sizes.(i) * int_of_string n)
    |> List.fold_left ( + ) 0
  in
  area >= tile_area

(** [part1 input] solves part 1 of the challenge

    @param input Raw input string from the puzzle
    @return Number of regions that can fit all their required presents *)
let part1 input =
  let problems, tile_sizes = parse input in
  List.fold_left
    (fun acc p -> if solve_problem tile_sizes p then acc + 1 else acc)
    0 problems

(** [part2 input] solves part 2 of the challenge

    @param input Raw input string from the puzzle
    @return Solution for part 2 *)
let part2 input =
  let _ = parse input in
  (* No Part2 Solution on the last day of advent of code *)
  0L

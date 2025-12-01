(** Day 01: Secret Entrance

    [Brief description of the problem and what this module solves]

    {2 Problem Summary:}
    - {b Part 1:} [Description of part 1]
    - {b Part 2:} [Description of part 2]

    See details at:
    {{:https://adventofcode.com/2025/day/01} Advent of Code 2025, Day 01} *)

type quadrant =
  | TopLeft  (** Positions 26-50 *)
  | BottomLeft  (** Positions 0-25 *)
  | BottomRight  (** Positions 75-99 *)
  | TopRight  (** Positions 51-74 *)

type dial = {
  position : int;
  quadrant : quadrant;
}

type direction =
  | Left
  | Right

type rotation = {
  dir : direction;
  amount : int;
}

let get_quadrant pos =
  if pos >= 26 && pos <= 50 then TopLeft
  else if pos >= 0 && pos <= 25 then BottomLeft
  else if pos >= 75 && pos <= 99 then BottomRight
  else TopRight

let make_dial position = { position; quadrant = get_quadrant position }

let parse_rotation s =
  let s = String.trim s in
  if String.length s < 2 then None
  else
    let dir_char = s.[0] in
    let amount_str = String.sub s 1 (String.length s - 1) in
    match dir_char with
    | 'L' ->
        Option.map
          (fun amount -> { dir = Left; amount })
          (int_of_string_opt amount_str)
    | 'R' ->
        Option.map
          (fun amount -> { dir = Right; amount })
          (int_of_string_opt amount_str)
    | _ -> None

(** Helper function to parse input string into a structured data format

    @param input Raw input string from the puzzle
    @return Parsed data structure *)
let parse input =
  input |> String.split_on_char '\n'
  |> List.filter (fun line -> String.trim line <> "")
  |> List.filter_map parse_rotation

let rotate dial rotation =
  let delta =
    match rotation.dir with
    | Left -> -rotation.amount (* -ve for left *)
    | Right -> rotation.amount (* +ve for right*)
  in
  let new_pos = (dial.position + delta) mod 100 in
  let new_pos = if new_pos < 0 then new_pos + 100 else new_pos in
  make_dial new_pos

(** [part1 input] solves part 1 of the challenge

    @param input Raw input string from the puzzle
    @return Number of times dial pointed to 0 *)
let part1 input =
  let rotations = parse input in
  let starting_dial = make_dial 50 in

  let _, zero_count =
    List.fold_left
      (fun (current_dial, count) rotation ->
        let new_dial = rotate current_dial rotation in
        let new_count = if new_dial.position = 0 then count + 1 else count in
        (new_dial, new_count))
      (starting_dial, 0) rotations
  in
  zero_count

let count_zero_crossings start rotation =
  match rotation.dir with
  | Left ->
      if start = 0 then rotation.amount / 100
      else if rotation.amount < start then 0
      else ((rotation.amount - start) / 100) + 1
  | Right ->
      let distance_to_zero = 100 - start in
      if start = 0 then rotation.amount / 100
      else if rotation.amount < distance_to_zero then 0
      else ((rotation.amount - distance_to_zero) / 100) + 1

(** [part2 input] solves part 2 of the challenge

    @param input Raw input string from the puzzle
    @return Total Count of 0 pointings *)
let part2 input =
  let rotations = parse input in
  let starting_dial = make_dial 50 in

  let _, total_zeros =
    List.fold_left
      (fun (current_dial, count) rotation ->
        let crossings = count_zero_crossings current_dial.position rotation in
        let new_dial = rotate current_dial rotation in
        (new_dial, count + crossings))
      (starting_dial, 0) rotations
  in
  Int64.of_int total_zeros

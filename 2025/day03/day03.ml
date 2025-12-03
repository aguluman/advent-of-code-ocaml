(** Day 03: Lobby

    [Brief description of the problem and what this module solves]

    {2 Problem Summary:}
    - {b Part 1:} [Description of part 1]
    - {b Part 2:} [Description of part 2]

    See details at:
    {{:https://adventofcode.com/2025/day/3} Advent of Code 2025, Day 03} *)

type bank = string

(** Helper function to parse input string into a structured data format

    @param input Raw input string from the puzzle
    @return Parsed data structure *)
let parse input =
  input |> String.split_on_char '\n'
  |> List.filter (fun line -> String.trim line <> "")

let joltage bank =
  let n = String.length bank in
  if n < 2 then 0
  else
    let suffix_max = Array.make n 0 in
    suffix_max.(n - 1) <- Char.code (String.unsafe_get bank (n - 1)) - 48;
    for j = n - 2 downto 0 do
      let dj = Char.code (String.unsafe_get bank j) - 48 in
      suffix_max.(j) <-
        (if dj > suffix_max.(j + 1) then dj else suffix_max.(j + 1))
    done;

    let max_jolt = ref 0 in
    for i = 0 to n - 2 do
      let di = Char.code (String.unsafe_get bank i) - 48 in
      let num = (10 * di) + suffix_max.(i + 1) in
      if num > !max_jolt then max_jolt := num
    done;
    !max_jolt

(** [part1 input] solves part 1 of the challenge

    @param input Raw input string from the puzzle
    @return Solution for part 1 *)
let part1 input =
  let banks = parse input in
  List.fold_left (fun acc bank -> acc + joltage bank) 0 banks

let joltage_two bank =
  let n = String.length bank in
  if n < 12 then 0L
  else
    let rec aux acc pos remaining =
      if remaining = 0 then acc
      else
        let end_idx = n - remaining in
        let max_char = ref '\x00' in
        let max_idx = ref pos in
        for i = pos to end_idx do
          let c = String.unsafe_get bank i in
          if c > !max_char then begin
            max_char := c;
            max_idx := i
          end
        done;
        let digit = Int64.of_int (Char.code !max_char - 48) in
        aux (Int64.add (Int64.mul acc 10L) digit) (!max_idx + 1) (remaining - 1)
    in
    aux 0L 0 12

(** [part2 input] solves part 2 of the challenge

    @param input Raw input string from the puzzle
    @return Solution for part 2 *)
let part2 input =
  let banks = parse input in
  List.fold_left (fun acc bank -> Int64.add acc (joltage_two bank)) 0L banks

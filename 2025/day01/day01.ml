(** Day 01: Secret Entrance

    [Brief description of the problem and what this module solves]

    {2 Problem Summary:}
    - {b Part 1:} Count times dial lands exactly on 0
    - {b Part 2:} Count total zero crossings during rotations

    See details at:
    {{:https://adventofcode.com/2025/day/1} Advent of Code 2025, Day 01} *)

(** Positive modulo - handles negative numbers correctly *)
let[@inline] pmod a b =
  let m = a mod b in
  if m < 0 then m + b else m

(** Process input directly, calling [f] for each parsed rotation. Returns final
    accumulator. Avoids all intermediate list allocations. *)
let fold_rotations input init f =
  let len = String.length input in
  let rec skip_whitespace i =
    if i >= len then i
    else
      match String.unsafe_get input i with
      | ' ' | '\t' | '\n' | '\r' -> skip_whitespace (i + 1)
      | _ -> i
  in
  let rec parse_int acc i =
    if i >= len then (acc, i)
    else
      match String.unsafe_get input i with
      | '0' .. '9' as c -> parse_int ((acc * 10) + Char.code c - 48) (i + 1)
      | _ -> (acc, i)
  in
  let rec loop acc i =
    let i = skip_whitespace i in
    if i >= len then acc
    else
      let dir = String.unsafe_get input i in
      if dir <> 'L' && dir <> 'R' then loop acc (i + 1)
      else
        let amount, next_i = parse_int 0 (i + 1) in
        if amount = 0 then loop acc next_i
        else
          let delta = if dir = 'R' then amount else -amount in
          loop (f acc delta) next_i
  in
  loop init 0

(** [part1 input] solves part 1 of the challenge

    @param input Raw input string from the puzzle
    @return Solution for part 1 *)
let part1 input =
  let zeros, _ =
    fold_rotations input (0, 50) (fun (zeros, pos) delta ->
        let pos' = pmod (pos + delta) 100 in
        let zeros' = if pos' = 0 then zeros + 1 else zeros in
        (zeros', pos'))
  in
  zeros

(** [part2 input] solves part 2 of the challenge

    @param input Raw input string from the puzzle
    @return Solution for part 2 *)
let part2 input =
  let total, _ =
    fold_rotations input (0, 50) (fun (total, pos) delta ->
        let pos' = pmod (pos + delta) 100 in
        (* Count crossings (not landing) *)
        let crossings =
          if
            pos <> 0 && pos' <> 0
            && ((delta < 0 && pos' >= pos) || (delta > 0 && pos' <= pos))
          then 1
          else 0
        in
        (* Count full loops *)
        let full_loops = (abs delta - 1) / 100 in
        (* Count landing on 0 *)
        let landing = if pos' = 0 then 1 else 0 in
        (total + crossings + full_loops + landing, pos'))
  in
  total

(** Day 02: Gift Shop

    Identifies invalid product IDs in given ranges. Invalid IDs consist of
    repeated digit sequences.

    {2 Problem Summary:}
    - {b Part 1:} Sum IDs made of a sequence repeated exactly twice (e.g., 1212,
      5555)
    - {b Part 2:} Sum IDs made of a sequence repeated at least twice (e.g.,
      123123123)

    {2 Optimization Notes:}
    - Uses integer math instead of string operations (no allocations)
    - Generates invalid IDs directly instead of enumerating all IDs in range
    - Avoids closure allocations in hot paths
    - O(digit_lengths) instead of O(range_size)

    See details at:
    {{:https://adventofcode.com/2025/day/2} Advent of Code 2025, Day 02} *)

(** {1 Utility Functions} *)

(** Compute 10^n efficiently — no closure, tail recursive *)
let rec pow10_aux acc n = if n <= 0 then acc else pow10_aux (acc * 10) (n - 1)

let pow10 n = if n < 0 then 0 else pow10_aux 1 n

(** Count digits in n — no closure, direct recursion *)
let rec digits_aux count p10 n =
  if n < p10 then count else digits_aux (count + 1) (p10 * 10) n

let digits n = if n = 0 then 1 else digits_aux 1 10 n

(** {1 Parsing} *)

(** Parse a single range "start-end" into a tuple *)
let parse_range range =
  let range = String.trim range in
  match String.index_opt range '-' with
  | None -> None
  | Some dash_pos -> (
      let start_str = String.sub range 0 dash_pos in
      let end_str =
        String.sub range (dash_pos + 1) (String.length range - dash_pos - 1)
      in
      match (int_of_string_opt start_str, int_of_string_opt end_str) with
      | Some s, Some e -> Some (s, e)
      | _ -> None)

(** Parse input into list of ranges *)
let parse input =
  input |> String.split_on_char ',' |> List.filter_map parse_range

(** {1 Part 1: Exactly Twice Repeats} *)

(** Construct an invalid ID from its "half" value. E.g., half=12 with 2 digits
    -> 12 * 100 + 12 = 1212 *)
let make_invalid half =
  let d = digits half in
  (half * pow10 d) + half

(** Find lowest half where make_invalid half >= low *)
let rec find_low half half_high low =
  if half > half_high then half
  else if make_invalid half >= low then half
  else find_low (half + 1) half_high low

(** Find highest half where make_invalid half <= high *)
let rec find_high half half_low high =
  if half < half_low then half
  else if make_invalid half <= high then half
  else find_high (half - 1) half_low high

(** Sum halves from half_low to half_high, each contributing half * multiplier
*)
let rec sum_halves half half_high multiplier total =
  if half > half_high then total
  else sum_halves (half + 1) half_high multiplier (total + (half * multiplier))

(** Sum all invalid IDs (exactly twice) in range [low, high] *)
let sum_invalid_part1 (low, high) =
  let rec sum_for_digits d acc =
    if d > 20 then acc
    else
      let half_digits = d / 2 in
      let p10_half = pow10 half_digits in
      let p10_half_minus1 = pow10 (half_digits - 1) in
      let min_half = if half_digits = 1 then 1 else p10_half_minus1 in
      let max_half = p10_half - 1 in
      let id_min = make_invalid min_half in
      let id_max = make_invalid max_half in
      if id_max < low || id_min > high then sum_for_digits (d + 2) acc
      else
        let half_low = max min_half ((low / p10_half) - 1) in
        let half_high = min max_half ((high / p10_half) + 1) in
        let half_low = find_low half_low half_high low in
        let half_high = find_high half_high half_low high in
        if half_low > half_high then sum_for_digits (d + 2) acc
        else
          let multiplier = p10_half + 1 in
          let range_sum = sum_halves half_low half_high multiplier 0 in
          sum_for_digits (d + 2) (acc + range_sum)
  in
  sum_for_digits 2 0

(** [part1 input] solves part 1 of the challenge

    @param input Raw input string from the puzzle
    @return Solution for part 1 *)
let part1 input =
  let ranges = parse input in
  List.fold_left (fun acc range -> acc + sum_invalid_part1 range) 0 ranges

(** {1 Part 2: At Least Twice Repeats} *)

(** Construct an ID from a unit repeated n times — no closure *)
let rec repeat_unit_aux acc unit p10_unit remaining =
  if remaining = 0 then acc
  else repeat_unit_aux ((acc * p10_unit) + unit) unit p10_unit (remaining - 1)

let repeat_unit unit n =
  let p10_unit = pow10 (digits unit) in
  repeat_unit_aux 0 unit p10_unit n

(** Collect units into set — no closure *)
let rec collect_units u max_unit n low high add_fn acc =
  if u > max_unit then acc
  else
    let id = repeat_unit u n in
    if id > high then acc
    else if id >= low then
      collect_units (u + 1) max_unit n low high add_fn (add_fn id acc)
    else collect_units (u + 1) max_unit n low high add_fn acc

(** Collect for unit digits — no closure *)
let rec collect_for_unit_digits ud n dhigh low high add_fn acc =
  if ud < 1 then acc
  else
    let total_digits = ud * n in
    if total_digits > dhigh then
      collect_for_unit_digits (ud - 1) n dhigh low high add_fn acc
    else
      let min_unit = if ud = 1 then 1 else pow10 (ud - 1) in
      let max_unit = pow10 ud - 1 in
      let acc = collect_units min_unit max_unit n low high add_fn acc in
      collect_for_unit_digits (ud - 1) n dhigh low high add_fn acc

(** Collect for repeats — no closure *)
let rec collect_for_repeats n dhigh low high add_fn acc =
  if n > dhigh then acc
  else
    let max_unit_digits = dhigh / n in
    let acc =
      collect_for_unit_digits max_unit_digits n dhigh low high add_fn acc
    in
    collect_for_repeats (n + 1) dhigh low high add_fn acc

(** Collect all invalid IDs (at least twice) in range [low, high] *)
let collect_invalid_part2 (low, high) =
  let module ISet = Set.Make (Int) in
  let dhigh = digits high in
  collect_for_repeats 2 dhigh low high ISet.add ISet.empty

(** [part2 input] solves part 2 of the challenge

    @param input Raw input string from the puzzle
    @return Solution for part 2 *)
let part2 input =
  let ranges = parse input in
  let module ISet = Set.Make (Int) in
  let all_invalid =
    List.fold_left
      (fun acc range -> ISet.union acc (collect_invalid_part2 range))
      ISet.empty ranges
  in
  ISet.fold (fun id acc -> Int64.add acc (Int64.of_int id)) all_invalid 0L

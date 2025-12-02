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
    - O(digit_lengths) instead of O(range_size)

    See details at:
    {{:https://adventofcode.com/2025/day/2} Advent of Code 2025, Day 02} *)

(** {1 Utility Functions} *)

(** Compute 10^n efficiently using tail recursion *)
let pow10 n =
  let rec loop acc = function
    | 0 -> acc
    | n -> loop (acc * 10) (n - 1)
  in
  if n < 0 then 0 else loop 1 n

(** Count digits in n without string conversion *)
let digits n =
  let rec loop count p10 =
    if n < p10 then count else loop (count + 1) (p10 * 10)
  in
  if n = 0 then 1 else loop 1 10

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

(** Check if id is invalid (repeated exactly twice) using only math. E.g., 1234
    -> left=12, right=34, check 12=34? No. 1212 -> left=12, right=12, check
    12=12? Yes. *)
let is_invalid_math id =
  let len = digits id in
  if len mod 2 <> 0 then false
  else
    let half = len / 2 in
    let divisor = pow10 half in
    let left = id / divisor in
    let right = id mod divisor in
    left = right

(** Construct an invalid ID from its "half" value. E.g., half=12 with 2 digits
    -> 12 * 100 + 12 = 1212 *)
let make_invalid half =
  let d = digits half in
  (half * pow10 d) + half

(** Sum all invalid IDs (exactly twice) in range [low, high] without
    enumeration. Iterates over possible digit counts (2, 4, 6, ...) and computes
    which "half" values produce invalid IDs within the range. *)
let sum_invalid_part1 (low, high) =
  let rec sum_for_digits d acc =
    if d > 20 then acc (* Max ~20 digits for int *)
    else
      let half_digits = d / 2 in
      let min_half = if half_digits = 1 then 1 else pow10 (half_digits - 1) in
      let max_half = pow10 half_digits - 1 in
      let id_min = make_invalid min_half in
      let id_max = make_invalid max_half in
      (* Skip this digit count if no overlap with range *)
      if id_max < low || id_min > high then sum_for_digits (d + 2) acc
      else
        (* Find the half values that produce IDs in [low, high] *)
        let half_low = max min_half ((low / pow10 half_digits) - 1) in
        let half_high = min max_half ((high / pow10 half_digits) + 1) in
        (* Refine bounds by checking actual IDs *)
        let rec find_low h =
          if h > half_high then h
          else if make_invalid h >= low then h
          else find_low (h + 1)
        in
        let rec find_high h =
          if h < half_low then h
          else if make_invalid h <= high then h
          else find_high (h - 1)
        in
        let half_low = find_low half_low in
        let half_high = find_high half_high in
        if half_low > half_high then sum_for_digits (d + 2) acc
        else
          (* Sum: each half h contributes h * (10^half_digits + 1) *)
          let multiplier = pow10 half_digits + 1 in
          let rec sum_halves h total =
            if h > half_high then total
            else sum_halves (h + 1) (total + (h * multiplier))
          in
          sum_for_digits (d + 2) (acc + sum_halves half_low 0)
  in
  sum_for_digits 2 0

(** [part1 input] solves part 1 — O(ranges × digit_counts) *)
let part1 input =
  let ranges = parse input in
  List.fold_left (fun acc range -> acc + sum_invalid_part1 range) 0 ranges

(** {1 Part 2: At Least Twice Repeats} *)

(** Construct an ID from a unit repeated n times. E.g., unit=12 (2 digits), n=3
    -> 121212 *)
let repeat_unit unit n =
  let d = digits unit in
  let rec loop acc remaining =
    if remaining = 0 then acc else loop ((acc * pow10 d) + unit) (remaining - 1)
  in
  loop 0 n

(** Collect all invalid IDs (at least twice) in range [low, high]. Uses a Set to
    avoid duplicates (e.g., 222222 = 2×3 = 22×3 = 222×2). *)
let collect_invalid_part2 (low, high) =
  let module ISet = Set.Make (Int) in
  let dhigh = digits high in
  let rec collect_for_repeats n acc =
    if n > dhigh then acc
    else
      (* For n repeats, unit can have 1 to dhigh/n digits *)
      let max_unit_digits = dhigh / n in
      let rec collect_for_unit_digits ud acc =
        if ud < 1 then acc
        else
          let total_digits = ud * n in
          if total_digits > dhigh then collect_for_unit_digits (ud - 1) acc
          else
            let min_unit = if ud = 1 then 1 else pow10 (ud - 1) in
            let max_unit = pow10 ud - 1 in
            let rec collect_units u acc =
              if u > max_unit then acc
              else
                let id = repeat_unit u n in
                if id > high then acc
                else if id >= low then collect_units (u + 1) (ISet.add id acc)
                else collect_units (u + 1) acc
            in
            collect_for_unit_digits (ud - 1) (collect_units min_unit acc)
      in
      collect_for_repeats (n + 1) (collect_for_unit_digits max_unit_digits acc)
  in
  collect_for_repeats 2 ISet.empty

(** [part2 input] solves part 2 — O(ranges × invalid_ids_in_range) *)
let part2 input =
  let ranges = parse input in
  let module ISet = Set.Make (Int) in
  (* Collect all invalid IDs across all ranges, avoiding duplicates *)
  let all_invalid =
    List.fold_left
      (fun acc range -> ISet.union acc (collect_invalid_part2 range))
      ISet.empty ranges
  in
  ISet.fold (fun id acc -> Int64.add acc (Int64.of_int id)) all_invalid 0L

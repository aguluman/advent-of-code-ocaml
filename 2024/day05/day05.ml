(** Day 5: Print Queue

    Page-ordering rules [X|Y] say page [X] must be printed before page [Y]. Each
    update is a list of pages.

    {2 Problem Summary:}
    - {b Part 1:} Sum the middle page of every update that is already in the
      right order.
    - {b Part 2:} Re-order every incorrect update and sum their middle pages.

    See details at:
    {{:https://adventofcode.com/2024/day/5} Advent of Code 2024, Day 5} *)

module IntMap = Map.Make (Int)
module IntSet = Set.Make (Int)

(** [(x, y)] means page [x] must come before page [y]. *)
type rule = int * int

type update = int list

type classified = {
  following : IntSet.t IntMap.t;
      (** Each page mapped to the pages that must come after it. *)
  correct : update list;
  incorrect : update list;
}

(** Every pair of distinct pages mentioned in the rules must be related by a
    rule; the comparison in {!part2} relies on it. *)
let assert_rules_are_total rules =
  let pages =
    List.concat_map (fun (p, q) -> [ p; q ]) rules |> List.sort_uniq Int.compare
  in
  let related p q =
    List.exists (fun (x, y) -> (x = p && y = q) || (x = q && y = p)) rules
  in
  List.iter
    (fun p -> List.iter (fun q -> assert (p = q || related p q)) pages)
    pages

let successors rules =
  List.fold_left
    (fun acc (p, q) ->
      IntMap.update p
        (fun after ->
          Some (IntSet.add q (Option.value after ~default:IntSet.empty)))
        acc)
    IntMap.empty rules

(** [in_order following p q] holds unless a rule forbids [p] before [q]. *)
let in_order following p q =
  let p_allows_q =
    match IntMap.find_opt p following with
    | Some after -> IntSet.mem q after
    | None -> true
  in
  let q_allows_p =
    match IntMap.find_opt q following with
    | Some after -> not (IntSet.mem p after)
    | None -> true
  in
  p_allows_q && q_allows_p

(** An update is correct when every page is in order with every later page. *)
let rec is_correct following = function
  | [] -> true
  | p :: rest ->
      List.for_all (in_order following p) rest && is_correct following rest

let classify (rules : rule list) (updates : update list) : classified =
  assert_rules_are_total rules;
  let following = successors rules in
  let correct, incorrect = List.partition (is_correct following) updates in
  { following; correct; incorrect }

let middle update = List.nth update (List.length update / 2)
let sum = List.fold_left ( + ) 0

let part1 rules updates =
  let { correct; _ } = classify rules updates in
  correct |> List.map middle |> sum

let part2 rules updates =
  let { following; incorrect; _ } = classify rules updates in
  let compare p q =
    match IntMap.find_opt p following with
    | Some after -> if IntSet.mem q after then -1 else 1
    | None -> 1
  in
  incorrect |> List.map (fun u -> middle (List.sort compare u)) |> sum

(** Lines up to the first blank line, and the lines after it. *)
let split_at_blank lines =
  let rec go acc = function
    | [] -> (List.rev acc, [])
    | "" :: rest -> (List.rev acc, rest)
    | line :: rest -> go (line :: acc) rest
  in
  go [] lines

let parse_rule line =
  match String.split_on_char '|' line with
  | [ p; q ] -> (
      match
        (int_of_string_opt (String.trim p), int_of_string_opt (String.trim q))
      with
      | Some p, Some q -> Some (p, q)
      | _ -> None)
  | _ -> None

let parse_update line =
  String.split_on_char ',' line
  |> List.filter_map (fun page -> int_of_string_opt (String.trim page))

let parse input : rule list * update list =
  let lines = String.split_on_char '\n' input |> List.map String.trim in
  let rule_lines, update_lines = split_at_blank lines in
  let non_blank = List.filter (fun line -> line <> "") in
  ( rule_lines |> non_blank |> List.filter_map parse_rule,
    update_lines |> non_blank |> List.map parse_update )

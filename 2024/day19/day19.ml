(** * Day 19: Linen Layout - Advent of Code 2024 * * Problem description: * At
    an onsen (Japanese hot spring), we need to help arrange towels with colored
    stripes. * Each towel has a specific pattern of colored stripes (w=white,
    u=blue, b=black, r=red, g=green). * For example, a pattern "ggr" has two
    green stripes followed by a red stripe. * * Given a set of available towel
    patterns and desired designs: * - Part 1: Count how many designs can be
    created using available patterns * - Part 2: Calculate the total number of
    different ways to create all possible designs * * This problem is
    essentially asking: * - Part 1: Can we completely cover the "design string"
    with a combination of "pattern strings"? * - Part 2: In how many different
    ways can we cover the "design string" with "pattern strings"? *)

open Str

(** Determines how many designs from the given list can be created using
    available patterns.

    @param patterns List of available towel patterns
    @param designs List of desired designs to check
    @return Number of designs that can be created *)
let part1 (patterns, designs) =
  (* Convert patterns to array for faster lookup *)
  let patterns_array = Array.of_list patterns in
  let pattern_lens = Array.map String.length patterns_array in

  (* Create first-character index for patterns *)
  let first_char_index = Hashtbl.create 5 in
  Array.iteri
    (fun i pattern ->
      if String.length pattern > 0 then
        let first_char = pattern.[0] in
        let current =
          try Hashtbl.find first_char_index first_char with Not_found -> []
        in
        Hashtbl.replace first_char_index first_char (i :: current))
    patterns_array;

  (* Process each design to check if it can be created *)
  let process_design design =
    let design_len = String.length design in

    (* Array-based memoization: None=unknown, Some true=constructible, Some false=not constructible *)
    let memo = Array.make (design_len + 1) None in

    (* Fast pattern matching without String.sub *)
    let pattern_matches pattern pattern_len pos =
      if pos + pattern_len > design_len then false
      else
        let rec check i =
          if i >= pattern_len then true
          else if design.[pos + i] <> pattern.[i] then false
          else check (i + 1)
        in
        check 0
    in

    (* Recursive function to check if design can be constructed *)
    let rec can_construct pos =
      if pos = design_len then true
      else
        match memo.(pos) with
        | Some result -> result
        | None ->
            (* Get patterns that start with the current character for faster filtering *)
            let relevant_patterns =
              if pos < design_len then
                try Hashtbl.find first_char_index design.[pos]
                with Not_found -> []
              else []
            in

            (* Try each relevant pattern *)
            let result =
              List.exists
                (fun i ->
                  let pattern = patterns_array.(i) in
                  let pattern_len = pattern_lens.(i) in
                  pattern_matches pattern pattern_len pos
                  && can_construct (pos + pattern_len))
                relevant_patterns
            in

            memo.(pos) <- Some result;
            result
    in

    can_construct 0
  in

  (* Count how many designs can be constructed *)
  List.fold_left
    (fun count design -> if process_design design then count + 1 else count)
    0 designs

(** Calculates the total number of ways to create each design using available
    patterns.

    @param patterns List of available towel patterns
    @param designs List of desired designs to count ways for
    @return Total number of ways to create all designs (potentially very large)
*)
let part2 (patterns, designs) =
  (* Convert patterns to array for faster lookup *)
  let patterns_array = Array.of_list patterns in
  let pattern_lens = Array.map String.length patterns_array in

  (* Create first-character index for patterns *)
  let first_char_index = Hashtbl.create 5 in
  Array.iteri
    (fun i pattern ->
      if String.length pattern > 0 then
        let first_char = pattern.[0] in
        let current =
          try Hashtbl.find first_char_index first_char with Not_found -> []
        in
        Hashtbl.replace first_char_index first_char (i :: current))
    patterns_array;

  (* Process each design to count ways it can be created *)
  let process_design design =
    let design_len = String.length design in

    (* Array-based memoization for Int64 counts *)
    let memo = Array.make (design_len + 1) None in

    (* Fast pattern matching without String.sub *)
    let pattern_matches pattern pattern_len pos =
      if pos + pattern_len > design_len then false
      else
        let rec check i =
          if i >= pattern_len then true
          else if design.[pos + i] <> pattern.[i] then false
          else check (i + 1)
        in
        check 0
    in

    (* Recursive function to count ways *)
    let rec count_ways pos =
      if pos = design_len then 1L
      else
        match memo.(pos) with
        | Some ways -> ways
        | None ->
            (* Get patterns that start with the current character *)
            let relevant_patterns =
              if pos < design_len then
                try Hashtbl.find first_char_index design.[pos]
                with Not_found -> []
              else []
            in

            (* Calculate ways using each relevant pattern *)
            let ways =
              List.fold_left
                (fun acc i ->
                  let pattern = patterns_array.(i) in
                  let pattern_len = pattern_lens.(i) in
                  if pattern_matches pattern pattern_len pos then
                    Int64.add acc (count_ways (pos + pattern_len))
                  else acc)
                0L relevant_patterns
            in

            memo.(pos) <- Some ways;
            ways
    in

    count_ways 0
  in

  (* Sum up the ways for all designs *)
  List.fold_left
    (fun acc design -> Int64.add acc (process_design design))
    0L designs

(** Parse input into patterns and designs.

    - The input format can be one of:
    - 1. A line of comma-separated patterns, followed by a blank line, followed
      by lines of designs.
    - 2. A line of comma-separated patterns, followed by lines of designs
      without a blank line.

    @param input Raw input string
    @return Tuple of (patterns list, designs list) *)
let parse input =
  (* Try to split on double newline to get patterns and designs sections *)
  let parts = split (regexp "\n\n") input in

  if List.length parts >= 2 then
    (* Format 1: patterns line, blank line, designs *)
    let patterns =
      split (regexp ", ") (List.nth parts 0)
      |> List.filter (fun s -> String.trim s <> "")
    in

    let designs =
      split (regexp "\n") (List.nth parts 1)
      |> List.map String.trim
      |> List.filter (fun s -> s <> "")
    in

    (patterns, designs)
  else
    (* Format 2: patterns line followed directly by designs *)
    let lines =
      split (regexp "\n") input
      |> List.map String.trim
      |> List.filter (fun s -> s <> "")
    in

    match lines with
    | [] -> ([], []) (* Handle empty input *)
    | first_line :: rest ->
        let patterns =
          split (regexp ", ") first_line
          |> List.filter (fun s -> String.trim s <> "")
        in

        (patterns, rest)

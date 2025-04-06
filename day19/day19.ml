(** 
 * Day 19: Linen Layout - Advent of Code 2024
 *
 * Problem description:
 * At an onsen (Japanese hot spring), we need to help arrange towels with colored stripes.
 * Each towel has a specific pattern of colored stripes (w=white, u=blue, b=black, r=red, g=green).
 * For example, a pattern "ggr" has two green stripes followed by a red stripe.
 * 
 * Given a set of available towel patterns and desired designs:
 * - Part 1: Count how many designs can be created using available patterns
 * - Part 2: Calculate the total number of different ways to create all possible designs
 *
 * This problem is essentially asking:
 * - Part 1: Can we completely cover the "design string" with a combination of "pattern strings"?
 * - Part 2: In how many different ways can we cover the "design string" with "pattern strings"?
 *)

open Str

(** 
 * Determines how many designs from the given list can be created using available patterns.
 *
 * @param patterns List of available towel patterns
 * @param designs List of desired designs to check
 * @return Number of designs that can be created
 *)
let part1 (patterns, designs) =
  (* Convert patterns to array for faster lookup *)
  let patterns_array = Array.of_list patterns in

  (* 
   * Process each design to check if it can be created with available patterns.
   * Uses dynamic programming with memoization to avoid recalculating results.
   *)
  let process_design design =
    (* Memoization table: position -> can be constructed (true/false) *)
    let memo = Hashtbl.create 100 in

    (* 
     * Recursive function to check if a design can be constructed
     * starting from the given position.
     *)
    let rec can_construct pos =
      (* Base case: if we've reached the end, we've successfully constructed the design *)
      if pos = String.length design then
        true
      (* If we've already computed this position, return the cached result *)
      else if Hashtbl.mem memo pos then
        Hashtbl.find memo pos
      else
        (* Try each pattern to see if any work from the current position *)
        let rec try_patterns i =
          if i >= Array.length patterns_array then
            (* We've tried all patterns and none worked *)
            false
          else
            let pattern = patterns_array.(i) in
            (* Check if this pattern can be placed at current position and
               continue constructing the rest of the design *)
            if pos + String.length pattern <= String.length design && 
               String.sub design pos (String.length pattern) = pattern &&
               can_construct (pos + String.length pattern) then
              true
            else
              (* Try the next pattern *)
              try_patterns (i + 1)
        in
        
        (* Start trying patterns from the first one *)
        let result = try_patterns 0 in
        (* Cache the result for this position *)
        Hashtbl.add memo pos result;
        result
    in
    
    (* Start processing from the beginning of the design *)
    can_construct 0
  in
  
  (* Count how many designs can be constructed *)
  List.length (List.filter process_design designs)


(**
 * Calculates the total number of ways to create each design using available patterns.
 *
 * - @param patterns List of available towel patterns
 * - @param designs List of desired designs to count ways for
 * - @return Total number of ways to create all designs (potentially very large)
 *)
let part2 (patterns, designs) =
  (* Convert patterns to array for faster lookup *)
  let patterns_array = Array.of_list patterns in
  
  (* 
   * Process each design to count ways it can be created.
   * Uses dynamic programming with memoization for efficiency.
   *)
  let process_design design =
    (* Memoization table: position -> number of ways to construct (Int64 due to potentially large counts) *)
    let memo = Hashtbl.create 100 in
    
    (*
     * Recursive function to count ways to construct design starting from position 'pos'.
     * Returns the number of ways as an Int64 value (since counts can be very large).
     *)
    let rec count_ways pos =
      (* Base case: if we've reached the end, we've found one valid way *)
      if pos = String.length design then
        1L
      (* If we've already computed this position, return the cached result *)
      else if Hashtbl.mem memo pos then
        Hashtbl.find memo pos
      else
        (* Count ways for each pattern *)
        let rec try_pattern i acc =
          if i >= Array.length patterns_array then
            acc
          else
            let pattern = patterns_array.(i) in
            let new_acc = 
              (* If pattern matches at current position, add ways to construct the rest *)
              if pos + String.length pattern <= String.length design &&
                 String.sub design pos (String.length pattern) = pattern then
                Int64.add acc (count_ways (pos + String.length pattern))
              else
                acc
            in
            try_pattern (i + 1) new_acc
        in
        
        (* Accumulate ways starting with 0L *)
        let ways = try_pattern 0 0L in
        (* Cache the result for this position *)
        Hashtbl.add memo pos ways;
        ways
    in
    
    (* Start counting from the beginning of the design *)
    count_ways 0
  in
  
  (* Sum up the ways for all designs *)
  List.fold_left 
    (fun acc design -> Int64.add acc (process_design design)) 
    0L 
    designs

(**
 * Parse input into patterns and designs.
 *
 * The input format can be one of:
 * 1. A line of comma-separated patterns, followed by a blank line,
 *    followed by lines of designs.
 * 2. A line of comma-separated patterns, followed by lines of designs 
 *    without a blank line.
 *
 * @param input Raw input string
 * @return Tuple of (patterns list, designs list)
 *)
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
    | [] -> ([], [])  (* Handle empty input *)
    | first_line :: rest ->
        let patterns = 
          split (regexp ", ") first_line
          |> List.filter (fun s -> String.trim s <> "")
        in
        
        (patterns, rest)


(** Main entry point for solution *)
let () =
  try
    (* Read input from stdin *)
    let input = In_channel.input_all In_channel.stdin |> String.trim in
    Printf.printf "Input length: %d\n%!" (String.length input);
    
    (* Parse input into patterns and designs *)
    let patterns, designs = parse input in
    Printf.printf "Parsed %d patterns and %d designs\n%!" 
      (List.length patterns) 
      (List.length designs);
    
    (* Time the execution *)
    let start_time = Unix.gettimeofday () in
    
    (* Solve and print Part 1 *)
    part1 (patterns, designs) 
      |> string_of_int 
      |> Printf.printf "Part 1: %s\n%!";
    
    (* Solve and print Part 2 *)
    part2 (patterns, designs) 
      |> Int64.to_int  (* Note: This could overflow for very large outputs *)
      |> string_of_int 
      |> Printf.printf "Part 2: %s\n%!";
    
    (* Print elapsed time *)
    Unix.gettimeofday () -. start_time
      |> Printf.printf "Elapsed time: %.4f seconds\n"
    
  with
  | Failure msg -> Printf.printf "Error: %s\n" msg
  | e -> Printf.printf "Unexpected error: %s\n" (Printexc.to_string e)
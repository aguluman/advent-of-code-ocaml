(** Day 25: Code Chronicle - Virtual Five-Pin Tumbler Lock Analysis

    This module solves the Advent of Code Day 25 challenge about analyzing
    virtual five-pin tumbler locks and keys. The Historians are trying to access
    a locked office and need to determine which keys fit which locks.

    {2 Challenge Parts}
    - {b Part 1:} Count all unique lock/key pairs that fit together without
      overlapping in any column

    {2 Problem Details}

    {3 Input Format}
    - Schematics of locks and keys represented as patterns of # and . characters
    - Locks: Schematics with top row filled (#) and bottom row empty (.)
    - Keys: Schematics with top row empty and bottom row filled

    {3 Rules}
    - Compatibility: A lock and key fit if their combined height in each column
      doesn't exceed the available space
    - Output (Part 1): Number of unique lock/key pairs that fit together without
      overlapping

    The challenge involves converting the visual schematics into numerical
    height representations and then testing each lock against each key to
    determine compatibility.

    @see <https://adventofcode.com/2023/day/25> Advent of Code 2023, Day 25 *)

(** Solves part 1 of the challenge by calculating the number of unique lock/key
    pairs that fit together.

    @param schematics
      A list of character arrays representing lock and key schematics
    @return
      The count of unique lock/key pairs that fit together without overlapping

    The function works by:
    + Identifying locks (schematics with top row filled) and keys (schematics
      with top row empty)
    + Converting both to representations of their heights
    + Testing each lock with each key to check for compatible fits
    + Counting the number of valid combinations *)
let part1 (schematics : char array array list) =
  (* Get height and width from the first schematic *)
  let h = Array.length (List.hd schematics) in
  let w = Array.length (List.hd schematics).(0) in

  (* Convert locks and keys into arrays for faster indexing *)
  let locks =
    List.filter (fun s -> Array.for_all (( = ) '#') s.(0)) schematics
    |> List.map (fun s ->
        Array.init w (fun j ->
            Array.fold_left
              (fun acc i -> acc + if s.(i).(j) = '.' then 0 else 1)
              0 (Array.init h Fun.id)))
  in

  let keys =
    List.filter (fun s -> Array.for_all (( = ) '.') s.(0)) schematics
    |> List.map (fun s ->
        Array.init w (fun j ->
            Array.fold_left
              (fun acc i -> acc + if s.(i).(j) = '.' then 0 else 1)
              0 (Array.init h Fun.id)))
  in

  (* Count valid lock-key pairs using arrays instead of lists *)
  Array.of_list locks
  |> Array.fold_left
       (fun acc lock ->
         acc
         + Array.fold_left
             (fun inner_acc key ->
               if
                 Array.for_all
                   (fun i -> lock.(i) + key.(i) <= h)
                   (Array.init w Fun.id)
               then inner_acc + 1
               else inner_acc)
             0 (Array.of_list keys))
       0

(** Parses the input string into a structured representation of lock and key
    schematics.

    @param input
      The raw input string containing multiple schematics separated by blank
      lines
    @return A list of character arrays representing individual schematics

    The function splits the input at blank lines to separate individual
    schematics, then converts each line to a character array. *)
let parse input =
  (* Normalize line endings and split into sections *)
  let sections = Str.split (Str.regexp "\n\n\\|\r\n\r\n") input in

  (* Process each section *)
  List.map
    (fun section ->
      let lines = Str.split (Str.regexp "\n\\|\r\n") section in
      List.map
        (fun line -> String.trim line |> String.to_seq |> Array.of_seq)
        lines)
    sections

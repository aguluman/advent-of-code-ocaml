(** Day 09: Disk Fragmenter

    Solves the disk defragmentation problem by compacting files and calculating checksums.
    
    {2 Problem Summary:}
    - {b Part 1:} Compact files by moving individual blocks from right to left to fill gaps
    - {b Part 2:} Move entire contiguous files to the leftmost available free space
    
    See details at: {{:https://adventofcode.com/2024/day/09} Advent of Code 2024, Day 09}
*)

(** Type representing a disk block - either Free or Occupied with a file_id *)
type block =
  | Free
  | Occupied of int (* file_id *)

(** Parse input string into disk block array
    @param input String of alternating file sizes and free space lengths
    @return Array of disk blocks *)
let parse input =
  input
  |> String.to_seq
  |> Array.of_seq
  |> fun digits ->
      let disk_blocks = ref [] in
      digits |> Array.iteri (fun integer character ->
        let block_length = int_of_char character - int_of_char '0' in
        let block_type = 
          if integer mod 2 = 0 
          then Occupied(integer/2) 
          else Free in
        for _ = 1 to block_length do
          disk_blocks := block_type :: !disk_blocks
        done
      );
      !disk_blocks
  |> List.rev
  |> Array.of_list

(** Calculate filesystem checksum by multiplying block positions with file IDs
    @param disk Array of disk blocks
    @return Int64 checksum value *)
let checksum disk =
  disk
  |> Array.mapi (fun i block_type ->
       match block_type with
       | Free -> 0L
       | Occupied file_id -> 
           (Int64.of_int i) |> Int64.mul (Int64.of_int file_id))
  |> Array.fold_left Int64.add 0L

(** [part1 input] solves part 1 of the challenge - Compact files from right to left, filling gaps
    
    @param input Raw input string from the puzzle
    @return Solution for part 1 as Int64
*)
let part1 input =
  let disk = parse input in
  disk
  |> Array.copy
  |> fun disk_copy -> 
      let rec compact left_pos right_pos disk =
        if left_pos >= right_pos then disk
        else
          match (disk.(left_pos), disk.(right_pos)) with
          | (Free, Free) -> 
              disk |> compact left_pos (right_pos - 1)
          | (Free, Occupied file_id) ->
              disk.(left_pos) <- Occupied file_id;
              disk.(right_pos) <- Free;
              disk |> compact (left_pos + 1) (right_pos - 1)
          | (Occupied _, Free) -> 
              disk |> compact (left_pos + 1) (right_pos - 1)
          | (Occupied _, Occupied _) -> 
              disk |> compact (left_pos + 1) right_pos
      in
      compact 0 (Array.length disk_copy - 1) disk_copy
  |> checksum

(** [part2 input] solves part 2 of the challenge - Move contiguous file blocks to first available free space
    
    @param input Raw input string from the puzzle
    @return Solution for part 2 as Int64
*)
let part2 input =
  let disk = parse input in
  let rec compact right_pos disk =
    if right_pos <= 0 then disk
    else match disk.(right_pos) with
      | Free -> compact (right_pos - 1) disk
      | Occupied file_id ->
          (* Find length of contiguous occupied blocks *)
          let length = 
            let count = ref 0 in
            while !count <= right_pos && disk.(right_pos - !count) = Occupied file_id do
              incr count
            done;
            !count
          in

          let new_r = right_pos - length in

          (* (Find Free Space) Find first position that can fit length Free blocks *)
          let rec find_free_pos left_pos =
            if left_pos > right_pos then None
            else 
              let is_free = ref true in
              for i = 0 to length - 1 do
                if left_pos + i >= Array.length disk || disk.(left_pos + i) <> Free then
                  is_free := false
              done;
              if !is_free then Some left_pos
              else find_free_pos (left_pos + 1)
          in

          (* Block Movement*)
          match find_free_pos 0 with
          | None -> compact new_r disk
          | Some left_pos ->
              (* Swap blocks to new position *)
              for d = 0 to length - 1 do
                disk.(left_pos + d) <- Occupied file_id;
                disk.(new_r + 1 + d) <- Free
              done;
              compact new_r disk
  in
    let disk_copy = Array.copy disk 
  in
    checksum (compact (Array.length disk_copy - 1) disk_copy)

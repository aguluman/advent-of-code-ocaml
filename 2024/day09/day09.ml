(** Day 09: Disk Fragmenter

    Solves the disk defragmentation problem by compacting files and calculating checksums.
    
    {2 Problem Summary:}
    - {b Part 1:} Compact files by moving individual blocks from right to left to fill gaps
    - {b Part 2:} Move entire contiguous files to the leftmost available free space
    
    See details at: {{:https://adventofcode.com/2024/day/09} Advent of Code 2024, Day 09}
*)

(** Type representing a file with position and size *)
type file = {
  position: int;
  size: int;
}

(** Type representing a free space with position and size *) 
type space = {
  position: int;
  size: int;
}

(** Parse input string into array of integers representing file and space sizes
    @param input String of alternating file sizes and free space lengths
    @return Array of integers *)
let parse input =
  input
  |> String.to_seq
  |> Seq.map (fun c -> int_of_char c - int_of_char '0')
  |> Array.of_seq

(** Create initial file and space arrays from disk map
    @param disk_map Array of alternating file and space sizes
    @return Tuple of (files array, spaces array) *)
let create_files_and_spaces disk_map =
  let files = ref [] in
  let spaces = ref [] in
  let position = ref 0 in
  
  Array.iteri (fun i size ->
    if i mod 2 = 0 then
      (* File *)
      files := (i / 2, { position = !position; size = size }) :: !files
    else
      (* Space *)
      if size > 0 then
        spaces := { position = !position; size = size } :: !spaces;
    position := !position + size
  ) disk_map;
  
  (Array.of_list (List.rev !files), Array.of_list (List.rev !spaces))

(** Calculate checksum for files represented as position/size pairs
    @param files Array of (file_id, file) pairs
    @return Int64 checksum value *)
let checksum_files files =
  files
  |> Array.fold_left (fun acc (file_id, file) ->
      let file_sum = ref 0L in
      for pos = file.position to file.position + file.size - 1 do
        file_sum := Int64.add !file_sum (Int64.mul (Int64.of_int pos) (Int64.of_int file_id))
      done;
      Int64.add acc !file_sum) 0L

(** [part1 input] solves part 1 of the challenge - Compact files from right to left, filling gaps
    
    @param input Raw input string from the puzzle
    @return Solution for part 1 as Int64
*)
let part1 input =
  let disk_map = parse input in
  let (files, spaces) = create_files_and_spaces disk_map in
  
  let spaces_list = ref (Array.to_list spaces) in
  
  (* Process files from right to left *)
  let rec compact_files remaining_files result_files =
    match remaining_files with
    | [] -> result_files
    | (file_id, file) :: rest_files ->
        (* Find space that can fit at least one block and move parts *)
        let rec move_file_parts file_remaining current_spaces acc_spaces moved_parts =
          if file_remaining.size = 0 then
            (moved_parts, List.rev_append acc_spaces current_spaces)
          else
            match current_spaces with
            | [] -> 
                (* No more spaces, add remaining file at original position *)
                let final_parts = 
                  if file_remaining.size > 0 then
                    (file_id, file_remaining) :: moved_parts
                  else moved_parts
                in
                (final_parts, List.rev acc_spaces)
            | space :: rest_spaces ->
                if space.position >= file_remaining.position then
                  (* Space is after file, don't use it, add remaining file *)
                  let final_parts = 
                    if file_remaining.size > 0 then
                      (file_id, file_remaining) :: moved_parts
                    else moved_parts
                  in
                  (final_parts, List.rev_append acc_spaces current_spaces)
                else if space.size > 0 then
                  (* Use this space *)
                  let blocks_to_move = min file_remaining.size space.size in
                  let new_file_part = { position = space.position; size = blocks_to_move } in
                  let remaining_file = 
                    { file_remaining with size = file_remaining.size - blocks_to_move }
                  in
                  let remaining_space = 
                    if space.size > blocks_to_move then
                      Some { position = space.position + blocks_to_move; 
                             size = space.size - blocks_to_move }
                    else None
                  in
                  let updated_spaces = 
                    match remaining_space with
                    | None -> rest_spaces
                    | Some rs -> rs :: rest_spaces
                  in
                  (* Continue with remaining file and updated spaces *)
                  move_file_parts remaining_file updated_spaces [] ((file_id, new_file_part) :: moved_parts)
                else
                  move_file_parts file_remaining rest_spaces (space :: acc_spaces) moved_parts
        in
        
        let (file_parts, updated_spaces) = move_file_parts file !spaces_list [] [] in
        spaces_list := updated_spaces;
        
        compact_files rest_files (List.rev_append file_parts result_files)
  in
  
  let compacted_files = compact_files (List.rev (Array.to_list files)) [] in
  checksum_files (Array.of_list compacted_files)

(** [part2 input] solves part 2 of the challenge - Move entire contiguous files to leftmost available space
    
    @param input Raw input string from the puzzle
    @return Solution for part 2 as Int64
*)
let part2 input =
  let disk_map = parse input in
  let (files, spaces) = create_files_and_spaces disk_map in
  
  (* Sort files by file_id in descending order (highest first) *)
  let sorted_files = 
    files 
    |> Array.to_list 
    |> List.sort (fun (id1, _) (id2, _) -> compare id2 id1)
  in
  
  let spaces_list = ref (Array.to_list spaces) in
  
  (* Process each file, trying to move it to the leftmost available space *)
  let rec process_files remaining_files result_files =
    match remaining_files with
    | [] -> result_files
    | (file_id, file) :: rest_files ->
        (* Find the leftmost space that can fit the entire file *)
        let rec find_space current_spaces acc_spaces =
          match current_spaces with
          | [] -> None
          | space :: rest_spaces ->
              if space.position < file.position && space.size >= file.size then
                Some (space, List.rev_append acc_spaces rest_spaces)
              else
                find_space rest_spaces (space :: acc_spaces)
        in
        
        match find_space !spaces_list [] with
        | None ->
            (* No suitable space found, keep file in original position *)
            process_files rest_files ((file_id, file) :: result_files)
        | Some (space, remaining_spaces) ->
            (* Move file to the space *)
            let moved_file = { position = space.position; size = file.size } in
            let remaining_space_size = space.size - file.size in
            
            (* Update spaces list *)
            let updated_spaces = 
              if remaining_space_size > 0 then
                let remaining_space = 
                  { position = space.position + file.size; size = remaining_space_size }
                in
                (* Insert remaining space back in position order *)
                let rec insert_space sp spaces_acc =
                  match spaces_acc with
                  | [] -> [sp]
                  | s :: rest when s.position > sp.position -> sp :: spaces_acc
                  | s :: rest -> s :: insert_space sp rest
                in
                insert_space remaining_space remaining_spaces
              else
                remaining_spaces
            in
            
            spaces_list := updated_spaces;
            process_files rest_files ((file_id, moved_file) :: result_files)
  in
  
  let final_files = process_files sorted_files [] in
  checksum_files (Array.of_list final_files)

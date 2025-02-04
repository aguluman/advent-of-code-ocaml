module IntSet = Set.Make(Int)

type block =
  | Free
  | Occupied of int (* file_id *)

let checksum disk =
  Array.mapi (fun i block_type ->
    match block_type with
    | Free -> 0L
    | Occupied file_id -> Int64.mul (Int64.of_int i) (Int64.of_int file_id)) disk
  |> Array.fold_left Int64.add 0L


let part1 disk =
  let rec compact left_pos right_pos disk =
    if left_pos >= right_pos then disk
    else
      match (disk.(left_pos), disk.(right_pos)) with
      | (Free, Free) -> compact left_pos (right_pos - 1) disk
      | (Free, Occupied file_id) ->
          (* swap disk.(left_pos), disk.(right_pos) *)
          disk.(left_pos) <- Occupied file_id;
          disk.(right_pos) <- Free;
          compact (left_pos + 1) (right_pos - 1) disk
      | (Occupied _, Free) -> compact (left_pos + 1) (right_pos - 1) disk
      | (Occupied _, Occupied _) -> compact (left_pos + 1) right_pos disk
  in
  let disk_copy = Array.copy disk in
  checksum (compact 0 (Array.length disk_copy - 1) disk_copy)



let part2 disk =
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

          (* Find first position that can fit length Free blocks *)
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

          match find_free_pos 0 with
          | None -> compact new_r disk
          | Some left_pos ->
              (* Swap blocks *)
              for d = 0 to length - 1 do
                disk.(left_pos + d) <- Occupied file_id;
                disk.(new_r + 1 + d) <- Free
              done;
              compact new_r disk
  in
  let disk_copy = Array.copy disk in
  checksum (compact (Array.length disk_copy - 1) disk_copy)



let parse input =
  let digits = input |> String.to_seq |> Array.of_seq in
  let disk_blocks = ref [] in
  Array.iteri (fun i c ->
    let block_length = int_of_char c - int_of_char '0' in
    let block_type = if i mod 2 = 0 then Occupied(i/2) else Free in
    for _ = 1 to block_length do
      disk_blocks := block_type :: !disk_blocks
    done
  ) digits;
  Array.of_list (List.rev !disk_blocks)



let () =
  let input = input_line stdin in
  let disk = parse input in

  let timer_start = Unix.gettimeofday () in
  
  Printf.printf "Part 1: %Ld\block_length" (part1 disk);
  disk |> part2 |> Printf.printf "Part 2: %Ld\block_length";

  let timer_end = Unix.gettimeofday () in

  let elapsed = timer_end -. timer_start in

  Printf.printf "Elapsed time: %.4f seconds\block_length" (elapsed)
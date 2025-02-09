module IntMap = Map.Make(Int64)




let blink stone = 
  if stone = 0L then
    [ 1L ]
  else 
    let stone_string = stone |> Int64.to_string in
    if String.length stone_string mod 2 = 0 then
      [Int64.of_string 
        (String.sub stone_string 0 (String.length stone_string / 2)); 
       Int64.of_string 
        (String.sub stone_string (String.length stone_string / 2) 
         (String.length stone_string / 2))]
    else 
      [Int64.mul stone 2024L]





let part1 stones = 
  let initial_stones = stones |> List.of_seq in
  List.init 25 (fun _ -> ())
  |> List.fold_left 
    (fun acc _ -> acc |> List.concat_map blink) 
    initial_stones
  |> List.length



let part2 stones =
  (* Initialize map with each stone having count of 1 *)
  let initial_map = 
    stones 
    |> Seq.map (fun x -> (x, 1L))
    |> List.of_seq
    |> List.to_seq  (* Convert back to sequence for IntMap.of_seq *)
    |> IntMap.of_seq
  in
  (* Do 75 blinks *)
  List.init 75 (fun _ -> ())
  |> List.fold_left (fun acc_map _ ->
      (* For each stone and its count in current map *)
      IntMap.fold (fun stone count new_map ->
        (* Apply blink to current stone *)
        blink stone
        |> List.fold_left (fun map new_stone ->
            (* Add count to new stone's existing count (if any) *)
            let current = 
              match IntMap.find_opt new_stone map with
              | Some count -> count
              | None -> 0L
            in
            IntMap.add new_stone (Int64.add current count) map
          ) new_map
      ) acc_map IntMap.empty
    ) initial_map
  |> (fun m -> IntMap.fold (fun _ count acc -> Int64.add acc count) m 0L)



let parse input = 
  input 
  |> String.split_on_char ' '
  |> List.map Int64.of_string
  |> List.to_seq




let () =
  let input = read_line () in
  let stones = parse input in
  
  let timer_start = Unix.gettimeofday () in 

  stones |> part1 |> Printf.printf "Part 1: %d\n";
  stones |> part2 |> Printf.printf "Part 2: %Ld\n";

  Unix.gettimeofday () -. timer_start
  |> Printf.printf "Elapsed time: %.4f seconds\n" 
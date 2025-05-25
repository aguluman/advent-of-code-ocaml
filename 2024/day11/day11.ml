(** Module for handling large integer maps in the stone transformation puzzle *)
module IntMap = Map.Make(Int64)



(** [blink current_stone] applies transformation rules to a single stone:
    - If stone is 0, it becomes 1
    - If stone has even number of digits, splits into two stones
    - If stone has odd number of digits or otherwise, multiplies it by 2024
    @param current_stone The stone to transform
    @return List of new stones after transformation
*)
let blink current_stone = 
  if current_stone = 0L then
    [ 1L ]
  else 
    let digit_string = current_stone |> Int64.to_string in
    let digit_count = String.length digit_string in
    if digit_count mod 2 = 0 then
      let half_length = digit_count / 2 in
      [Int64.of_string 
        (String.sub digit_string 0 half_length); 
       Int64.of_string 
        (String.sub digit_string half_length half_length)]
    else 
      [Int64.mul current_stone 2024L]




(** [part1 initial_stones] simulates 25 blinks and counts resulting stones
    @param initial_stones Sequence of starting stones
    @return Total number of stones after 25 transformations
*)
let part1 initial_stones = 
  let starting_stones = initial_stones |> List.of_seq in
  List.init 25 (fun _ -> ())
  |> List.fold_left 
    (fun current_generation _ -> 
       current_generation |> List.concat_map blink) 
    starting_stones
  |> List.length




(** [part2 initial_stones] simulates 75 blinks efficiently using count tracking
    @param initial_stones Sequence of starting stones
    @return Total number of stones after 75 transformations
*)
let part2 initial_stones =
  (* Initialize map with each stone having count of 1 *)
  let starting_map = 
    initial_stones 
    |> Seq.map (fun stone -> (stone, 1L))
    |> List.of_seq
    |> List.to_seq  (* Convert back to sequence for IntMap.of_seq *)
    |> IntMap.of_seq
  in
  (* Simulate 75 blinks *)
  List.init 75 (fun _ -> ())
  |> List.fold_left (fun generation_map _ ->
      (* Transform each stone and update counts *)
      IntMap.fold (fun current_stone frequency new_generation_map ->
        (* Get new stones from current stone *)
        blink current_stone
        |> List.fold_left (fun accumulated_map new_stone ->
            (* Update count for new stone *)
            let existing_frequency = 
              match IntMap.find_opt new_stone accumulated_map with
              | Some count -> count
              | None -> 0L
            in
            IntMap.add new_stone (Int64.add existing_frequency frequency) accumulated_map
          ) new_generation_map
      ) generation_map IntMap.empty
    ) starting_map
  |> (fun final_map -> 
      IntMap.fold (fun _ stone_count total -> 
        Int64.add total stone_count) final_map 0L)




(** [parse input_string] converts space-separated numbers to stone sequence
    @param input_string Space-separated string of numbers
    @return Sequence of int64 stones
*)
let parse input_string = 
  input_string 
  |> String.split_on_char ' '
  |> List.map Int64.of_string
  |> List.to_seq





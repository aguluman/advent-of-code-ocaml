(** Module for handling coordinate pairs in a grid *)
module CoordSet = Set.Make(struct
  (** [(row, col)] coordinate pair *)
  type t = int * int    
  let compare = compare
end)



(** Find all connected positions with same plant type using DFS
    @param garden_map The 2D array of plant types
    @param row Current row position
    @param col Current column position
    @param visited Set of already visited positions
    @return Set of all positions in the same region *)
let rec region garden_map (row, col) visited =
  let direction_vectors = [(-1, 0); (0, -1); (1, 0); (0, 1)] in
  let visited_with_current = CoordSet.add (row, col) visited in
  
  List.fold_left (fun visited_positions (delta_row, delta_col) ->
    let next_row = row + delta_row in
    let next_col = col + delta_col in
    if next_row >= 0 && next_row < Array.length garden_map &&
       next_col >= 0 && next_col < Array.length garden_map.(row) &&
       garden_map.(row).(col) = garden_map.(next_row).(next_col) &&
       not (CoordSet.mem (next_row, next_col) visited_positions)
    then
      region garden_map (next_row, next_col) visited_positions
    else
      visited_positions
  ) visited_with_current direction_vectors




(** Generate cartesian product of two lists
    @param list1 First list
    @param list2 Second list
    @return List of all possible pairs *)
let cartesian_product list1 list2 =
  list1 
  |> List.map (fun elem1 -> 
       list2 
       |> List.map (fun elem2 -> (elem1, elem2)))
  |> List.concat

(** Calculate total price for all regions in garden
    @param garden_map The 2D array of plant types
    @param price_calculator Function to calculate price for a region
    @return Total price for all regions *)
let solve garden_map price_calculator =
  let grid_size = Array.length garden_map in
  garden_map |> Array.iter (fun row -> assert (Array.length row = grid_size));
  
  let grid_indices = List.init grid_size (fun index -> index) in
  let all_positions = cartesian_product grid_indices grid_indices in
  
  let process_position (total_price, visited_positions) (row, col) =
    if CoordSet.mem (row, col) visited_positions then
      (total_price, visited_positions)
    else
      let current_region = region garden_map (row, col) CoordSet.empty in
      (total_price + price_calculator current_region, 
       CoordSet.union visited_positions current_region)
  in
  
  let (final_price, _) = List.fold_left process_position (0, CoordSet.empty) all_positions in
  final_price




(** Calculate solution for part 1
    @param garden_map The 2D array of plant types
    @return Total price using area * perimeter calculation *)
let part1 garden_map =
  let calculate_region_price positions =
    let area = CoordSet.cardinal positions in
    let perimeter =
      CoordSet.fold (fun (row, col) perimeter_count ->
        List.fold_left (fun count (delta_row, delta_col) ->
          let next_row = row + delta_row in
          let next_col = col + delta_col in
          if next_row >= 0 && next_row < Array.length garden_map &&
             next_col >= 0 && next_col < Array.length garden_map.(next_row) &&
             garden_map.(row).(col) = garden_map.(next_row).(next_col)
          then 
            (* Don't count this side - it touches same plant type *)
            count
          else 
            (* Count this side - it's exposed *)
            count + 1
        ) perimeter_count [(-1, 0); (0, -1); (1, 0); (0, 1)] (* Check all four sides *)
      ) positions 0
    in
    area * perimeter
  in
  solve garden_map calculate_region_price




(** Calculate solution for part 2
    @param garden_map The 2D array of plant types
    @return Total price using area * number of corners calculation *)
let part2 garden_map =
  let calculate_region_price positions =
    let area = CoordSet.cardinal positions in
    
    (* Generate all intersection points from positions *)
    let get_intersections (row, col) =
      [(row, col); (row + 1, col); (row, col + 1); (row + 1, col + 1)]
    in
    
    let intersections = 
      CoordSet.fold (fun pos acc -> 
        List.fold_left (fun set point -> 
          CoordSet.add point set
        ) acc (get_intersections pos)
      ) positions CoordSet.empty
    in

    (* Count corners *)
    let count_corner (i, j) =
      let surrounding_points = [(i - 1, j - 1); (i - 1, j); (i, j - 1); (i, j)] in
      let surrounding = 
        List.filter (fun pos -> CoordSet.mem pos positions) surrounding_points
        |> List.sort compare
      in
      
      match List.length surrounding with
      | 3 -> 1  (* Three points = one corner *)
      | 1 -> 1  (* Single point = one corner *)
      | 2 -> 
          (* Check diagonal arrangement *)
          if surrounding = [(i - 1, j - 1); (i, j)] ||
             surrounding = [(i - 1, j); (i, j - 1)] 
          then 2  (* Diagonal arrangement = two corners *)
          else 0
      | _ -> 0
    in

    let corner_count = 
      CoordSet.fold (fun pos acc ->
        acc + count_corner pos
      ) intersections 0
    in
    
    area * corner_count
  in
  solve garden_map calculate_region_price


  
(** Parse input string into 2D array
    @param input Raw input string
    @return 2D array of characters representing garden *)
let parse input =
  input 
  |> String.split_on_char '\n'
  |> List.map String.trim
  |> List.map String.to_seq
  |> List.map Array.of_seq
  |> Array.of_list

  


(** Main entry point *)
let () =
  In_channel.input_all In_channel.stdin
  |> String.trim
  |> parse
  |> (fun garden_map ->
      let start_time = Unix.gettimeofday () in

      garden_map |> part1 |> Printf.printf "Part 1: %d\n";
      garden_map |> part2 |> Printf.printf "Part 2: %d\n";

      Unix.gettimeofday () -. start_time)
  |> Printf.printf "Elapsed time: %.4f seconds\n"
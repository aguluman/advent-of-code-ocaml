(** Module for representing a [Set] of garden plot coordinates. *)
module GardenPlotSet = Set.Make(struct
  (** A garden plot coordinate represented as [(row, col)] *)
  type t = int * int    
  let compare = compare
end)




(** Find all connected garden plots with the same plant type using Depth-First Search
    @param garden_plots The 2D array representing the garden layout
    @param current_row Starting row position
    @param current_col Starting column position
    @param discovered_plots Set of already discovered plot positions
    @return Set of all plot positions containing the same plant type *)
let rec find_connected_plots garden_plots (current_row, current_col) discovered_plots =
  let adjacent_directions = [(-1, 0); (0, -1); (1, 0); (0, 1)] (* Up, Left, Down, Right *)
in 
  let plots_with_current = GardenPlotSet.add (current_row, current_col) discovered_plots 
in
  
  List.fold_left (fun discovered (row_step, col_step) ->
    let next_row = current_row + row_step 
  in
    let next_col = current_col + col_step 
  in

    let is_valid_position = 
      next_row >= 0 && next_row < Array.length garden_plots &&
      next_col >= 0 && next_col < Array.length garden_plots.(current_row) 
    in

    let is_same_plant_type = 
      is_valid_position && 
      garden_plots.(current_row).(current_col) = garden_plots.(next_row).(next_col) 
    in

    let is_undiscovered = 
      not (GardenPlotSet.mem (next_row, next_col) discovered) 
    in

    if 
      is_valid_position && is_same_plant_type && is_undiscovered 
    then
      find_connected_plots garden_plots (next_row, next_col) discovered
    else
      discovered
  ) plots_with_current adjacent_directions




(** Generate cartesian product of two lists [(list1 * list2)]
    @param list1 First list of elements
    @param list2 Second list of elements
    @return List containing all possible pairs from both lists *)
let generate_combinations list1 list2 =
  list1 
  |> List.map (fun item1 -> 
       list2 
       |> List.map (fun item2 -> (item1, item2)))
  |> List.concat



(** Calculate total fencing cost for all plant regions in garden
    @param garden_plots The 2D array representing the garden layout
    @param calculate_region_cost Function to calculate cost for a specific region
    @return Total fencing cost for all regions *)
let calculate_total_cost garden_plots calculate_region_cost =
  let garden_size = Array.length garden_plots in
  garden_plots |> Array.iter (fun row -> assert (Array.length row = garden_size));
  
  let garden_coordinates = 
    List.init garden_size (fun index -> index) 
  in
  let all_plot_positions = 
    generate_combinations garden_coordinates garden_coordinates 
  in
  
  let process_plot (total_cost, processed_plots) (row, col) =
    if 
      GardenPlotSet.mem (row, col) processed_plots 
    then
      (total_cost, processed_plots)
    else
      let current_plant_region = find_connected_plots garden_plots (row, col) GardenPlotSet.empty
    in
      (total_cost + calculate_region_cost current_plant_region, 
       GardenPlotSet.union processed_plots current_plant_region)
  in
  
  let (final_cost, _) = 
    List.fold_left process_plot (0, GardenPlotSet.empty) all_plot_positions 
  in
  final_cost



(** Calculate solution for part 1
    @param garden_plots The 2D array of plant types
    @return Total price using area * perimeter calculation *)
let part1 garden_plots =
  let calculate_region_price positions =
    let area = GardenPlotSet.cardinal positions in
    let perimeter =
      GardenPlotSet.fold (fun (row, col) perimeter_count ->
        List.fold_left (fun count (delta_row, delta_col) ->
          let next_row = row + delta_row 
        in
          let next_col = col + delta_col 
        in
          if next_row >= 0 && next_row < Array.length garden_plots &&
             next_col >= 0 && next_col < Array.length garden_plots.(next_row) &&
             garden_plots.(row).(col) = garden_plots.(next_row).(next_col)
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
  calculate_total_cost garden_plots calculate_region_price




(** Calculate solution for part 2
    @param garden_plots The 2D array of plant types
    @return Total price using area * number of corners calculation *)
let part2 garden_plots =
  let calculate_region_price positions =
    let area = GardenPlotSet.cardinal positions in
    
    (* Generate all intersection points from positions *)
    let get_intersections (row, col) =
      [(row, col); (row + 1, col); (row, col + 1); (row + 1, col + 1)]
    in
    
    let intersections = 
      GardenPlotSet.fold (fun pos acc -> 
        List.fold_left (fun set point -> 
          GardenPlotSet.add point set
        ) acc (get_intersections pos)
      ) positions GardenPlotSet.empty
    in

    (* Count corners *)
    let count_corner (i, j) =
      let surrounding_points = 
        [(i - 1, j - 1); (i - 1, j); (i, j - 1); (i, j)] 
    in
      let surrounding = 
        List.filter (fun pos -> GardenPlotSet.mem pos positions) surrounding_points
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
      GardenPlotSet.fold (fun pos acc ->
        acc + count_corner pos
      ) intersections 0
    in
    
    area * corner_count
  in
  calculate_total_cost garden_plots calculate_region_price


  
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
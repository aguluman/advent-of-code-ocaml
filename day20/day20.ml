
let find_index_2d array value =
  let rows = Array.length array in
  let cols = Array.length array.(0) in
  
  (* Create a list of all (row,col) pairs *)
  let all_pairs = 
    let row_indices = List.init rows (fun i -> i) in
    let col_indices = List.init cols (fun i -> i) in
    
    List.flatten (List.map (fun row -> 
      List.map (fun column -> (row, column)) col_indices
    ) row_indices)
  in
  
  (* Find the first pair where the array value matches *)
  List.find (fun (row, column) -> array.(row).(column) = value) all_pairs


(* Create a map module for coordinate pairs *)
module IntPairMap = Map.Make(struct
  type t = int * int
  let compare = compare
end)

let bfs (si, sj) (maze : char array array) =
  (* Process a node and find valid neighbors *)
  let transition (i, j) dist =
    let process_direction acc (di, dj) =
      let nodes, dist = acc in
      let ni, nj = i + di, j + dj in
      
      if 0 <= ni && ni < Array.length maze &&
         0 <= nj && nj < Array.length maze.(ni) &&
         maze.(ni).(nj) <> '#' &&
         not (IntPairMap.mem (ni, nj) dist)
      then
        let new_dist = IntPairMap.add (ni, nj) (IntPairMap.find (i, j) dist + 1) dist in
        (Some (ni, nj) :: nodes, new_dist)
      else
        (None :: nodes, dist)
    in
    
    let new_nodes_opt, new_dist = 
      List.fold_left process_direction ([], dist) [(-1, 0); (0, -1); (1, 0); (0, 1)]
    in
    
    (* Filter out None values *)
    let new_nodes = List.filter_map (fun x -> x) new_nodes_opt in
    (new_nodes, new_dist)
  in
  
  (* Main BFS logic *)
  let rec bfs' nodes dist =
    if nodes = [] then
      dist
    else
      let new_nodes, final_dist =
        List.fold_left
          (fun (all_nodes, current_dist) node ->
            let new_nodes, new_dist = transition node current_dist in
            (new_nodes @ all_nodes, new_dist))
          ([], dist)
          nodes
      in
      
      bfs' new_nodes final_dist
  in
  
  (* Start BFS from initial position *)
  bfs' [(si, sj)] (IntPairMap.singleton (si, sj) 0)







let part2 (maze : char array array) =
  let si, sj = find_index_2d maze 'S' in
  
  let dist = bfs (si, sj) maze in
  let dist_list = IntPairMap.bindings dist in
  
  (* Create all pairs of positions - equivalent to List.allPairs in F# *)
  let all_pairs = 
    List.flatten 
      (List.map (fun ((i, j), d) -> 
        List.map (fun ((i', j'), d') -> 
          (((i, j), d), ((i', j'), d'))
        ) dist_list
      ) dist_list)
  in
  
  (* Filter and map pairs *)
  let differences = 
    List.filter_map (fun (((i, j), d), ((i', j'), d')) ->
      let e = abs (i - i') + abs (j - j') in
      if d' - d >= 0 && e <= 20 then Some(d' - d - e) else None
    ) all_pairs
  in
  
  (* Count occurrences of each difference - equivalent to List.countBy in F# *)
  let module IntMap = Map.Make(Int) in
  let counts = 
    List.fold_left (fun acc x ->
      let count = try IntMap.find x acc with Not_found -> 0 in
      IntMap.add x (count + 1) acc
    ) IntMap.empty differences
  in
  
  (* Convert map to list of (value, count) pairs and sort *)
  let count_list = IntMap.bindings counts in
  List.sort compare count_list



let parse input =
  input
  |> String.split_on_char '\n'
  |> List.filter (fun s -> String.trim s <> "")
  |> List.map (fun row -> Array.of_list (List.init (String.length row) (String.get row)))
  |> Array.of_list
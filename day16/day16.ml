(* Direction type *)
type direction =
  | E
  | N
  | W
  | S

(* Functions to replace the Direction members *)
let counter_clock_wise direction =
  match direction with
  | E -> N
  | N -> W
  | W -> S
  | S -> E

let clock_wise direction =
  match direction with
  | E -> S
  | S -> W
  | W -> N
  | N -> E

(* Node record type *)
type node = {
  row: int;
  column: int;
  direction: direction;
}

(* Functions to replace the Node members *)
let forward node maze =
  let (nrow, ncolumn) =
    match node.direction with
    | E -> (node.row, node.column + 1)
    | N -> (node.row - 1, node.column)
    | W -> (node.row, node.column - 1)
    | S -> (node.row + 1, node.column)
  in
  if
    nrow >= 0
    && nrow < Array.length maze
    && ncolumn >= 0
    && ncolumn < Array.length maze.(nrow)
    && maze.(nrow).(ncolumn) <> '#'
  then
    Some { node with row = nrow; column = ncolumn }
  else
    None

let rotate node =
  [
    { node with direction = counter_clock_wise node.direction };
    { node with direction = clock_wise node.direction };
  ]

(* Edge record type *)
type edge = {
  from_node: node;
  to_node: node;
  cost: int;
}


module NodeMap = Map.Make(struct
  type t = node
  let compare n1 n2 =
    let c1 = compare n1.row n2.row in
    if c1 <> 0 then c1 else
    let c2 = compare n1.column n2.column in
    if c2 <> 0 then c2 else
    compare n1.direction n2.direction
end)

let group_by f list =
  List.fold_left (fun acc x ->
    let key = f x in
    let data = try NodeMap.find key acc with Not_found -> [] in
    NodeMap.add key (x :: data) acc
  ) NodeMap.empty list

let dijkstra start edges =
  (* Group edges by their origin node *)
  let adjacent = group_by (fun e -> e.from_node) edges in
  
  (* Relaxation function that updates distances if better path found *)
  let relax from_node edge dist =
    let distance = NodeMap.find from_node dist in
    let new_distance = distance + edge.cost in
    
    match 
      try Some (NodeMap.find edge.to_node dist) with Not_found -> None 
    with
    | Some distance' when new_distance < distance' -> 
        Some (NodeMap.add edge.to_node new_distance dist)
    | None -> 
        Some (NodeMap.add edge.to_node new_distance dist)
    | _ -> 
        None
  in
  
  (* Transition function that processes a node and returns new nodes to process *)
  let transition node distance =
    let edges = 
      try NodeMap.find node adjacent with Not_found -> []
    in
    List.fold_left (fun (nodes_acc, dist_acc) edge ->
      match relax node edge dist_acc with
      | Some dist' -> (Some edge.to_node :: nodes_acc, dist')
      | None -> (None :: nodes_acc, dist_acc)
    ) ([], distance) edges
    |> fun (nodes, dist) -> (List.filter_map (fun x -> x) nodes, dist)
  in
  
  (* Main solver function *)
  let rec solve nodes distance =
    if nodes = [] then
      distance
    else
      let new_nodes, new_distance =
        List.fold_left (fun (nodes_acc, dist_acc) node ->
          let new_nodes, new_dist = transition node dist_acc in
          (new_nodes @ nodes_acc, new_dist)
        ) ([], distance) nodes
      in
      solve new_nodes new_distance
  in
  
  solve [start] (NodeMap.singleton start 0)

let all_pairs list1 list2 =
  List.concat_map (fun x -> List.map (fun y -> (x, y)) list2) list1

let collect_edges maze =
  all_pairs (List.init (Array.length maze) (fun i -> i))
           (List.init (Array.length maze.(0)) (fun j -> j))
  |> List.concat_map (fun (i, j) ->
      if maze.(i).(j) = '#' then
        []
      else
        [E; N; W; S]
        |> List.concat_map (fun direction ->
            let node = { 
              row = i; 
              column = j; 
              direction = direction } 
            in
            
            let forward_edge =
              match forward node maze with
              | Some node' -> [{ from_node = node; to_node = node'; cost = 1 }]
              | None -> []
            in
            
            let rotate_edges =
              rotate node
              |> List.map (fun node' -> { from_node = node; to_node = node'; cost = 1000 })
            in
            
            forward_edge @ rotate_edges
          )
    )

let find_index_2d_opt a v =
  all_pairs (List.init (Array.length a) (fun i -> i))
           (List.init (Array.length a.(0)) (fun j -> j))
  |> List.find_opt (fun (i, j) -> a.(i).(j) = v)

let part1 maze =
  let edges = collect_edges maze in
  
  (* Find start and end positions, unwrapping the options *)
  let start_i, start_j = 
    match find_index_2d_opt maze 'S' with
    | Some pos -> pos
    | None -> failwith "Start position not found"
  in
  
  let end_i, end_j = 
    match find_index_2d_opt maze 'E' with
    | Some pos -> pos
    | None -> failwith "End position not found"
  in
  
  (* Create start node *)
  let start = {
    row = start_i;
    column = start_j;
    direction = E;
  } in
  
  (* Calculate distances *)
  let distance = dijkstra start edges in
  
  (* Find minimum distance to goal in any direction *)
  [E; N; W; S]
  |> List.map (fun direction ->
      let goal = {
        row = end_i;
        column = end_j;
        direction = direction;
      } in
      try NodeMap.find goal distance
      with Not_found -> max_int)  (* If direction not reachable, use max_int *)
  |> List.fold_left min max_int   (* Find minimum distance *)



(* Find the direction that gives minimum distance to goal *)
let find_min_direction distance end_i end_j =
  List.fold_left (fun (min_dir, min_dist) dir ->
    let goal = { row = end_i; column = end_j; direction = dir } in
    let dist = try NodeMap.find goal distance with Not_found -> max_int in
    if dist < min_dist then (dir, dist) else (min_dir, min_dist)
  ) (E, max_int) [E; N; W; S]
  |> fst

let part2 maze =
  let edges = collect_edges maze in
  
  (* Find start and end positions *)
  let start_i, start_j = 
    match find_index_2d_opt maze 'S' with
    | Some pos -> pos
    | None -> failwith "Start position not found"
  in
  
  let end_i, end_j = 
    match find_index_2d_opt maze 'E' with
    | Some pos -> pos
    | None -> failwith "End position not found"
  in
  
  (* Create start node *)
  let start = {
    row = start_i;
    column = start_j;
    direction = E;
  } in
  
  (* Calculate distances from start *)
  let distance = dijkstra start edges in
  
  (* Find the minimum distance from part1 *)
  let min_distance = part1 maze in
  
  (* Find direction that gives minimum distance at goal *)
  let best_direction = find_min_direction distance end_i end_j in
  
  (* Create goal node with best direction *)
  let goal = { 
    row = end_i;
    column = end_j;
    direction = best_direction 
  } in
  
  (* Create reversed edges for path from goal back to start *)
  let reversed_edges = 
    List.map (fun edge -> 
      { from_node = edge.to_node; to_node = edge.from_node; cost = edge.cost }
    ) edges 
  in
  
  (* Calculate distances from goal back to start *)
  let reversed_distance = dijkstra goal reversed_edges in
  
  (* Count nodes on the shortest path *)
  all_pairs (List.init (Array.length maze) (fun i -> i))
           (List.init (Array.length maze.(0)) (fun j -> j))
  |> List.filter (fun (i, j) -> 
      maze.(i).(j) <> '#' && 
      List.exists (fun direction ->
        let node = {
          row = i;
          column = j;
          direction = direction
        } in
        let forward = try Some (NodeMap.find node distance) with Not_found -> None in
        let backward = try Some (NodeMap.find node reversed_distance) with Not_found -> None in
        match forward, backward with
        | Some d, Some rd when d + rd = min_distance -> true
        | _ -> false
      ) [E; N; W; S]
    )
  |> List.length



(** Parse input string into 2D array
    @param input Raw input string
    @return 2D array of characters representing the maze *)
let parse input =
  input 
  |> Str.split (Str.regexp "[\r\n]+")  (* Split on \r and \n sequences, skipping empty lines *)
  |> List.map String.trim
  |> List.map String.to_seq
  |> List.map Array.of_seq
  |> Array.of_list


(** Main entry point *)
let () =
  In_channel.input_all In_channel.stdin
  |> String.trim
  |> parse
  |> (fun maze ->
      let start_time = Unix.gettimeofday () in

      maze |> part1 |> Printf.printf "Part 1: %d\n";
      maze |> part2 |> Printf.printf "Part 2: %d\n";

      Unix.gettimeofday () -. start_time)
  |> Printf.printf "Elapsed time: %.4f seconds\n"
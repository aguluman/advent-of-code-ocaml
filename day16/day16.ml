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

module PriorityQueue = struct
  type 'a t = {
    mutable heap: (int * 'a) array;
    mutable size: int;
  }
  
  let create capacity =
    { heap = Array.make capacity (max_int, Obj.magic ()); size = 0 }
  
  let parent i = (i - 1) / 2
  let left i = 2 * i + 1
  let right i = 2 * i + 2
  
  let swap a i j =
    let temp = a.(i) in
    a.(i) <- a.(j);
    a.(j) <- temp
  
  let grow q =
    let new_heap = Array.make (Array.length q.heap * 2) (max_int, Obj.magic ()) in
    Array.blit q.heap 0 new_heap 0 q.size;
    q.heap <- new_heap
  
  let insert q priority item =
    if q.size = Array.length q.heap then grow q;
    q.heap.(q.size) <- (priority, item);

    let rec bubble_up i =
      let p = parent i in
      if i > 0 && fst q.heap.(i) < fst q.heap.(p) then (
        swap q.heap i p;
        bubble_up p
      )
    in
    bubble_up q.size;
    q.size <- q.size + 1
  
  let extract_min q =
    if q.size = 0 then None
    else (
      let min_item = q.heap.(0) in
      q.heap.(0) <- q.heap.(q.size - 1);
      q.size <- q.size - 1;
      
      let rec heapify i =
        let l = left i in
        let r = right i in
        let smallest = ref i in
        if l < q.size && fst q.heap.(l) < fst q.heap.(!smallest) then
          smallest := l;
        if r < q.size && fst q.heap.(r) < fst q.heap.(!smallest) then
          smallest := r;
        if !smallest <> i then (
          swap q.heap i !smallest;
          heapify !smallest
        )
      in
      
      if q.size > 0 then heapify 0;
      Some min_item
    )

end



let dijkstra start edges =
  (* Group edges by their origin node *)
  let adjacent = group_by (fun e -> e.from_node) edges in
  
  (* Create distance map and priority queue *)
  let distances = ref (NodeMap.singleton start 0) in
  let queue = PriorityQueue.create 1024 in
  PriorityQueue.insert queue 0 start;
  
  (* Process nodes by distance until queue is empty *)
  let rec process () =
    match PriorityQueue.extract_min queue with
    | None -> !distances
    | Some (dist, node) ->
        (* Skip if we've already found a better path *)
        let current_dist = try NodeMap.find node !distances with Not_found -> max_int in
        if dist > current_dist then process ()
        else (
          (* Process all adjacent edges *)
          let edges = try NodeMap.find node adjacent with Not_found -> [] in
          List.iter (fun edge ->
            let new_dist = dist + edge.cost in
            let to_node = edge.to_node in
            let existing = 
              try Some (NodeMap.find to_node !distances) 
              with Not_found -> None 
            in
            match existing with
            | Some old_dist when new_dist < old_dist ->
                distances := NodeMap.add to_node new_dist !distances;
                PriorityQueue.insert queue new_dist to_node
            | None ->
                distances := NodeMap.add to_node new_dist !distances;
                PriorityQueue.insert queue new_dist to_node
            | _ -> ()
          ) edges;
          process ()
        )
  in
  
  process ()

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
  let height = Array.length a in
  if height = 0 then None
  else 
    let width = Array.length a.(0) in
    let rec loop i j =
      if i >= height then None
      else if j >= width then loop (i+1) 0
      else if a.(i).(j) = v then Some (i, j)
      else loop i (j+1)
    in
    loop 0 0


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
  
  (* Calculate distances from start - use optimized Dijkstra *)
  let distance = dijkstra start edges in
  
  (* Find minimum distance to any goal orientation and the direction *)
  let min_distance, best_direction = 
    List.fold_left (fun (min_dist, min_dir) dir ->
      let goal = { row = end_i; column = end_j; direction = dir } in
      let dist = try NodeMap.find goal distance with Not_found -> max_int in
      if dist < min_dist then (dist, dir) else (min_dist, min_dir)
    ) (max_int, E) [E; N; W; S]
  in
  
  (* Create goal node with best direction *)
  let goal = { 
    row = end_i;
    column = end_j;
    direction = best_direction 
  } in
  
  (* Create reversed edges *)
  let reversed_edges = 
    List.map (fun edge -> 
      { from_node = edge.to_node; to_node = edge.from_node; cost = edge.cost }
    ) edges 
  in
  
  (* Calculate distances from goal back to start *)
  let reversed_distance = dijkstra goal reversed_edges in
  
  (* Create a cache for nodes on shortest path *)
  let path_nodes = Hashtbl.create 1024 in
  
  (* Count nodes on the shortest path more efficiently *)
  let count = ref 0 in
  for i = 0 to Array.length maze - 1 do
    for j = 0 to Array.length maze.(0) - 1 do
      if maze.(i).(j) <> '#' then (
        if Hashtbl.mem path_nodes (i, j) then
          incr count
        else (
          let found = ref false in
          let dir_iter = [E; N; W; S] in
          List.iter (fun direction ->
            if not !found then (
              let node = { row = i; column = j; direction = direction } in
              let forward = try Some (NodeMap.find node distance) with Not_found -> None in
              let backward = try Some (NodeMap.find node reversed_distance) with Not_found -> None in
              match forward, backward with
              | Some d, Some rd when d + rd = min_distance ->
                  found := true;
                  Hashtbl.add path_nodes (i, j) true;
                  incr count
              | _ -> ()
            )
          ) dir_iter
        )
      )
    done
  done;
  !count



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
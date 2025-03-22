module IntPair = struct
  type t = int * int
  let compare = compare
end

module PairMap = Map.Make(IntPair)
module PairSet = Set.Make(IntPair)

let solve n positions =
  let positions = PairSet.of_list positions in

  let transition (x, y) distance =
    let directions = [(-1, 0); (0, -1); (1, 0); (0, 1)] in
    let new_nodes, new_distance =
      List.fold_left
        (fun (nodes, dist) (dx, dy) ->
          let nx, ny = x + dx, y + dy in
          if nx >= 0 && nx <= n && ny >= 0 && ny <= n &&
             not (PairSet.mem (nx, ny) positions) &&
             not (PairMap.mem (nx, ny) dist)
          then
            (Some (nx, ny) :: nodes, PairMap.add (nx, ny) (PairMap.find (x, y) dist + 1) dist)
          else
            (None :: nodes, dist))
        ([], distance)
        directions
    in
    (List.filter_map (fun x -> x) new_nodes, new_distance)
  in

  let rec bfs nodes distance =
    if nodes = [] then
      distance
    else
      let combined_results = 
        List.fold_left
          (fun (node_lists, dist) node ->
            let new_nodes, new_dist = transition node dist in
            (new_nodes :: node_lists, new_dist))
          ([], distance)
          nodes
      in
      let new_nodes_lists, final_dist = combined_results in
      let all_new_nodes = List.flatten (List.rev new_nodes_lists) in
      bfs all_new_nodes final_dist
  in

  let start_distance = PairMap.singleton (0, 0) 0 in
  let final_distance = bfs [(0, 0)] start_distance in
  PairMap.find_opt (n, n) final_distance


let part1 (n, bytes, positions) =
  let positions_list = 
    positions
    |> Seq.take bytes
    |> List.of_seq
  in
  match solve n positions_list with
  | Some value -> value
  | None -> failwith "No solution found"


let part2 (n, positions) =
  let positions_list = List.of_seq positions in
  
  (* Helper function to take first n elements of a list *)
  let take n lst =
    let rec aux n acc = function
      | [] -> List.rev acc
      | h :: t -> if n <= 0 then List.rev acc else aux (n-1) (h::acc) t
    in
    aux n [] lst
  in
  
  let rec bisect ok ng =
    if ok + 1 = ng then 
      ng
    else 
      let mid = (ok + ng) / 2 in
      
      match solve n (take (mid + 1) positions_list) with
      | Some _ -> bisect mid ng
      | None -> bisect ok mid
  in
  
  let i = bisect 0 (List.length positions_list) in
  List.nth positions_list i


let parse input =
  input
  |> String.split_on_char '\n'
  |> List.filter (fun s -> String.length s > 0) (* Handle potential empty lines *)
  |> List.map (fun line ->
      let parts = String.split_on_char ',' line in
      match parts with
      | x :: y :: _ -> (int_of_string x, int_of_string y)
      | _ -> failwith "Invalid input format: each line should have at least two numbers"
    )
  |> List.to_seq


let () =
  try
    let input = In_channel.input_all In_channel.stdin |> String.trim in
    let positions = parse input in
    
    let start_time = Unix.gettimeofday () in
    
    let answer1 = part1 (70, 1024, positions) in
    Printf.printf "Part 1: %d\n" answer1;
    
    let (x, y) = part2 (70, positions) in
    Printf.printf "Part 2: (%d, %d)\n" x y;
    
    Unix.gettimeofday () -. start_time
    |> Printf.printf "Elapsed time: %.8f seconds\n"
    
  with
  | Failure msg -> Printf.printf "Error: %s\n" msg
  | e -> Printf.printf "Unexpected error: %s\n" (Printexc.to_string e)
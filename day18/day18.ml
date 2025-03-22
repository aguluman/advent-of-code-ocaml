module IntPair = struct
  type t = int * int
  let compare = compare
end

module PairMap = Map.Make(IntPair)
module PairSet = Set.Make(IntPair)

(* Use a grid-based representation for faster checking *)
let create_grid n positions =
  let grid = Array.make_matrix (n+1) (n+1) false in
  List.iter (fun (x, y) -> grid.(y).(x) <- true) positions;
  grid

let solve n positions =
  let grid = create_grid n positions in
  
  (* Using a real queue for BFS *)
  let queue = Queue.create () in
  Queue.add (0, 0) queue;
  
  let distance = ref (PairMap.singleton (0, 0) 0) in
  
  let directions = [(-1, 0); (0, -1); (1, 0); (0, 1)] in
  
  (* BFS with queue *)
  let rec process_queue () =
    if Queue.is_empty queue then
      !distance
    else
      let (x, y) = Queue.take queue in
      let current_dist = PairMap.find (x, y) !distance in
      
      List.iter (fun (dx, dy) ->
        let nx, ny = x + dx, y + dy in
        if nx >= 0 && nx <= n && ny >= 0 && ny <= n &&
           not grid.(ny).(nx) &&
           not (PairMap.mem (nx, ny) !distance)
        then begin
          distance := PairMap.add (nx, ny) (current_dist + 1) !distance;
          Queue.add (nx, ny) queue
        end
      ) directions;
      
      process_queue ()
  in
  
  let final_distance = process_queue () in
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
  
  (* Using Array.sub for more efficient slicing *)
  let positions_array = Array.of_list positions_list in
  
  let take count =
    if count > Array.length positions_array then positions_list
    else Array.sub positions_array 0 count |> Array.to_list
  in
  
  (* Optimized binary search with early exit *)
  let rec bisect ok ng =
    if ok + 1 = ng then 
      ng
    else 
      let mid = (ok + ng) / 2 in
      
      match solve n (take (mid + 1)) with
      | Some _ -> 
          (* Solution exists, try with fewer bytes *)
          bisect mid ng
      | None -> 
          (* No solution exists with mid+1 bytes *)
          match solve n (take mid) with
          | Some _ -> 
              (* Found exact cutoff point *)
              mid
          | None ->
              (* Keep searching in lower half *)
              bisect ok mid
  in
  
  let i = bisect 0 (List.length positions_list) in
  List.nth positions_list i

let parse input =
  input
  |> String.split_on_char '\n'
  |> List.filter (fun s -> String.length s > 0)
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
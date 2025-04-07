open Domainslib

module CoordHash = Hashtbl.Make(struct
  type t = int * int
  let equal (x1,y1) (x2,y2) = x1 = x2 && y1 = y2
  let hash (x,y) = (x lsl 16) + y
end)

module UnboxedArray = struct
  type 'a t = 'a array
  external create: int -> 'a -> 'a t = "caml_make_vect"
  external get: 'a t -> int -> 'a = "%array_safe_get"
  external set: 'a t -> int -> 'a -> unit = "%array_safe_set"
  external length: 'a t -> int = "%array_length"
end

let inline_idx cols i j = i * cols + j

(* Optimized finder with early termination *)
let find_index_2d array value =
  let rows = Array.length array in
  let cols = Array.length array.(0) in
  let found = ref (-1, -1) in
  try
    for i = 0 to rows - 1 do
      for j = 0 to cols - 1 do
        if array.(i).(j) = value then begin
          found := (i, j);
          raise Exit  (* Early termination *)
        end
      done
    done;
    raise Not_found
  with Exit -> !found

(* Ultra-optimized BFS using fixed direction array and loop unrolling *)
let bfs (si, sj) (maze : char array array) =
  let module Q = Queue in
  let q = Q.create () in
  let rows = Array.length maze in
  let cols = Array.length maze.(0) in
  let size = rows * cols in
  let dist = UnboxedArray.create size max_int in
  let visited = Bytes.make size '\000' in
  
  (* Direction offsets - stored but used directly in unrolled loop *)
  let _directions = [|(-1, 0); (0, -1); (1, 0); (0, 1)|] in
  
  let idx = inline_idx cols in
  UnboxedArray.set dist (idx si sj) 0;
  Bytes.set visited (idx si sj) '\001';
  Q.push (si, sj) q;
  
  while not (Q.is_empty q) do
    let (i, j) = Q.pop q in
    let curr_dist = UnboxedArray.get dist (idx i j) in
    let next_dist = curr_dist + 1 in
    
    (* Unrolled direction checking for better branch prediction *)
    let ni, nj = i - 1, j in  (* Up *)
    if ni >= 0 && maze.(ni).(nj) <> '#' then
      let nidx = idx ni nj in
      if Bytes.get visited nidx = '\000' then begin
        Bytes.set visited nidx '\001';
        UnboxedArray.set dist nidx next_dist;
        Q.push (ni, nj) q
      end;
      
    let ni, nj = i, j - 1 in  (* Left *)
    if nj >= 0 && maze.(ni).(nj) <> '#' then
      let nidx = idx ni nj in
      if Bytes.get visited nidx = '\000' then begin
        Bytes.set visited nidx '\001';
        UnboxedArray.set dist nidx next_dist;
        Q.push (ni, nj) q
      end;
      
    let ni, nj = i + 1, j in  (* Down *)
    if ni < rows && maze.(ni).(nj) <> '#' then
      let nidx = idx ni nj in
      if Bytes.get visited nidx = '\000' then begin
        Bytes.set visited nidx '\001';
        UnboxedArray.set dist nidx next_dist;
        Q.push (ni, nj) q
      end;
      
    let ni, nj = i, j + 1 in  (* Right *)
    if nj < cols && maze.(ni).(nj) <> '#' then
      let nidx = idx ni nj in
      if Bytes.get visited nidx = '\000' then begin
        Bytes.set visited nidx '\001';
        UnboxedArray.set dist nidx next_dist;
        Q.push (ni, nj) q
      end;
  done;
  dist

let part1 (maze : char array array) =
  let rows = Array.length maze in
  let cols = Array.length maze.(0) in
  let si, sj = find_index_2d maze 'S' in
  let gi, gj = find_index_2d maze 'E' in
  
  (* Pre-compute original BFS *)
  let init_dist = bfs (si, sj) maze in
  let original_dist = UnboxedArray.get init_dist (inline_idx cols gi gj) in
  
  (* Pre-identify all potential walls to check *)
  let wall_candidates = Array.make (rows * cols) false in
  let count = ref 0 in
  
  for i = 1 to rows - 2 do
    for j = 1 to cols - 2 do
      if maze.(i).(j) = '#' then
        let is_vertical = maze.(i-1).(j) <> '#' && maze.(i+1).(j) <> '#' in
        let is_horizontal = maze.(i).(j-1) <> '#' && maze.(i).(j+1) <> '#' in
        if is_vertical || is_horizontal then begin
          wall_candidates.(inline_idx cols i j) <- true;
          incr count
        end
    done
  done;
  
  (* Create flat arrays for better cache locality *)
  let wall_positions = Array.make !count (0, 0) in
  let k = ref 0 in
  for i = 1 to rows - 2 do
    for j = 1 to cols - 2 do
      if wall_candidates.(inline_idx cols i j) then begin
        wall_positions.(!k) <- (i, j);
        incr k
      end
    done
  done;
  
  (* Process in parallel *)
  let pool = Task.setup_pool ~num_domains:8 () in
  
  (* Use Task.parallel_for with manually tracked results *)
  let improvements = Array.make !count 0 in
  
  Task.parallel_for pool ~start:0 ~finish:(!count - 1) ~body:(fun idx ->
    let i, j = wall_positions.(idx) in
    maze.(i).(j) <- '.';  (* Remove wall *)
    let new_dist = bfs (si, sj) maze in
    maze.(i).(j) <- '#';  (* Restore wall *)
    
    let after_idx = inline_idx cols gi gj in
    let after_dist = UnboxedArray.get new_dist after_idx in
    
    if after_dist <> max_int && after_dist < original_dist then
      improvements.(idx) <- original_dist - after_dist
  );
  
  Task.teardown_pool pool;
  
  (* Count frequencies using Array instead of Map for speed *)
  let max_diff = 100 in (* Reasonable upper bound *)
  let freq = Array.make (max_diff + 1) 0 in
  
  Array.iter (fun x ->
    if x > 0 && x <= max_diff then freq.(x) <- freq.(x) + 1
  ) improvements;
  
  (* Convert to final result format *)
  let module IntMap = Map.Make(Int) in
  let counts = ref IntMap.empty in
  
  for i = 0 to max_diff do
    if freq.(i) > 0 then
      counts := IntMap.add i freq.(i) !counts
  done;
  
  IntMap.bindings !counts |> List.sort compare

let part2 (maze : char array array) =
  let si, sj = find_index_2d maze 'S' in
  let dist = bfs (si, sj) maze in
  let rows = Array.length maze in
  let cols = Array.length maze.(0) in
  
  (* Pre-compute reachable points *)
  let point_count = ref 0 in
  for i = 0 to rows - 1 do
    for j = 0 to cols - 1 do
      if UnboxedArray.get dist (inline_idx cols i j) <> max_int then
        incr point_count
    done
  done;
  
  let points = Array.make !point_count (0, 0, 0) in
  let idx = ref 0 in
  
  for i = 0 to rows - 1 do
    for j = 0 to cols - 1 do
      let d = UnboxedArray.get dist (inline_idx cols i j) in
      if d <> max_int then begin
        points.(!idx) <- (i, j, d);
        incr idx
      end
    done
  done;
  
  (* Sort points by distance for better iteration *)
  Array.sort (fun (_, _, d1) (_, _, d2) -> compare d1 d2) points;
  
  (* Process in parallel with efficient chunking *)
  let pool = Task.setup_pool ~num_domains:8 () in
  let chunks = min 16 !point_count in
  let chunk_size = (!point_count + chunks - 1) / chunks in
  
  (* Pre-allocate result array *)
  let results = Array.make chunks [] in
  
  Task.parallel_for pool ~start:0 ~finish:(chunks - 1) ~body:(fun chunk ->
    let start_idx = chunk * chunk_size in
    let end_idx = min ((chunk + 1) * chunk_size - 1) (!point_count - 1) in
    let local_diffs = ref [] in
    
    for i = start_idx to end_idx do
      let (x1, y1, d1) = points.(i) in
      (* Only need to check within manhattan distance <= 20 *)
      for j = 0 to !point_count - 1 do
        let (x2, y2, d2) = points.(j) in
        let e = abs (x1 - x2) + abs (y1 - y2) in
        if e <= 20 && d2 - d1 >= 0 then
          local_diffs := (d2 - d1 - e) :: !local_diffs
      done
    done;
    
    results.(chunk) <- !local_diffs
  );
  
  (* Combine results *)
  let all_diffs = Array.fold_left (@) [] results in
  
  Task.teardown_pool pool;
  
  (* Fast frequency counting *)
  let freq_table = Hashtbl.create 1024 in
  List.iter (fun x ->
    let count = try Hashtbl.find freq_table x with Not_found -> 0 in
    Hashtbl.replace freq_table x (count + 1)
  ) all_diffs;
  
  (* Convert to result format *)
  let module IntMap = Map.Make(Int) in
  let counts = Hashtbl.fold (fun k v acc -> IntMap.add k v acc) freq_table IntMap.empty in
  
  IntMap.bindings counts |> List.sort compare

let parse input =
  input
  |> String.split_on_char '\n'
  |> List.filter (fun s -> String.trim s <> "")
  |> List.map (fun row -> Array.of_list (List.init (String.length row) (String.get row)))
  |> Array.of_list
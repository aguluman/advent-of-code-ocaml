module CoordHash = Hashtbl.Make(struct
  type t = int * int
  let equal (x1,y1) (x2,y2) = x1 = x2 && y1 = y2
  let hash (x,y) = (x lsl 16) + y
end)

(* Use specialized array for better memory layout *)
module UnboxedArray = struct
  type 'a t = 'a array
  external create: int -> 'a -> 'a t = "caml_make_vect"
  external get: 'a t -> int -> 'a = "%array_safe_get"
  external set: 'a t -> int -> 'a -> unit = "%array_safe_set"
  external length: 'a t -> int = "%array_length"
end

let idx cols i j = i * cols + j


let find_index_2d array value =
  let rows = Array.length array in
  let cols = Array.length array.(0) in
  let found = ref None in
  
  for i = 0 to rows - 1 do
    for j = 0 to cols - 1 do
      if array.(i).(j) = value then
        found := Some (i, j)
    done;
  done;
  
  match !found with
  | Some pos -> pos
  | None -> raise Not_found


(* Optimized version using Array for visited tracking *)
let bfs (si, sj) (maze : char array array) =
  let module Q = Queue in
  let q = Q.create () in
  let rows = Array.length maze in
  let cols = Array.length maze.(0) in
  let dist = UnboxedArray.create (rows * cols) max_int in
  let visited = Bytes.make (rows * cols) '\000' in
  
  UnboxedArray.set dist (idx cols si sj) 0;
  Bytes.set visited (idx cols si sj) '\001';
  Q.push (si, sj) q;
  
  while not (Q.is_empty q) do
    let (i, j) = Q.pop q in
    let curr_dist = UnboxedArray.get dist (idx cols i j) in
    
    [(-1, 0); (0, -1); (1, 0); (0, 1)]
    |> List.iter (fun (di, dj) ->
      let ni, nj = i + di, j + dj in
      let nidx = idx cols ni nj in
      if ni >= 0 && ni < rows && nj >= 0 && nj < cols &&
         maze.(ni).(nj) <> '#' && Bytes.get visited nidx = '\000'
      then begin
        UnboxedArray.set dist nidx (curr_dist + 1);
        Bytes.set visited nidx '\001';
        Q.push (ni, nj) q
      end)
  done;
  dist



let part1 (maze : char array array) =
  (* First get the original shortest path length *)
  let rows = Array.length maze in
  let cols = Array.length maze.(0) in
  let si, sj = find_index_2d maze 'S' in
  let gi, gj = find_index_2d maze 'E' in
  let init_dist = bfs (si, sj) maze in
  let original_dist = UnboxedArray.get init_dist (idx cols gi gj) in
  
  (* Create a copy of the maze for safe parallel processing *)
  let maze_copy = Array.map Array.copy maze in
  
  (* Sequential processing for consistent results *)
  let wall_improvements = ref [] in
  for i = 1 to rows - 2 do
    for j = 1 to cols - 2 do
      if maze.(i).(j) = '#' then
        (* Check if wall has passage through it (vertical or horizontal) *)
        let is_vertical_passage = maze.(i-1).(j) <> '#' && maze.(i+1).(j) <> '#' in
        let is_horizontal_passage = maze.(i).(j-1) <> '#' && maze.(i).(j+1) <> '#' in
        
        if is_vertical_passage || is_horizontal_passage then begin
          (* Try removing the wall *)
          maze_copy.(i).(j) <- '.';
          let new_dist = bfs (si, sj) maze_copy in
          let after_dist = UnboxedArray.get new_dist (idx cols gi gj) in
          maze_copy.(i).(j) <- '#';  (* Restore the wall *)
          
          (* If removing wall improves path length, record the improvement *)
          if after_dist <> max_int && after_dist < original_dist then
            wall_improvements := (original_dist - after_dist) :: !wall_improvements
        end
    done
  done;
  
  (* Count the frequencies of each improvement value *)
  let module IntMap = Map.Make(Int) in
  let counts = 
    List.fold_left (fun acc x ->
      let count = try IntMap.find x acc with Not_found -> 0 in
      IntMap.add x (count + 1) acc
    ) IntMap.empty !wall_improvements
  in
  
  (* Return sorted list of (improvement, frequency) pairs *)
  IntMap.bindings counts |> List.sort compare





let part2 (maze : char array array) =
  let si, sj = find_index_2d maze 'S' in
  let dist = bfs (si, sj) maze in
  let differences = ref [] in
  let rows = Array.length maze in
  let cols = Array.length maze.(0) in
  let idx i j = i * cols + j in
  
  for i = 0 to rows - 1 do
    for j = 0 to cols - 1 do
      for i' = 0 to rows - 1 do
        for j' = 0 to cols - 1 do
          let d = UnboxedArray.get dist (idx i j) in
          let d' = UnboxedArray.get dist (idx i' j') in
          if d <> max_int && d' <> max_int then begin
            let e = abs (i - i') + abs (j - j') in
            if d' - d >= 0 && e <= 20 then
              differences := (d' - d - e) :: !differences
          end
        done
      done
    done
  done;
  
  let module IntMap = Map.Make(Int) in
  let counts = 
    List.fold_left (fun acc x ->
      let count = try IntMap.find x acc with Not_found -> 0 in
      IntMap.add x (count + 1) acc
    ) IntMap.empty !differences
  in
  
  IntMap.bindings counts |> List.sort compare




let parse input =
  input
  |> String.split_on_char '\n'
  |> List.filter (fun s -> String.trim s <> "")
  |> List.map (fun row -> Array.of_list (List.init (String.length row) (String.get row)))
  |> Array.of_list
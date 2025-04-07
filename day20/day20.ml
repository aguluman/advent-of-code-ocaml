module CoordHash = Hashtbl.Make(struct
  type t = int * int
  let equal (x1,y1) (x2,y2) = x1 = x2 && y1 = y2
  let hash (x,y) = (x lsl 16) + y
end)

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

let bfs (si, sj) (maze : char array array) =
  let module Q = Queue in
  let q = Q.create () in
  let dist = CoordHash.create 10000 in
  let rows = Array.length maze in
  let cols = Array.length maze.(0) in
  
  CoordHash.add dist (si, sj) 0;
  Q.push (si, sj) q;
  
  while not (Q.is_empty q) do
    let (i, j) = Q.pop q in
    let curr_dist = CoordHash.find dist (i, j) in
    
    [(-1, 0); (0, -1); (1, 0); (0, 1)]
    |> List.iter (fun (di, dj) ->
      let ni, nj = i + di, j + dj in
      if ni >= 0 && ni < rows && nj >= 0 && nj < cols &&
         maze.(ni).(nj) <> '#' &&
         not (CoordHash.mem dist (ni, nj))
      then begin
        CoordHash.add dist (ni, nj) (curr_dist + 1);
        Q.push (ni, nj) q
      end)
  done;
  dist

let part1 (maze : char array array) =
  let si, sj = find_index_2d maze 'S' in
  let gi, gj = find_index_2d maze 'E' in
  let rows = Array.length maze in
  let cols = Array.length maze.(0) in
  let init_dist = bfs (si, sj) maze in
  let differences = ref [] in
  
  for i = 1 to rows - 2 do
    for j = 1 to cols - 2 do
      if maze.(i).(j) = '#' then begin
        let tate = maze.(i-1).(j) <> '#' && maze.(i+1).(j) <> '#' in
        let yoko = maze.(i).(j-1) <> '#' && maze.(i).(j+1) <> '#' in
        
        if tate || yoko then begin
          maze.(i).(j) <- '.';
          let new_dist = bfs (si, sj) maze in
          maze.(i).(j) <- '#';
          
          let original_dist = CoordHash.find init_dist (gi, gj) in
          let after_dist = CoordHash.find new_dist (gi, gj) in
          differences := (original_dist - after_dist) :: !differences
        end
      end
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

let part2 (maze : char array array) =
  let si, sj = find_index_2d maze 'S' in
  let dist = bfs (si, sj) maze in
  let differences = ref [] in
  
  CoordHash.iter (fun (i, j) d ->
    CoordHash.iter (fun (i', j') d' ->
      let e = abs (i - i') + abs (j - j') in
      if d' - d >= 0 && e <= 20 then
        differences := (d' - d - e) :: !differences
    ) dist
  ) dist;
  
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
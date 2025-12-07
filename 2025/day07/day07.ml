(** Day 07: Laboratories

    [Brief description of the problem and what this module solves]

    {2 Problem Summary:}
    - {b Part 1:} Count how many times the beam splits at '^' splitters.
    - {b Part 2:} Count total number of distinct complete paths (timelines).

    See details at:
    {{:https://adventofcode.com/2025/day/7} Advent of Code 2025, Day 07} *)

(** Helper function to parse input string into a structured data format

    @param input Raw input string from the puzzle
    @return Parsed data structure *)
let parse input =
  let lines =
    input |> String.split_on_char '\n' |> List.filter (fun l -> l <> "")
  in
  let grid_arr =
    Array.of_list
      (List.map (fun s -> Array.init (String.length s) (fun i -> s.[i])) lines)
  in
  let height = Array.length grid_arr in
  let width = if height > 0 then Array.length grid_arr.(0) else 0 in
  let start_r, start_c =
    let found = ref None in
    for r = 0 to height - 1 do
      for c = 0 to width - 1 do
        if grid_arr.(r).(c) = 'S' then found := Some (r, c)
      done
    done;
    match !found with
    | Some p -> p
    | None -> failwith "No 'S' found"
  in
  (grid_arr, height, width, start_r, start_c)

let rec bfs grid_arr height width visited queue count =
  if Queue.is_empty queue then count
  else
    let r, c = Queue.take queue in
    if r < 0 || r >= height || c < 0 || c >= width || visited.(r).(c) then
      bfs grid_arr height width visited queue count
    else begin
      visited.(r).(c) <- true;
      let cell = grid_arr.(r).(c) in
      if cell = '^' then begin
        Queue.add (r + 1, c - 1) queue;
        Queue.add (r + 1, c + 1) queue;
        bfs grid_arr height width visited queue (count + 1)
      end
      else if cell = '.' || cell = 'S' then begin
        Queue.add (r + 1, c) queue;
        bfs grid_arr height width visited queue count
      end
      else bfs grid_arr height width visited queue count
    end

let count_paths grid_arr height width =
  let memo = Hashtbl.create 10000 in

  let rec dfs r c =
    if c < 0 || c >= width || r >= height then 1L
    else if r < 0 then 0L
    else
      match Hashtbl.find_opt memo (r, c) with
      | Some cached -> cached
      | None ->
          let cell = grid_arr.(r).(c) in
          let result =
            if cell = '^' then
              Int64.add (dfs (r + 1) (c - 1)) (dfs (r + 1) (c + 1))
            else if cell = '.' || cell = 'S' then
              dfs (r + 1) c
            else 0L
          in
          Hashtbl.add memo (r, c) result;
          result
  in
  dfs

(** [part1 input] solves part 1 of the challenge

    @param input Raw input string from the puzzle
    @return Solution for part 1 *)
let part1 input =
  let grid_arr, height, width, start_r, start_c = parse input in
  let visited = Array.make_matrix height width false in
  let queue = Queue.create () in
  Queue.add (start_r, start_c) queue;
  bfs grid_arr height width visited queue 0

(** [part2 input] solves part 2 of the challenge

    @param input Raw input string from the puzzle
    @return Solution for part 2 *)
let part2 input =
  let grid_arr, height, width, start_r, start_c = parse input in
  let dfs = count_paths grid_arr height width in
  dfs start_r start_c

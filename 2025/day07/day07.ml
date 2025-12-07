(** Day 07: Laboratories

    [Brief description of the problem and what this module solves]

    {2 Problem Summary:}
    - {b Part 1:} [Description of part 1]
    - {b Part 2:} [Description of part 2]

    See details at:
    {{:https://adventofcode.com/2025/day/7} Advent of Code 2025, Day 07} *)

(** Helper function to parse input string into a structured data format

    @param input Raw input string from the puzzle
    @return Parsed data structure *)
let parse input =
  let lines =
    input |> String.split_on_char '\n' |> List.filter (fun l -> l <> "")
  in
  let height = List.length lines in
  let width = if height > 0 then String.length (List.hd lines) else 0 in
  let grid = lines in
  let start_r, start_c =
    List.mapi
      (fun r line ->
        let c = String.index_opt line 'S' in
        match c with
        | Some c -> (r, c)
        | None -> (-1, -1))
      grid
    |> List.find (fun (r, _) -> r >= 0)
  in
  (grid, height, width, start_r, start_c)

(** Enqueue a position to the queue (functional: append to list) *)
let enqueue queue pos = queue @ [ pos ]

(** Dequeue from queue (functional: head and tail) *)
let dequeue = function
  | [] -> failwith "Empty queue"
  | hd :: tl -> (hd, tl)

let rec bfs grid_arr height width visited queue count =
  if queue = [] then count
  else
    let (r, c), queue' = dequeue queue in
    if r < 0 || r >= height || c < 0 || c >= width || visited.(r).(c) then
      bfs grid_arr height width visited queue' count
    else begin
      visited.(r).(c) <- true;
      let cell = grid_arr.(r).(c) in
      if cell = '^' then
        let queue'' = enqueue (enqueue queue' (r + 1, c - 1)) (r + 1, c + 1) in
        bfs grid_arr height width visited queue'' (count + 1)
      else if cell = '.' || cell = 'S' then
        let queue'' = enqueue queue' (r + 1, c) in
        bfs grid_arr height width visited queue'' count
      else bfs grid_arr height width visited queue' count
    end

(** [part1 input] solves part 1 of the challenge

    @param input Raw input string from the puzzle
    @return Solution for part 1 *)
let part1 input =
  let grid, height, width, start_r, start_c = parse input in
  let grid_arr =
    Array.of_list
      (List.map (fun s -> Array.init (String.length s) (fun i -> s.[i])) grid)
  in
  let visited = Array.make_matrix height width false in
  let queue = [ (start_r, start_c) ] in
  bfs grid_arr height width visited queue 0

(** [part2 input] solves part 2 of the challenge

    @param input Raw input string from the puzzle
    @return Solution for part 2 *)
let part2 input =
  let _ = parse input in
  (* TODO: Implement part 2 solution *)
  0L

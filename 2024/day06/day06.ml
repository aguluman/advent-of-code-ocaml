(** Solution for Day 6 of Advent of Code. Implements path finding in a 2D grid
    with directional movement. *)

(** Direction represents possible movements in the grid. *)
type direction =
  | Up  (** Move upward *)
  | Right  (** Move rightward *)
  | Down  (** Move downward *)
  | Left  (** Move leftward *)

(** [next_direction dir] returns the next direction in clockwise rotation. *)
let next_direction = function
  | Up -> Right
  | Right -> Down
  | Down -> Left
  | Left -> Up

(** [get_direction_offset dir] returns (dx, dy) for a direction. *)
let get_direction_offset = function
  | Up -> (0, -1)
  | Right -> (1, 0)
  | Down -> (0, 1)
  | Left -> (-1, 0)

(** Position set module *)
module PosSet = Set.Make (struct
  type t = int * int

  let compare = compare
end)

(** State set module for tracking position + direction *)
module StateSet = Set.Make (struct
  type t = (int * int) * direction

  let compare = compare
end)

(** [parse_obstacles_and_start map] extracts obstacles as a hash table and finds
    start position. *)
let parse_obstacles_and_start map =
  let n = Array.length map in
  let obstacles = Hashtbl.create (n * n / 4) in
  (* Estimate capacity *)
  let start = ref None in

  for i = 0 to n - 1 do
    for j = 0 to Array.length map.(i) - 1 do
      match map.(i).(j) with
      | '#' -> Hashtbl.add obstacles (j, i) () (* Note: (x, y) format *)
      | '^' -> start := Some (j, i)
      | _ -> ()
    done
  done;

  match !start with
  | Some s -> (obstacles, s, Array.length map.(0), n)
  | None -> failwith "No starting position found"

(** [get_guard_positions obstacles start width height] returns all positions
    visited by guard. *)
let get_guard_positions obstacles start width height =
  let visited = ref PosSet.empty in
  let x = ref (fst start) and y = ref (snd start) in
  let direction = ref Up in

  let rec loop () =
    if !x >= 0 && !x < width && !y >= 0 && !y < height then (
      visited := PosSet.add (!x, !y) !visited;
      let dx, dy = get_direction_offset !direction in
      let rec find_next_move nx ny dir =
        if
          nx >= 0 && nx < width && ny >= 0 && ny < height
          && Hashtbl.mem obstacles (nx, ny)
        then
          let new_dir = next_direction dir in
          let ndx, ndy = get_direction_offset new_dir in
          find_next_move (!x + ndx) (!y + ndy) new_dir
        else (
          direction := dir;
          x := nx;
          y := ny)
      in
      find_next_move (!x + dx) (!y + dy) !direction;
      loop ())
  in
  loop ();
  !visited

(** [is_loop obstacles start width height] checks if guard gets stuck in a loop.
*)
let is_loop obstacles start width height =
  let visited = ref StateSet.empty in
  let x = ref (fst start) and y = ref (snd start) in
  let direction = ref Up in

  let rec loop () =
    if !x >= 0 && !x < width && !y >= 0 && !y < height then
      let state = ((!x, !y), !direction) in
      if StateSet.mem state !visited then true (* Loop detected *)
      else (
        visited := StateSet.add state !visited;
        let dx, dy = get_direction_offset !direction in
        let rec find_next_move nx ny dir =
          if
            nx >= 0 && nx < width && ny >= 0 && ny < height
            && Hashtbl.mem obstacles (nx, ny)
          then
            let new_dir = next_direction dir in
            let ndx, ndy = get_direction_offset new_dir in
            find_next_move (!x + ndx) (!y + ndy) new_dir
          else (
            direction := dir;
            x := nx;
            y := ny)
        in
        find_next_move (!x + dx) (!y + dy) !direction;
        loop ())
    else false (* Out of bounds - no loop *)
  in
  loop ()

let part1 map =
  let n = Array.length map in
  if Array.exists (fun row -> Array.length row <> n) map then
    failwith "Invalid map: not all rows have the same length";

  let obstacles, start, width, height = parse_obstacles_and_start map in
  let visited = get_guard_positions obstacles start width height in
  PosSet.cardinal visited

let part2 map =
  let n = Array.length map in
  Array.iter
    (fun row ->
      if Array.length row <> n then
        failwith "Invalid map: not all rows have the same length")
    map;

  let obstacles, start, width, height = parse_obstacles_and_start map in

  (* Get all positions the guard visits in the original path *)
  let guard_positions = get_guard_positions obstacles start width height in

  (* Remove the starting position from candidates *)
  let candidates = PosSet.remove start guard_positions in
  (* Count how many candidate positions create loops *)
  let loop_count = ref 0 in
  PosSet.iter
    (fun pos ->
      (* Add obstacle at this position *)
      let new_obstacles = Hashtbl.copy obstacles in
      Hashtbl.add new_obstacles pos ();
      if is_loop new_obstacles start width height then incr loop_count)
    candidates;
  !loop_count

(** [parse input] converts input string to 2D grid *)
let parse input =
  input |> String.split_on_char '\n'
  |> List.filter (fun s -> String.trim s <> "")
  |> List.map (fun row ->
      Array.of_list (List.init (String.length row) (String.get row)))
  |> Array.of_list

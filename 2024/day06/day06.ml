(* Day 6: Guard Gallivant. See day06.mli for the interface. *)

type direction =
  | Up
  | Right
  | Down
  | Left

type position = {
  row : int;
  col : int;
}

type guard = {
  position : position;
  facing : direction;
}

type move =
  | Step of guard
  | Turn of guard
  | Exit

type t = {
  rows : string array;
  width : int;
  start : guard;
}

let turn_right = function
  | Up -> Right
  | Right -> Down
  | Down -> Left
  | Left -> Up

let ahead { row; col } = function
  | Up -> { row = row - 1; col }
  | Right -> { row; col = col + 1 }
  | Down -> { row = row + 1; col }
  | Left -> { row; col = col - 1 }

(* One set type for both positions and guard states, via a unique int key
   for each: Int.compare is fast, where compare on records is generic. *)
module Keys = Set.Make (Int)

let cell map { row; col } = (row * map.width) + col

let state map { position; facing } =
  let d =
    match facing with
    | Up -> 0
    | Right -> 1
    | Down -> 2
    | Left -> 3
  in
  (4 * cell map position) + d

let inside map { row; col } =
  0 <= row && row < Array.length map.rows && 0 <= col && col < map.width

let is_obstruction map { row; col } = map.rows.(row).[col] = '#'

let next map ~blocked guard =
  let p = ahead guard.position guard.facing in
  if not (inside map p) then Exit
  else if blocked p then Turn { guard with facing = turn_right guard.facing }
  else Step { guard with position = p }

(* The guard's states from [guard] until she leaves the map *)
let rec patrol map ~blocked guard () =
  match next map ~blocked guard with
  | Exit -> Seq.Cons (guard, Seq.empty)
  | Step g | Turn g -> Seq.Cons (guard, patrol map ~blocked g)

(* Any loop contains a turn, so remembering the states after turns is enough *)
let rec loops map ~blocked turns guard =
  match next map ~blocked guard with
  | Exit -> false
  | Step g -> loops map ~blocked turns g
  | Turn g ->
      let s = state map g in
      Keys.mem s turns || loops map ~blocked (Keys.add s turns) g

let part1 map =
  patrol map ~blocked:(is_obstruction map) map.start
  |> Seq.fold_left
       (fun seen g -> Keys.add (cell map g.position) seen)
       Keys.empty
  |> Keys.cardinal

(* An obstruction at [p] cannot change the patrol before the guard first
   reaches [p], so each candidate is checked from the state just before. *)
let part2 map =
  let traps p guard =
    let blocked q = (q.row = p.row && q.col = p.col) || is_obstruction map q in
    loops map ~blocked Keys.empty guard
  in
  let step (seen, previous, count) g =
    let c = cell map g.position in
    if Keys.mem c seen then (seen, g, count)
    else
      ( Keys.add c seen,
        g,
        if traps g.position previous then count + 1 else count )
  in
  let start = (Keys.singleton (cell map map.start.position), map.start, 0) in
  let _, _, count =
    Seq.fold_left step start
      (patrol map ~blocked:(is_obstruction map) map.start)
  in
  count

let parse input =
  let facing = function
    | '^' -> Some Up
    | '>' -> Some Right
    | 'v' -> Some Down
    | '<' -> Some Left
    | _ -> None
  in
  let rows =
    String.split_on_char '\n' input
    |> List.map String.trim
    |> List.filter (fun line -> line <> "")
    |> Array.of_list
  in
  let guard_in row line =
    String.to_seqi line
    |> Seq.filter_map (fun (col, c) ->
        Option.map (fun f -> { position = { row; col }; facing = f }) (facing c))
  in
  let guards =
    Array.to_seqi rows
    |> Seq.concat_map (fun (r, l) -> guard_in r l)
    |> List.of_seq
  in
  match rows with
  | [||] -> Error "the map is empty"
  | _ -> (
      let width = String.length rows.(0) in
      if Array.exists (fun line -> String.length line <> width) rows then
        Error "the map is not rectangular"
      else
        match guards with
        | [ start ] -> Ok { rows; width; start }
        | [] -> Error "the map has no guard"
        | _ -> Error "the map has more than one guard")

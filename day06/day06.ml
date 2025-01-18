(** Solution for Day 6 of Advent of Code.
    Implements path finding in a 2D grid with directional movement. *)

(** Direction represents possible movements in the grid. *)
type direction = 
  | Up    (** Move upward *)
  | Right (** Move rightward *)
  | Down  (** Move downward *)
  | Left  (** Move leftward *)


(** [next_direction dir] returns the next direction in clockwise rotation. *)
let next_direction = function
  | Up -> Right
  | Right -> Down
  | Down -> Left
  | Left -> Up

let part1 map =
  let n = Array.length map in

  if Array.exists (fun row -> Array.length row <> n) map then
    failwith "Invalid map: not all rows have the same length";

  let forward (i, j) dir history =
      (**[forward (i,j) dir history] computes next position and updates history.
      Returns (new_i, new_j, new_dir, new_history) *)
    let next_pos = match dir with
      | Up -> if i >= 1 then Some (i - 1, j) else None
      | Right -> if j < n - 1 then Some (i, j + 1) else None
      | Down -> if i < n - 1 then Some (i + 1, j) else None
      | Left -> if j >= 1 then Some (i, j - 1) else None
    in
    match next_pos with
    | Some (ni, nj) ->
      let ni, nj, nd, nh =
        if map.(ni).(nj) = '#' then
          (* Hit wall - stay in place but rotate           
             Obstacle found: stay put + rotate right (Direction handling) *)
          (i, j, next_direction dir, history)
        else
          (* Move forward + track position (History tracking) *)
          (ni, nj, dir, (ni, nj) :: history)
      in
      (ni, nj, nd, nh)
    | None -> (i, j, dir, history) (* Out of bounds: stay put *)
  in

  let rec loop (i, j, dir, history) =
    (**[loop (i,j,dir,history)] follows path until cycle is detected.
        Returns number of unique positions visited*)
    let (ni, nj, nd, nh) = forward (i, j) dir history in
    (* Check if we're stuck (same position and direction) *)
    if (ni, nj) = (i, j) && dir = nd then
      (* Count unique positions *)
      history |> List.sort_uniq compare |> List.length
    else
      loop (ni, nj, nd, nh)
  in

  (* Find starting position marked with '^' *)
  let si, sj =
    List.find (fun (i, j) -> map.(i).(j) = '^')
      (List.concat (List.init n (fun i -> List.init n (fun j -> (i, j)))))
  in
  (* Start simulation with initial position and Up direction *)
  loop (si, sj, Up, [(si, sj)])

let part2 map =
  let n = Array.length map in
  Array.iter (fun row -> if Array.length row <> n then 
    failwith "Invalid map: not all rows have the same length") map;

  let find_loop map (i, j) dir =
    (**[find_loop map (i,j) dir] checks if blocking position creates loop.
        Returns true if loop is formed*)
    let history = Hashtbl.create 100 in
    let stack = Queue.create () in
    Queue.add (i, j, dir) stack;
    let rec loop found_loop =
      if found_loop || Queue.is_empty stack then found_loop
      else
        let (i, j, dir) = Queue.take stack in
        let next_pos = match dir with
          | Up -> if i >= 1 then Some (i - 1, j) else None
          | Right -> if j + 1 < n then Some (i, j + 1) else None
          | Down -> if i + 1 < n then Some (i + 1, j) else None
          | Left -> if j >= 1 then Some (i, j - 1) else None
        in
        match next_pos with
        | Some (ni, nj) ->
          let ni, nj, nd =
            if map.(ni).(nj) = '#' then
              (i, j, next_direction dir)
            else
              (ni, nj, dir)
          in
          if Hashtbl.mem history (ni, nj, nd) then
            loop true
          else begin
            Hashtbl.add history (ni, nj, nd) ();
            Queue.add (ni, nj, nd) stack;
            loop false
          end
        | None -> loop false
    in
    loop false
  in

  (* Find starting position marked with '^' *)
  let si, sj =
    List.find (fun (i, j) -> map.(i).(j) = '^')
      (List.concat (List.init n (fun i -> List.init n (fun j -> (i, j)))))
  in
  
  (* Count positions that create loops when blocked *)
  List.length (
    List.filter (fun (i, j) ->
      if map.(i).(j) = '.' then
        let row = Array.copy map.(i) in
        row.(j) <- '#';
        let new_map = Array.copy map in
        new_map.(i) <- row;
        find_loop new_map (si, sj) Up
      else
        false
    ) (List.concat (List.init n (fun i -> List.init n (fun j -> (i, j)))))
  )

(** [parse input] converts input string to 2D grid *)
let parse input =
  input
  |> String.split_on_char '\n'
  |> List.filter (fun s -> String.trim s <> "")
  |> List.map (fun row -> Array.of_list (List.init (String.length row) (String.get row)))
  |> Array.of_list

(** Main entry point *)
let () =
  let input = In_channel.input_all In_channel.stdin |> parse in

  let start_time = Unix.gettimeofday () in

  let part1_result = part1 input in
  Printf.printf "Part 1: %d\n" part1_result;

  let part2_result = part2 input in
  Printf.printf "Part 2: %d\n" part2_result;

  let end_time = Unix.gettimeofday () in
  Printf.printf "Elapsed time: %f seconds\n" (end_time -. start_time)
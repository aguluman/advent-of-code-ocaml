open Stdio
open Domainslib

type direction = 
  | Up
  | Right
  | Down
  | Left

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
          (i, j, next_direction dir, history)
        else
          (ni, nj, dir, (ni, nj) :: history)
      in
      (ni, nj, nd, nh)
    | None -> (i, j, dir, history)
  in

  let rec loop (i, j, dir, history) =
    let (ni, nj, nd, nh) = forward (i, j) dir history in
    if (ni, nj) = (i, j) && dir = nd then
      List.length (List.sort_uniq compare history)
    else
      loop (ni, nj, nd, nh)
  in

  let si, sj =
    List.find (fun (i, j) -> map.(i).(j) = '^')
      (List.concat (List.init n (fun i -> List.init n (fun j -> (i, j)))))
  in
  loop (si, sj, Up, [(si, sj)])

let part2 map =
  let n = Array.length map in
  Array.iter (fun row -> if Array.length row <> n then failwith "Invalid map: not all rows have the same length") map;

  let find_loop map (i, j) dir =
    let history = Hashtbl.create 100 in
    let stack = Queue.create () in
    Queue.add (i, j, dir) stack;
    let rec loop () =
      if Queue.is_empty stack then false
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
            true
          else begin
            Hashtbl.add history (ni, nj, nd) ();
            Queue.add (ni, nj, nd) stack;
            loop ()
          end
        | None -> loop ()
    in
    loop ()
  in

  let si, sj =
    List.init n (fun i -> List.init n (fun j -> (i, j)))
    |> List.concat
    |> List.find (fun (i, j) -> map.(i).(j) = '^')
  in

  let pool = (Task.setup_pool ~num_domains:(Domain.recommended_domain_count()))() in
  let results =
    Task.parallel_for_reduce pool ~chunk_size:1 ~start:0 ~finish:(n * n - 1)
      ~body:(fun idx ->
        let i = idx / n in
        let j = idx mod n in
        if map.(i).(j) = '.' then
          let row = Array.copy map.(i) in
          row.(j) <- '#';
          let new_map = Array.copy map in
          new_map.(i) <- row;
          if find_loop new_map (si, sj) Up then 1 else 0
        else
          0)
      (+)  (* Use built-in addition as reducer *)
      0    (* Start with 0 as initial value *)
  in
  Task.teardown_pool pool;
  results


let parse input =
  input
  |> String.split_on_char '\n'
  |> List.filter (fun s -> String.trim s <> "")
  |> List.map (fun row -> Array.of_list (List.init (String.length row) (String.get row)))
  |> Array.of_list

let () =
  let input = In_channel.input_all In_channel.stdin |> parse in

  let start_time = Unix.gettimeofday () in

  let part1_result = part1 input in
  printf "Part 1: %d\n" part1_result;

  let part2_result = part2 input in
  printf "Part 2: %d\n" part2_result;

  let end_time = Unix.gettimeofday () in
  printf "Elapsed time: %f seconds\n" (end_time -. start_time)
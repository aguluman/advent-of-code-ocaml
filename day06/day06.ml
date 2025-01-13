type dir = U | R | D | L

let next_dir = function
  | U -> R
  | R -> D
  | D -> L
  | L -> U

let is_in_bounds n (i, j) = 
  i >= 0 && i < n && j >= 0 && j < n

let is_blocked map n (i, j) = function
  | U -> i <= 0 || map.(i-1).(j) = '#'
  | R -> j >= n-1 || map.(i).(j+1) = '#'
  | D -> i >= n-1 || map.(i+1).(j) = '#'
  | L -> j <= 0 || map.(i).(j-1) = '#'

let move (i, j) = function
  | U -> (i - 1, j)
  | R -> (i, j + 1)
  | D -> (i + 1, j)
  | L -> (i, j - 1)

let find_start map n =
  let rec aux i j =
    if i >= n then failwith "No start position found"
    else if j >= n then aux (i + 1) 0
    else if map.(i).(j) = '^' then (i, j)
    else aux i (j + 1)
  in aux 0 0

let part1 map =
  let n = Array.length map in
  let rec walk pos dir visited =
    if not (is_in_bounds n pos) then
      List.length visited
    else
      let new_visited = pos :: visited in
      if is_blocked map n pos dir then
        walk pos (next_dir dir) new_visited
      else
        let next_pos = move pos dir in
        walk next_pos dir new_visited
  in
  let start = find_start map n in
  walk start U []

let part2 map =
  let n = Array.length map in
  
  let rec find_loop map (i, j) dir history =
    let next_pos = match dir with
      | U -> if i >= 1 then Some(i - 1, j) else None
      | R -> if j + 1 < n then Some(i, j + 1) else None
      | D -> if i + 1 < n then Some(i + 1, j) else None
      | L -> if j >= 1 then Some(i, j - 1) else None
    in
    match next_pos with
    | None -> false
    | Some(ni, nj) ->
        let (ni, nj, nd) =
          if map.(ni).(nj) = '#' then
            (i, j, next_dir dir)
          else
            (ni, nj, dir)
        in
        if List.mem (ni, nj, nd) history then
          true
        else
          find_loop map (ni, nj) nd ((ni, nj, nd) :: history)
  in
  
  let si, sj = find_start map n in
  let count = ref 0 in
  
  for i = 0 to n-1 do
    for j = 0 to n-1 do
      if map.(i).(j) = '.' then begin
        let new_map = Array.map Array.copy map in
        new_map.(i).(j) <- '#';
        if find_loop new_map (si, sj) U [(si, sj, U)] then
          incr count
      end
    done
  done;
  !count

let parse input =
  let lines = String.split_on_char '\n' input in
  Array.of_list (List.map (fun s -> Array.of_seq (String.to_seq s)) lines)

let () =
  let input = read_line () in
  let map = parse input in
  Printf.printf "Part 1: %d\n" (part1 map);
  Printf.printf "Part 2: %d\n" (part2 map)
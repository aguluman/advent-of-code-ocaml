(* Helper function to generate ranges *)
let range start end_val =
  let rec aux start end_val acc =
    if start > end_val then List.rev acc
    else aux (start + 1) end_val (start :: acc)
  in aux start end_val []

(* Generate all pairs from two lists *)
let list_allpairs xs ys =
  List.concat (List.map (fun x -> List.map (fun y -> (x,y)) ys) xs)

(* Convert F#'s List.unfold to OCaml *)
let unfold f state =
  let rec aux acc state =
    match f state with
    | None -> List.rev acc
    | Some (value, newState) -> aux (value :: acc) newState
  in aux [] state

let antinodePositions n (i, j) (i', j') =
  if (i, j) = (i', j') then []
  else
    let di, dj = (i' - i, j' - j) in
    unfold (fun (i', j') ->
      let ni, nj = (i' + di, j' + dj) in
      if 0 <= ni && ni < n && 0 <= nj && nj < n then
        Some((ni, nj), (ni, nj))
      else None
    ) (i', j')

let solve map mapping =
  let n = Array.length map in
  assert (Array.for_all (fun row -> Array.length row = n) map);
  
  let chars = 
    List.append (range (Char.code '0') (Char.code '9'))
    (List.append 
       (range (Char.code 'a') (Char.code 'z'))
       (range (Char.code 'A') (Char.code 'Z')))
    |> List.map Char.chr
  in
  
  let positions c =
    list_allpairs (range 0 (n-1)) (range 0 (n-1))
    |> List.filter (fun (i,j) -> map.(i).(j) = c)
  in
  
  chars
  |> List.map (fun c ->
      let pos = positions c in
      let new_pos = 
        list_allpairs pos pos
        |> List.concat_map (fun ((i,j), (i',j')) -> 
            mapping n (i,j) (i',j'))
      in
      List.sort_uniq compare new_pos)
  |> List.concat
  |> List.sort_uniq compare
  |> List.length

let part1 map =
  solve map (fun n (i,j) (i',j') ->
    match antinodePositions n (i,j) (i',j') with
    | [] -> []
    | h :: _ -> [h])

let part2 map =
  solve map (fun n (i,j) (i',j') ->
    (i',j') :: antinodePositions n (i,j) (i',j'))


(** [parse input] converts input string to 2D grid *)
let parse input =
  input
  |> String.split_on_char '\n'
  |> List.filter (fun s -> String.trim s <> "")
  |> List.map (fun row -> 
    Array.of_list (List.init (String.length row) (String.get row)))
  |> Array.of_list


let () =
  let input = 
    In_channel.input_all In_channel.stdin 
    |> String.trim
  in
  let map = parse input in

  let start_time = Unix.gettimeofday () in

  map |> part1 |> Printf.printf "Part 1: %d\n";

  map |> part2 |> Printf.printf "Part 2: %d\n";

  let end_time = Unix.gettimeofday () in
  let elapsed = end_time -. start_time in
  Printf.printf "Elapsed time: %.4f seconds\n" elapsed
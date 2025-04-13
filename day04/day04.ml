(** Get character at position (i,j) in the grid, returning None if out of bounds *)
let getC i j chars =
  if 0 <= i && i < Array.length chars && 0 <= j && j < Array.length chars.(i) then
    Some(chars.(i).(j))
  else
    None

(** Part 1: Count the number of "XMAS" patterns in the grid in all 8 directions *)
let part1 chars =
  let getC i j = getC i j chars in
  
  chars
  |> Array.mapi (fun i row ->
      row
      |> Array.mapi (fun j _ ->
          let north = List.init 4 (fun d -> getC (i - d) j) in
          let northWest = List.init 4 (fun d -> getC (i - d) (j - d)) in
          let west = List.init 4 (fun d -> getC i (j - d)) in
          let southWest = List.init 4 (fun d -> getC (i + d) (j - d)) in
          let south = List.init 4 (fun d -> getC (i + d) j) in
          let southEast = List.init 4 (fun d -> getC (i + d) (j + d)) in
          let east = List.init 4 (fun d -> getC i (j + d)) in
          let northEast = List.init 4 (fun d -> getC (i - d) (j + d)) in

          [ north; northWest; west; southWest; south; southEast; east; northEast ]
          |> List.filter (fun word -> word = [ Some('X'); Some('M'); Some('A'); Some('S') ])
          |> List.length)
      |> Array.fold_left (+) 0)
  |> Array.fold_left (+) 0

(** Part 2: Count characters forming an X pattern with "MAS" on opposite sides *)
let part2 chars =
  let getC i j = getC i j chars in
  
  let isMAS (c1, c2, c3) =
    c1 = Some('M') && c2 = Some('A') && c3 = Some('S')
  in
  
  chars
  |> Array.mapi (fun i row ->
      row
      |> Array.mapi (fun j c ->
          let ne = getC (i - 1) (j + 1) in
          let nw = getC (i - 1) (j - 1) in
          let se = getC (i + 1) (j + 1) in
          let sw = getC (i + 1) (j - 1) in

          if (isMAS (ne, Some(c), sw) || isMAS (sw, Some(c), ne))
             && (isMAS (nw, Some(c), se) || isMAS (se, Some(c), nw)) then
            1
          else
            0)
      |> Array.fold_left (+) 0)
  |> Array.fold_left (+) 0

(** Parse the input string into a 2D character array *)
let parse input =
  String.split_on_char '\n' input
  |> List.filter (fun s -> String.trim s <> "")
  |> List.map (fun s -> String.to_seq s |> Array.of_seq)
  |> Array.of_list
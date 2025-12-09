(** Day 09: Movie Theater

    [Brief description of the problem and what this module solves]

    {2 Problem Summary:}
    - {b Part 1:} [Description of part 1]
    - {b Part 2:} [Description of part 2]

    See details at:
    {{:https://adventofcode.com/2025/day/9} Advent of Code 2025, Day 09} *)

type point = {
  x : int;
  y : int;
}

(** Helper function to parse input string into a structured data format

    @param input Raw input string from the puzzle
    @return Parsed data structure *)
let parse input =
  input |> String.split_on_char '\n'
  |> List.filter (fun line -> String.trim line <> "")
  |> List.map (fun line ->
      match String.split_on_char ',' line with
      | [ x; y ] ->
          {
            x = int_of_string (String.trim x);
            y = int_of_string (String.trim y);
          }
      | _ -> failwith "Invalid coordinate format")
  |> Array.of_list

let rectangle_area p1 p2 = (abs (p1.x - p2.x) + 1) * (abs (p1.y - p2.y) + 1)

(** [part1 input] solves part 1 of the challenge

    @param input Raw input string from the puzzle
    @return Solution for part 1 *)
let part1 input =
  let reds = parse input in
  let n = Array.length reds in
  let best = ref 0 in
  for i = 0 to n - 2 do
    for j = i + 1 to n - 1 do
      let area = rectangle_area reds.(i) reds.(j) in
      if area > !best then best := area
    done
  done;
  !best

let point_on_segment p a b =
  if a.x = b.x then p.x = a.x && p.y >= min a.y b.y && p.y <= max a.y b.y
  else if a.y = b.y then p.y = a.y && p.x >= min a.x b.x && p.x <= max a.x b.x
  else false

let point_on_polygon poly p =
  let n = Array.length poly in
  let rec loop i =
    if i = n then false
    else
      let a = poly.(i) in
      let b = poly.((i + 1) mod n) in
      if point_on_segment p a b then true else loop (i + 1)
  in
  loop 0

let point_in_polygon poly p =
  if point_on_polygon poly p then true
  else
    let crossings = ref 0 in
    let n = Array.length poly in
    for i = 0 to n - 1 do
      let a = poly.(i) and b = poly.((i + 1) mod n) in
      let ay, by = (a.y, b.y) in
      let ax, bx = (a.x, b.x) in
      if ay > p.y <> (by > p.y) then
        let xinters =
          float_of_int ax
          +. float_of_int (bx - ax)
             *. float_of_int (p.y - ay)
             /. float_of_int (by - ay)
        in
        if float_of_int p.x < xinters then incr crossings
    done;
    !crossings land 1 = 1

let rectangle_inside poly p1 p2 =
  let x1 = min p1.x p2.x and x2 = max p1.x p2.x in
  let y1 = min p1.y p2.y and y2 = max p1.y p2.y in
  let mid_x = (x1 + x2) / 2 and mid_y = (y1 + y2) / 2 in

  (* 1) all corners inside or on boundary *)
  let corners =
    [|
      { x = x1; y = y1 };
      { x = x1; y = y2 };
      { x = x2; y = y1 };
      { x = x2; y = y2 };
    |]
  in
  if not (Array.for_all (point_in_polygon poly) corners) then false
  else
    (* 2) no polygon edge crosses the rectangle's open interior *)
    let n = Array.length poly in
    let crosses = ref false in
    for i = 0 to n - 1 do
      let a = poly.(i) and b = poly.((i + 1) mod n) in
      if a.x = b.x then (
        (* vertical edge *)
        let ex = a.x in
        let ylo = min a.y b.y and yhi = max a.y b.y in
        if ex > x1 && ex < x2 && yhi > y1 && ylo < y2 then crosses := true)
      else if a.y = b.y then (
        (* horizontal edge *)
        let ey = a.y in
        let xlo = min a.x b.x and xhi = max a.x b.x in
        if ey > y1 && ey < y2 && xhi > x1 && xlo < x2 then crosses := true)
      else ()
    done;
    if !crosses then false
    else
      (* 3) sample along rectangle edges at polygon vertex coordinates *)
      let xs =
        poly |> Array.to_list
        |> List.filter_map (fun v ->
            if v.x >= x1 && v.x <= x2 then Some v.x else None)
        |> List.append [ x1; x2; mid_x ]
        |> List.sort_uniq compare
      in
      let ys =
        poly |> Array.to_list
        |> List.filter_map (fun v ->
            if v.y >= y1 && v.y <= y2 then Some v.y else None)
        |> List.append [ y1; y2; mid_y ]
        |> List.sort_uniq compare
      in
      let all_ok =
        (* sample left/right and middle verticals at each y *)
        List.for_all
          (fun y ->
            point_in_polygon poly { x = x1; y }
            && point_in_polygon poly { x = x2; y }
            && point_in_polygon poly { x = mid_x; y })
          ys
        &&
        (* sample top/bottom and middle horizontals at each x *)
        List.for_all
          (fun x ->
            point_in_polygon poly { x; y = y1 }
            && point_in_polygon poly { x; y = y2 }
            && point_in_polygon poly { x; y = mid_y })
          xs
      in
      all_ok

(** [part2 input] solves part 2 of the challenge

    @param input Raw input string from the puzzle
    @return Solution for part 2 *)
let part2 input =
  let reds = parse input in
  let n = Array.length reds in
  let best = ref Int64.zero in
  for i = 0 to n - 2 do
    for j = i + 1 to n - 1 do
      if rectangle_inside reds reds.(i) reds.(j) then
        let dx = Int64.of_int (abs (reds.(i).x - reds.(j).x) + 1) in
        let dy = Int64.of_int (abs (reds.(i).y - reds.(j).y) + 1) in
        let area = Int64.mul dx dy in
        if area > !best then best := area
    done
  done;
  !best

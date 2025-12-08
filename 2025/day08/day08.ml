(** Day 08: Playground

    Union-Find problem to connect junction boxes and form circuits.

    {2 Problem Summary:}
    - {b Part 1:} Connect 1000 shortest pairs, multiply 3 largest circuit sizes
    - {b Part 2:} Connect the closest unconnected pairs until all junction boxes
      are in one circuit. Return the product of the X coordinates of the last
      pair that caused the final union.

    See details at:
    {{:https://adventofcode.com/2025/day/8} Advent of Code 2025, Day 08} *)

(** Min-heap module for edges *)
module EdgeHeap = struct
  type edge = int * int * int (* distance, idx_i, idx_j *)

  (* Simple binary heap implementation *)
  type t = {
    mutable data : edge array;
    mutable size : int;
  }

  let create capacity = { data = Array.make capacity (0, 0, 0); size = 0 }
  let parent i = (i - 1) / 2
  let left i = (2 * i) + 1
  let right i = (2 * i) + 2

  let swap heap i j =
    let tmp = heap.data.(i) in
    heap.data.(i) <- heap.data.(j);
    heap.data.(j) <- tmp

  let rec sift_up heap i =
    if i > 0 then
      let p = parent i in
      let d1, _, _ = heap.data.(i) in
      let d2, _, _ = heap.data.(p) in
      if d1 < d2 then (
        swap heap i p;
        sift_up heap p)

  let rec sift_down heap i =
    let l = left i in
    let r = right i in
    let smallest = ref i in

    if l < heap.size then (
      let d1, _, _ = heap.data.(l) in
      let d2, _, _ = heap.data.(!smallest) in
      if d1 < d2 then smallest := l;

      if r < heap.size then (
        let d1, _, _ = heap.data.(r) in
        let d2, _, _ = heap.data.(!smallest) in
        if d1 < d2 then smallest := r;

        if !smallest <> i then (
          swap heap i !smallest;
          sift_down heap !smallest)))

  let add heap edge =
    if heap.size >= Array.length heap.data then failwith "Heap full";
    heap.data.(heap.size) <- edge;
    sift_up heap heap.size;
    heap.size <- heap.size + 1

  let pop_min heap =
    if heap.size = 0 then None
    else
      let min_elem = heap.data.(0) in
      heap.size <- heap.size - 1;
      heap.data.(0) <- heap.data.(heap.size);
      if heap.size > 0 then sift_down heap 0;
      Some min_elem
end

(** 3D coordinate type *)
type coord = {
  x : int;
  y : int;
  z : int;
}

(** Calculate squared Euclidean distance *)
let distance_squared c1 c2 =
  let dx = c1.x - c2.x in
  let dy = c1.y - c2.y in
  let dz = c1.z - c2.z in
  (dx * dx) + (dy * dy) + (dz * dz)

(** Union-Find data structure *)
type union_find = {
  mutable parent : int array;
  mutable rank : int array;
}

let make_union_find n =
  { parent = Array.init n (fun i -> i); rank = Array.make n 0 }

let rec find uf x =
  if uf.parent.(x) <> x then (
    uf.parent.(x) <- find uf uf.parent.(x);
    uf.parent.(x))
  else x

let union uf x y =
  let root_x = find uf x in
  let root_y = find uf y in
  if root_x = root_y then false
  else (
    if uf.rank.(root_x) < uf.rank.(root_y) then uf.parent.(root_x) <- root_y
    else if uf.rank.(root_x) > uf.rank.(root_y) then
      uf.parent.(root_y) <- root_x
    else (
      uf.parent.(root_y) <- root_x;
      uf.rank.(root_x) <- uf.rank.(root_x) + 1);
    true)

(** Parse a line into a coordinate *)
let parse_line line =
  match String.split_on_char ',' line with
  | [ x; y; z ] ->
      { x = int_of_string x; y = int_of_string y; z = int_of_string z }
  | _ -> failwith "Invalid coordinate format"

(** Helper function to parse input string into a structured data format

    @param input Raw input string from the puzzle
    @return Array of 3D coordinates *)
let parse input =
  input |> String.split_on_char '\n'
  |> List.filter (fun line -> String.trim line <> "")
  |> List.map parse_line |> Array.of_list

(** Count circuit sizes *)
let count_circuit_sizes uf n =
  for i = 0 to n - 1 do
    ignore (find uf i)
  done;
  let size_arr = Array.make n 0 in
  for i = 0 to n - 1 do
    let root = uf.parent.(i) in
    size_arr.(root) <- size_arr.(root) + 1
  done;
  let sizes = ref [] in
  for i = 0 to n - 1 do
    if size_arr.(i) > 0 then sizes := size_arr.(i) :: !sizes
  done;
  !sizes

(** [part1 input] solves part 1 of the challenge

    @param input Raw input string from the puzzle
    @return Solution for part 1 *)
let part1 input =
  let coords = parse input in
  let n = Array.length coords in
  let uf = make_union_find n in

  (* Build heap with all edges *)
  let num_edges = n * (n - 1) / 2 in
  let heap = EdgeHeap.create num_edges in

  for i = 0 to n - 2 do
    for j = i + 1 to n - 1 do
      let dist = distance_squared coords.(i) coords.(j) in
      EdgeHeap.add heap (dist, i, j)
    done
  done;

  (* Extract and process exactly num_connections edges *)
  let num_connections = if n = 20 then 10 else 1000 in
  for _ = 1 to num_connections do
    match EdgeHeap.pop_min heap with
    | Some (_, i, j) -> ignore (union uf i j)
    | None -> ()
  done;

  (* Count and multiply top 3 circuit sizes *)
  let sizes = count_circuit_sizes uf n in
  let sorted_sizes = List.sort (fun a b -> compare b a) sizes in
  match sorted_sizes with
  | a :: b :: c :: _ -> a * b * c
  | _ -> 0

(** [part2 input] solves part 2 of the challenge

    @param input Raw input string from the puzzle
    @return Solution for part 2 *)
let part2 input =
  let coords = parse input in
  let n = Array.length coords in
  let uf = make_union_find n in

  let num_edges = n * (n - 1) / 2 in
  let heap = EdgeHeap.create num_edges in

  for i = 0 to n - 2 do
    for j = i + 1 to n - 1 do
      let dist = distance_squared coords.(i) coords.(j) in
      EdgeHeap.add heap (dist, i, j)
    done
  done;

  (* Process until single component (n-1 successful unions) *)
  let remaining = ref n in
  let result = ref Int64.zero in

  while !remaining > 1 do
    match EdgeHeap.pop_min heap with
    | Some (_, i, j) ->
        if union uf i j then (
          remaining := !remaining - 1;
          if !remaining = 1 then
            result :=
              Int64.mul (Int64.of_int coords.(i).x) (Int64.of_int coords.(j).x))
    | None -> remaining := 1
  done;

  !result

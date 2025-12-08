(** Day 08: Playground

    Union-Find problem to connect junction boxes and form circuits.

    {2 Problem Summary:}
    - {b Part 1:} Connect 1000 shortest pairs, multiply 3 largest circuit sizes
    - {b Part 2:} Connect the closest unconnected pairs until all junction boxes
      are in one circuit. Return the product of the X coordinates of the last
      pair that caused the final union.

    See details at:
    {{:https://adventofcode.com/2025/day/8} Advent of Code 2025, Day 08} *)

(** 3D coordinate type *)
type coord = {
  x : int;
  y : int;
  z : int;
}

(** Calculate squared Euclidean distance between two coordinates (avoiding sqrt
    for performance) *)
let distance_squared c1 c2 =
  let dx = c1.x - c2.x in
  let dy = c1.y - c2.y in
  let dz = c1.z - c2.z in
  (dx * dx) + (dy * dy) + (dz * dz)

(** Union-Find data structure with path compression parent: array where
    parent[i] is the parent of node i rank: array for union by rank optimization
*)
type union_find = {
  mutable parent : int array;
  mutable rank : int array;
}

(** Create a new Union-Find structure with n elements *)
let make_union_find n =
  { parent = Array.init n (fun i -> i); rank = Array.make n 0 }

(** Find the root of element x with path compression *)
let rec find uf x =
  if uf.parent.(x) <> x then (
    uf.parent.(x) <- find uf uf.parent.(x);
    uf.parent.(x))
  else x

(** Union two sets containing x and y, return true if they were in different
    sets *)
let union uf x y =
  let root_x = find uf x in
  let root_y = find uf y in
  if root_x = root_y then false
  else (
    (* Union by rank *)
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

(** Generate all pairs of indices with their distances *)
let generate_edges coords =
  let n = Array.length coords in
  let edges = ref [] in
  for i = 0 to n - 2 do
    for j = i + 1 to n - 1 do
      let dist = distance_squared coords.(i) coords.(j) in
      edges := (dist, i, j) :: !edges
    done
  done;
  !edges

(** Count the size of each circuit (connected component) *)
let count_circuit_sizes uf n =
  (* First, ensure all paths are compressed *)
  for i = 0 to n - 1 do
    ignore (find uf i)
  done;
  (* Count elements in each circuit *)
  let size_map = Hashtbl.create n in
  for i = 0 to n - 1 do
    let root = uf.parent.(i) in
    let current_size = try Hashtbl.find size_map root with Not_found -> 0 in
    Hashtbl.replace size_map root (current_size + 1)
  done;
  (* Convert to list of sizes *)
  Hashtbl.fold (fun _ size acc -> size :: acc) size_map []

(** [part1 input] solves part 1 of the challenge

    @param input Raw input string from the puzzle
    @return Solution for part 1 *)
let part1 input =
  let coords = parse input in
  let n = Array.length coords in
  let uf = make_union_find n in

  (* Generate all edges and sort by distance *)
  let edges = generate_edges coords in
  let sorted_edges =
    List.sort (fun (d1, _, _) (d2, _, _) -> compare d1 d2) edges
  in

  (* Connect the 1000 shortest pairs (or 10 for example) *)
  let num_connections = if n = 20 then 10 else 1000 in
  let rec connect edges_list count =
    if count >= num_connections then ()
    else
      match edges_list with
      | [] -> ()
      | (_, i, j) :: rest ->
          ignore (union uf i j);
          connect rest (count + 1)
  in
  connect sorted_edges 0;

  (* Count circuit sizes *)
  let sizes = count_circuit_sizes uf n in

  (* Sort sizes in descending order and take top 3 *)
  let top3 =
    sizes
    |> List.sort (fun a b -> compare b a)
    |> List.to_seq |> Seq.take 3 |> List.of_seq
  in

  (* Multiply the three largest *)
  List.fold_left ( * ) 1 top3

(** [part2 input] solves part 2 of the challenge

    @param input Raw input string from the puzzle
    @return Solution for part 2 *)
let part2 input =
  let coords = parse input in
  let n = Array.length coords in
  let uf = make_union_find n in

  (* Generate and sort all edges by distance (ascending) *)
  let edges = generate_edges coords in
  let sorted_edges =
    List.sort (fun (d1, _, _) (d2, _, _) -> compare d1 d2) edges
  in

  (* Track remaining components; initially each node is its own component *)
  let remaining = ref n in

  (* Process edges until there's a single component; when a union reduces the
     components count to 1, return the product of the X coordinates. *)
  let rec process = function
    | [] -> Int64.zero
    | (_, i, j) :: rest ->
        if union uf i j then (
          remaining := !remaining - 1;
          if !remaining = 1 then
            Int64.mul (Int64.of_int coords.(i).x) (Int64.of_int coords.(j).x)
          else process rest)
        else process rest
  in
  process sorted_edges

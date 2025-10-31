module IntMap = Map.Make (Int)

(* Parse function to convert string input to a list of integer pairs *)
let parse input =
  String.split_on_char '\n' input
  |> List.filter (fun sentence -> String.length sentence > 0)
  |> List.map (fun line ->
      let parts =
        line |> String.split_on_char ' '
        |> List.filter (fun sentence -> String.length sentence > 0)
      in
      (int_of_string (List.nth parts 0), int_of_string (List.nth parts 1)))

(* Part 1: Sum absolute differences between sorted lists *)
let part1 location_ids =
  let left, right = List.split location_ids in

  let sorted_left = List.sort compare left in
  let sorted_right = List.sort compare right in

  List.combine sorted_left sorted_right
  |> List.fold_left (fun acc (l, r) -> acc + abs (l - r)) 0

(* Part 2: Calculate weighted sum based on frequency counter *)
let part2 location_ids =
  let left, right = List.split location_ids in

  (* Create a frequency counter map for right elements *)
  let counter =
    List.fold_left
      (fun acc r ->
        let count = try IntMap.find r acc with Not_found -> 0 in
        IntMap.add r (count + 1) acc)
      IntMap.empty right
  in

  (* Sum left elements multiplied by their frequency in right *)
  List.fold_left
    (fun acc l ->
      let c = try IntMap.find l counter with Not_found -> 0 in
      Int64.add acc (Int64.mul (Int64.of_int l) (Int64.of_int c)))
    Int64.zero left

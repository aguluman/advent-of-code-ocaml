let pairwise lst =
  let rec aux = function
    | x :: y :: rest -> (x, y) :: aux (y :: rest)
    | _ -> []
  in
  aux lst

let isSafe report =
  let pairs = pairwise report in
  let inc (x, y) = x > y in
  let dec (x, y) = x < y in
  let geq1 (x, y) = abs (x - y) >= 1 in
  let leq3 (x, y) = abs (x - y) <= 3 in

  (List.for_all inc pairs || List.for_all dec pairs)
  && List.for_all (fun p -> geq1 p && leq3 p) pairs

let remove_at n lst =
  let rec aux i = function
    | [] -> []
    | h :: t -> if i = n then t else h :: aux (i + 1) t
  in
  aux 0 lst

let part1 reports = reports |> List.filter isSafe |> List.length

let part2 reports =
  reports
  |> List.filter (fun report ->
      List.init (List.length report) (fun x -> x)
      |> List.exists (fun i -> isSafe (remove_at i report)))
  |> List.length

let parse input =
  input |> String.split_on_char '\n'
  |> List.filter (fun line -> String.trim line <> "")
  |> List.map (fun line ->
      line |> String.split_on_char ' '
      |> List.filter (fun s -> String.trim s <> "")
      |> List.map int_of_string)

(* To run this day on an input file, from the repository root:
     make run-day DAY=02 INPUT=path/to/input.txt
   or from this directory:
     dune exec ./test.exe < path/to/input.txt *)

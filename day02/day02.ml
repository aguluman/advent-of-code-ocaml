let pairwise lst =
  let rec aux = function
    | x :: y :: rest -> (x, y) :: aux (y :: rest)
    | _ -> []
  in aux lst

let isSafe report =
    let pairs = pairwise report in
      let inc (x, y) = x > y in
      let dec (x, y) = x < y in
      let geq1 (x, y) = abs (x - y) >= 1 in
      let leq3 (x, y) = abs (x - y) <= 3 in

    (List.for_all inc pairs || List.for_all dec pairs) &&
    List.for_all (fun p -> geq1 p && leq3 p) pairs

let remove_at n lst =
  let rec aux i = function
    | [] -> []
    | h :: t -> if i = n then t else h :: aux (i+1) t
  in aux 0 lst

let part1 reports =
  reports |> List.filter isSafe |> List.length

let part2 reports =
  reports
  |> List.filter (fun report ->
       List.init (List.length report) (fun x -> x)
       |> List.exists (fun i -> isSafe (remove_at i report))
     )
  |> List.length

let parse input =
  input
  |> String.split_on_char '\n'
  |> List.map (fun line ->
       line
       |> String.split_on_char ' '
       |> List.map int_of_string
     )

let read_all_lines () =
  let rec read_lines acc =
    try
      let line = read_line () in
      read_lines (line :: acc)
    with
    End_of_file -> List.rev acc
  in
  read_lines []
  |> List.rev
  |> String.concat "\n"

let () =
  read_all_lines ()
  |> parse
  |> fun reports ->
    Printf.printf "Part 1: %d\n" (part1 reports);
    Printf.printf "Part 2: %d\n" (part2 reports)

(*
  How to run am Ocam file in the console and read from an input, 
  1] ocamlc -o test.exe test.ml
  2] type "C:\Users\chukw\Downloads\input.txt" | .\test.exe
*)
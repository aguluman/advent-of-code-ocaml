let part1 equations =
  let rec collect_result a acc =
    match a with
    | [] -> acc
    | h :: t ->
        let new_acc =
          List.fold_left
            (fun acc s -> 
              (Int64.add s h) :: (Int64.mul s h) :: acc)
            []
            acc
        in
        collect_result t new_acc
  in
  equations
  |> List.filter (fun (s, a) ->
         let a = List.of_seq a in
         let result =
           collect_result (List.tl a) [ List.hd a ]
           |> List.exists (fun x -> Int64.equal x s)
         in
         result)
  |> List.fold_left (fun acc (s, _) -> Int64.add acc s) Int64.zero

let part2 equations =
  let rec collect_result a acc =
    match a with
    | [] -> acc
    | h :: t ->
        let new_acc =
          List.fold_left
            (fun acc s ->
              (Int64.add s h)
              :: (Int64.mul s h)
              :: (Int64.of_string (Int64.to_string s ^ Int64.to_string h))
              :: acc)
            []
            acc
        in
        collect_result t new_acc
  in
  equations
  |> List.filter (fun (s, a) ->
         let a = List.of_seq a in
         collect_result (List.tl a) [ List.hd a ]
         |> List.exists (fun x -> Int64.equal x s))
  |> List.fold_left (fun acc (s, _) -> Int64.add acc s) Int64.zero

let parse input =
  String.split_on_char '\n' input
  |> List.map (fun line ->
         match String.split_on_char ':' line with
         | [ num; values ] ->
             ( Int64.of_string (String.trim num),
               String.split_on_char ' ' (String.trim values)
               |> List.filter (fun s -> s <> "")
               |> List.map Int64.of_string
               |> List.to_seq )
         | _ -> failwith "Invalid input format")


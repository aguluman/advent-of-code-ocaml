let part1 (schematics : char array array list) =
  (* Get height and width from the first schematic *)
  let h = Array.length (List.hd schematics) in
  let w = Array.length (List.hd schematics).(0) in

  (* Extract locks *)
  let locks = 
    List.filter (fun s -> Array.for_all ((=) '#') s.(0)) schematics
    |> List.map (fun s -> 
      List.init w (fun j -> 
        List.fold_left (fun acc i -> acc + if s.(i).(j) = '.' then 0 else 1) 0 (List.init h Fun.id)
      )
    )
  in

  (* Extract keys *)
  let keys = 
    List.filter (fun s -> Array.for_all ((=) '.') s.(0)) schematics
    |> List.map (fun s -> 
      List.init w (fun j -> 
        List.fold_left (fun acc i -> acc + if s.(i).(j) = '.' then 0 else 1) 0 (List.init h Fun.id)
      )
    )
  in

  (* Count valid lock-key pairs *)
  List.concat (List.map (fun lock -> List.map (fun key -> (lock, key)) keys) locks)
  |> List.filter (fun (lock, key) -> List.for_all (fun (l, k) -> l + k <= h) (List.combine lock key))
  |> List.length
;;


let parse input =
  (* Normalize line endings and split into sections *)
  let sections = Str.split (Str.regexp "\n\n\\|\r\n\r\n") input in

  (* Process each section *)
  List.map (fun section ->
    let lines = Str.split (Str.regexp "\n\\|\r\n") section in
    List.map (fun line -> String.trim line |> String.to_seq |> Array.of_seq) lines
  ) sections
;;
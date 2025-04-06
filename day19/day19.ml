open Str

let part1 (patterns, designs) =
  (* Convert patterns to array for faster lookup *)
  let patterns_array = Array.of_list patterns in

  (* Function to process each design *)
  let process_design design =
    (* Use memoization to avoid recalculating results *)
    let memo = Hashtbl.create 100 in

    let rec can_construct pos =
      (* If we've reached the end of the design, we've found a valid construction *)
      if pos = String.length design then
        true
      (* If we've already computed this position, return the cached result *)
      else if Hashtbl.mem memo pos then
        Hashtbl.find memo pos
      else
        (* Try each pattern, if any pattern works, return true *)
        let rec try_patterns i =
          if i >= Array.length patterns_array then
            false
          else
            let pattern = patterns_array.(i) in
            if pos + String.length pattern <= String.length design && 
               String.sub design pos (String.length pattern) = pattern &&
               can_construct (pos + String.length pattern) then
              true
            else
              try_patterns (i + 1)
        in
        
        let result = try_patterns 0 in
        (* Cache the result *)
        Hashtbl.add memo pos result;
        result
    in
    
    (* Start processing from the beginning of the design *)
    can_construct 0
  in
  
  List.length (List.filter process_design designs)


let part2 (patterns, designs) =
  (* Convert patterns to array for faster lookup *)
  let patterns_array = Array.of_list patterns in
  
  (* Process each design *)
  let process_design design =
    (* Memoization table to avoid recalculating results *)
    let memo = Hashtbl.create 100 in
    
    let rec count_ways pos =
      (* If we've reached the end of the design, we've found one valid way *)
      if pos = String.length design then
        1L
      (* If we've already computed this position, return the cached result *)
      else if Hashtbl.mem memo pos then
        Hashtbl.find memo pos
      else
        (* Count ways for each pattern *)
        let rec try_pattern i acc =
          if i >= Array.length patterns_array then
            acc
          else
            let pattern = patterns_array.(i) in
            let new_acc = 
              if pos + String.length pattern <= String.length design &&
                 String.sub design pos (String.length pattern) = pattern then
                Int64.add acc (count_ways (pos + String.length pattern))
              else
                acc
            in
            try_pattern (i + 1) new_acc
        in
        
        let ways = try_pattern 0 0L in
        (* Cache the result *)
        Hashtbl.add memo pos ways;
        ways
    in
    
    (* Start counting from the beginning *)
    count_ways 0
  in
  
  List.fold_left (fun acc design -> Int64.add acc (process_design design)) 0L designs



    let parse input =
    (* Try to split on double newline to get patterns and designs sections *)
    let parts = split (regexp "\n\n") input in
    
    if List.length parts >= 2 then
      (* Normal format: patterns on first part (comma separated), designs on lines in second part *)
      let patterns = 
        split (regexp ", ") (List.nth parts 0)
        |> List.filter (fun s -> String.trim s <> "")
      in
      
      let designs = 
        split (regexp "\n") (List.nth parts 1)
        |> List.map String.trim
        |> List.filter (fun s -> s <> "")
      in
      
      (patterns, designs)
    else
      (* Fallback to the current implementation for other formats *)
      let lines = 
        split (regexp "\n") input
        |> List.map String.trim
        |> List.filter (fun s -> s <> "")
      in
      
      match lines with
      | [] -> ([], [])
      | first_line :: rest ->
          let patterns = 
            split (regexp ", ") first_line
            |> List.filter (fun s -> String.trim s <> "")
          in
          
          (patterns, rest)


(** Main entry point for solution *)
let () =
  try
    let input = In_channel.input_all In_channel.stdin |> String.trim in
    Printf.printf "Input length: %d\n%!" (String.length input);
        
    let patterns, designs = parse input in
    Printf.printf "Parsed %d patterns and %d designs\n%!" 
      (List.length patterns) 
      (List.length designs);
    
    let start_time = Unix.gettimeofday () in
    
    part1 (patterns, designs) 
      |> string_of_int 
      |> Printf.printf "Part 1: %s\n%!";
      
    part2 (patterns, designs) 
      |> Int64.to_int 
      |> string_of_int 
      |> Printf.printf "Part 2: %s\n%!";
      
    Unix.gettimeofday () -. start_time
      |> Printf.printf "Elapsed time: %.4f seconds\n"
    
  with
  | Failure msg -> Printf.printf "Error: %s\n" msg
  | e -> Printf.printf "Unexpected error: %s\n" (Printexc.to_string e)
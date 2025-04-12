type gate_operation =
  | And
  | Or
  | Xor

let evaluate_gate operation a b =
  match operation with
  | And -> a land b  (* Bitwise AND *)
  | Or  -> a lor b   (* Bitwise OR *)
  | Xor -> a lxor b  (* Bitwise XOR *)

type gate = {
  input : string * string;
  operation : gate_operation;
  output : string;
}


let evaluate wires gates =
  (* Create dependency graph *)
  let deps = Hashtbl.create (List.length gates) in
  let waiting = Hashtbl.create (List.length gates) in
  
  (* Initialize dependency tracking *)
  List.iter (fun g ->
    let input1, input2 = g.input in
    Hashtbl.add waiting g.output (input1, input2);
    List.iter (fun input -> 
      let deps_list = try Hashtbl.find deps input with Not_found -> [] in
      Hashtbl.replace deps input (g.output :: deps_list)
    ) [input1; input2]
  ) gates;

  (* Process gates in dependency order *)
  let rec process_ready () =
    let ready = Hashtbl.fold (fun output (in1, in2) acc ->
      if Hashtbl.mem wires in1 && Hashtbl.mem wires in2 then
        (output, (in1, in2)) :: acc
      else acc
    ) waiting [] in
    
    match ready with
    | [] -> wires
    | _ ->
        List.iter (fun (output, (in1, in2)) ->
          let g = List.find (fun g -> g.output = output) gates in
          let result = evaluate_gate g.operation 
            (Hashtbl.find wires in1) 
            (Hashtbl.find wires in2) in
          Hashtbl.add wires output result;
          Hashtbl.remove waiting output
        ) ready;
        process_ready ()
  in
  process_ready ()
;;


let extract_result wires =
  (* Pre-calculate max z-wire index *)
  let max_z = Hashtbl.fold (fun key _ acc ->
    if String.starts_with ~prefix:"z" key then
      max acc (int_of_string (String.sub key 1 (String.length key - 1)))
    else acc
  ) wires 0 in
  
  (* Build result directly as int *)
  let rec build_num i acc =
    if i < 0 then acc
    else
      let wire_name = Printf.sprintf "z%02d" i in
      let bit = if Hashtbl.mem wires wire_name then Hashtbl.find wires wire_name else 0 in
      build_num (i-1) ((acc lsl 1) lor bit)
  in
  build_num max_z 0
;;


let part1 (wires, gates) =
  let wire_table = Hashtbl.create 100 in
  List.iter (fun (key, value) -> Hashtbl.add wire_table key value) wires;
  let final_state = evaluate wire_table gates in
  Int64.of_int (extract_result final_state)
;;



let has_loop gate_map =
  let rec dfs out path =
    if List.mem out path then true
    else match Hashtbl.find_opt gate_map out with
    | None -> false
    | Some g ->
        let left, right = g.input in
        dfs left (out :: path) || dfs right (out :: path)
  in
  Hashtbl.fold (fun key _ acc -> acc || dfs key []) gate_map false
;;

let rec collect out gate_map =
  match Hashtbl.find_opt gate_map out with
  | None -> []
  | Some g ->
      let left, right = g.input in
      out :: (collect left gate_map @ collect right gate_map)
;;

let rec make out gate_map =
  match Hashtbl.find_opt gate_map out with
  | None -> out
  | Some g ->
      let left, right = make (fst g.input) gate_map, make (snd g.input) gate_map in
      let left, right = min left right, max left right in
      match g.operation with
      | And -> "(" ^ left ^ ")and(" ^ right ^ ")"
      | Or -> "(" ^ left ^ ")or(" ^ right ^ ")"
      | Xor -> "(" ^ left ^ ")xor(" ^ right ^ ")"
;;

let validate out gate_map =
  let circuit = make out gate_map in
  let valid_xy = 
    let regex_xy = Str.regexp "[xy][0-9]+" in
    let xy_matches = Str.full_split regex_xy circuit |> List.filter_map (function
      | Str.Text _s -> None
      | Str.Delim s -> Some s) in
    match xy_matches with
    | [] -> true
    | "x00" :: "y00" :: t ->
        let pairs = List.filteri (fun i _ -> i mod 2 = 0) t in
        List.for_all (fun i -> List.nth pairs i = "x" ^ string_of_int i && List.nth pairs (i+1) = "y" ^ string_of_int i) (List.init (List.length pairs / 2) (fun i -> i))
    | _ -> false
  in
  let valid_operations =
    let regex_ops = Str.regexp "\\(and\\|or\\|xor\\)" in
    let ops_matches = Str.full_split regex_ops circuit |> List.filter_map (function
      | Str.Text _s -> None
      | Str.Delim s -> Some s) in
    match ops_matches with
    | [] -> true
    | ["xor"] -> true
    | "and" :: t ->
      let chunks = Array.of_list (List.filteri (fun i _ -> i mod 4 = 0) t) in
      List.for_all (fun i ->
        if i + 1 < Array.length chunks then
          chunks.(i) = "and" && chunks.(i+1) = "xor" && chunks.(i+2) = "or" && chunks.(i+3) = "and"
        else
          chunks.(i) = "xor" && chunks.(i+1) = "xor"
      ) (List.init (Array.length chunks / 4) (fun i -> i))
    | _ -> false
  in
  valid_xy && valid_operations
;;

let rec search i gate_map =
  if i >= 45 then Some gate_map
  else
    let out = "z" ^ string_of_int i in
    if validate out gate_map then search (i + 1) gate_map
    else
      let swaps = collect out gate_map in
      List.find_map (fun _swap ->
        Hashtbl.fold (fun out' g' acc ->
          let g = Hashtbl.find gate_map out in
          let new_gate_map = Hashtbl.copy gate_map in
          Hashtbl.replace new_gate_map out g';
          Hashtbl.replace new_gate_map out' g;
          if not (has_loop new_gate_map) then
            match List.find_opt (fun j -> not (validate ("z" ^ string_of_int j) new_gate_map)) (List.init 45 (fun j -> j)) with
            | Some next_invalid when i < next_invalid -> search next_invalid new_gate_map
            | _ -> acc
          else acc
        ) gate_map None
      ) swaps
;;

let part2 (_wires, gates) =
  let gate_map = Hashtbl.create 100 in
  List.iter (fun g -> Hashtbl.add gate_map g.output g) gates;
  match search 0 gate_map with
  | None -> ""
  | Some correct_gate_map ->
      let diff = Hashtbl.fold (fun out g acc ->
        if Hashtbl.mem correct_gate_map out then
          let g' = Hashtbl.find correct_gate_map out in
          if g <> g' then out :: acc else acc
        else acc
      ) gate_map [] in
      String.concat "," (List.sort compare diff)
;;


let parse input =
  let normalized_input = Str.global_replace (Str.regexp "\r\n") "\n" input in
  let sections = Str.split (Str.regexp "\n\n") normalized_input in

  if List.length sections < 2 then
    failwith "Expected at least 2 sections, but found fewer.";

  let wire_lines = match List.nth_opt sections 0 with
    | Some s -> Str.split (Str.regexp "\n") s
    | None -> failwith "Expected wire section, but input is malformed."
  in
  let wires = List.map (fun line ->
    match Str.split (Str.regexp ":") line with
    | [wire; value] -> (String.trim wire, int_of_string (String.trim value))
    | _ -> failwith ("Invalid wire line: " ^ line)
  ) wire_lines in

  let gate_lines = match List.nth_opt sections 1 with
    | Some s -> Str.split (Str.regexp "\n") s
    | None -> failwith "Expected gate section, but input is missing."
  in
  let gates = List.map (fun line ->
    let parts = Str.split (Str.regexp " ") line in
    match parts with
    | left :: op_str :: right :: "->" :: output :: _ ->
        let operation =
          match op_str with
          | "AND" -> And
          | "OR" -> Or
          | "XOR" -> Xor
          | _ -> failwith ("Unknown operation: " ^ op_str ^ " in line: " ^ line)
        in
        { input = (left, right); operation; output }
    | _ -> failwith ("Malformed gate line: " ^ line)
  ) gate_lines in

  wires, gates
;;
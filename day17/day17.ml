

type cpu_registers = {
  accumulator : int64;   
  register_b : int64;   
  register_c : int64;   
}

type cpu_state = {
  instruction_pointer : int;          
  register : cpu_registers;     
  instructions : int array; 
  outputs : int64 list;     
}

let rec power_of_two base exponent =
  if exponent = 0L then 1L
  else if Int64.rem exponent 2L = 0L then
    power_of_two (Int64.mul base base) (Int64.div exponent 2L)
  else
    Int64.mul base (power_of_two base (Int64.sub exponent 1L))




let run_3bit_computer init_register init_instruction (halt_condition : cpu_state -> int64 array option) =
  let rec step (state : cpu_state) =
    match halt_condition state with
    | Some output_array -> output_array
    | None ->
      let { instruction_pointer; register; instructions; outputs } = state in

      let resolve_combo_operand op =
        match op with
        | 0 | 1 | 2 | 3 as x -> Int64.of_int x
        | 4 -> register.accumulator
        | 5 -> register.register_b
        | 6 -> register.register_c
        | x -> failwith (Printf.sprintf "Invalid combo: %d" x)
      in
        
        let opcode = program.(ip) in
        let operand = program.(ip + 1) in

        let newEnv = 
          match opcode with
          | 0 -> 
              { env with 
                ip = ip + 2;
                register = 
                  { reg with a = Int64.div reg.a (pow' 2L (combo operand)) } 
              }
          | 1 -> 
              { env with 
                ip = ip + 2;
                register = { reg with b = Int64.logxor reg.b (Int64.of_int operand) } 
              }
          | 2 -> 
              { env with 
                ip = ip + 2;
                register = { reg with b = Int64.rem (combo operand) 8L } 
              }
          | 3 -> 
              if reg.a = 0L then 
                { env with ip = ip + 2 }
              else 
                { env with ip = operand }
          | 4 -> 
              { env with 
                ip = ip + 2;
                register = { reg with b = Int64.logxor reg.b reg.c } 
              }
          | 5 -> 
              { env with 
                ip = ip + 2;
                out = Int64.rem (combo operand) 8L :: out
              }
          | 6 -> 
              { env with 
                ip = ip + 2;
                register = 
                  { reg with b = Int64.div reg.a (pow' 2L (combo operand)) } 
              }
          | 7 -> 
              { env with 
                ip = ip + 2; 
                register = 
                  { reg with c = Int64.div reg.a (pow' 2L (combo operand)) } 
              }
          | x -> failwith (Printf.sprintf "%d !?" x)
        in
        
        f newEnv
  in
  
  f { ip = 0;
      register = register;
      program = program;
      out = [] }

  
let output_on_halt env = 
  if env.ip >= Array.length env.program then
    Some (
      env.out
      |> List.rev_map Int64.to_int
      |> Array.of_list
    )
  else
    None


let part1 register program =
  let out = execute register program output_on_halt in
  out 
  |> Array.map string_of_int 
  |> Array.to_list 
  |> String.concat ","


let part2 register program =
  (* A helper to compare two int arrays by value *)
  let array_equal eq a b =
    if Array.length a <> Array.length b then false
    else
      let rec aux i =
        if i = Array.length a then true
        else if not (eq a.(i) b.(i)) then false
        else aux (i + 1)
      in
      aux 0
  in

  (* OCaml equivalent of F# List.tryPick *)
  let rec find_map f = function
    | [] -> None
    | x :: xs -> 
        match f x with
        | Some _ as res -> res
        | None -> find_map f xs
  in

  let rec find i a =
    if i = 0 then
      Some a
    else
      find_map
        (fun j ->
          let a' = Int64.add (Int64.mul a 8L) (Int64.of_int j) in
          let out = execute { register with a = a' } program output_on_halt in
          let slice_len = Array.length program - (i - 1) in
          let remainder = Array.sub program (i - 1) slice_len in
          if array_equal ( = ) out remainder then find (i - 1) a' else None
        )
        [0; 1; 2; 3; 4; 5; 6; 7]
  in

  match find (Array.length program) 0L with
  | Some result -> result
  | None -> failwith "No solution found"


let parse input =
  (* Normalize line endings and trim *)
  let input = 
    input 
    |> String.map (function '\r' -> ' ' | c -> c) 
    |> String.trim 
  in
  
  (* Split input into lines *)
  let lines = String.split_on_char '\n' input 
  in
  
  (* Find the register section and program section *)
  let register_lines =
    lines 
    |> List.take_while (fun line -> String.trim line <> "") 
  in
  
  let program_line =
    lines
    |> List.drop_while (fun line -> String.trim line <> "")
    |> List.drop_while (fun line -> String.trim line = "")
    |> function
       | [] -> None
       | line :: _ -> Some line
  in
  
  (* Parse registers *)
  let get_register_value prefix =
    register_lines
    |> List.find_opt (fun line -> String.starts_with ~prefix line)
    |> Option.map (fun line -> 
        String.sub line (String.length prefix) (String.length line - String.length prefix)
        |> String.trim
        |> Int64.of_string
      )
    |> Option.value ~default:0L
  in
  
  let register = {
    a = get_register_value "Register A:";
    b = get_register_value "Register B:";
    c = get_register_value "Register C:"
  } 
 in
  
  (* Parse program *)
  let program = 
    match program_line with
    | Some line when String.starts_with ~prefix:"Program:" line ->
        let program_str = String.sub line (String.length "Program:") 
                             (String.length line - String.length "Program:") 
                         |> String.trim 
        in
        String.split_on_char ',' program_str
        |> List.map String.trim
        |> List.map int_of_string
        |> Array.of_list
    | _ -> [||]
  in
  
  register, program



(** Main Entry Point*)
let () =
    let input = In_channel.input_all In_channel.stdin 
    |> String.trim 
  in
    let register, program = parse input 
  in
    let start_time = Unix.gettimeofday () 
  in
    part1 register program |> Printf.printf "Part 1: %s\n";
    part2 register program |> Int64.to_int |> Printf.printf "Part 2: %d\n";
    
    let elapsed = Unix.gettimeofday () -. start_time 
  in
    Printf.printf "Elapsed time: %.4f seconds\n" elapsed
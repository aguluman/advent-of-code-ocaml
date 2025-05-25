(* Instruction type *)
type instruction =
  | Multiply of int * int
  | Enable
  | Disable



(* Helper function to check if a character is a digit *)
let is_digit c = c >= '0' && c <= '9'


(* Parse a number and return the rest of the string *)
let parse_number text =
  let rec take_digits i =
    if i < String.length text && is_digit text.[i] then
      take_digits (i + 1)
    else
      i
  in
  let len = take_digits 0 in
  if len = 0 then
    None
  else
    let num = int_of_string (String.sub text 0 len) in
    Some (num, String.sub text len (String.length text - len))

(* Parse a prefix string *)
let parse_prefix prefix text =
  let prefix_len = String.length prefix in
  if String.length text >= prefix_len &&
     String.sub text 0 prefix_len = prefix then
    Some (prefix, String.sub text prefix_len (String.length text - prefix_len))
  else
    None

(* Parse a multiplication expression *)
let parse_mul_expr text =
  match parse_prefix "mul(" text with
  | None -> None
  | Some (_, rest1) ->
      match parse_number rest1 with
      | None -> None
      | Some (x, rest2) ->
          match parse_prefix "," rest2 with
          | None -> None
          | Some (_, rest3) ->
              match parse_number rest3 with
              | None -> None
              | Some (y, rest4) ->
                  match parse_prefix ")" rest4 with
                  | None -> None
                  | Some (_, rest5) -> Some ((x, y), rest5)

(* Parse input for part 1 *)
let parse1 input =
  let rec p text acc =
    if String.length text = 0 then
      List.rev acc
    else
      match parse_mul_expr text with
      | Some ((x, y), rest) -> p rest ((x, y) :: acc)
      | None -> p (String.sub text 1 (String.length text - 1)) acc
  in
  p input []

(* Parser type *)
type 'a parser = string -> ('a * string) option

(* Parse input for part 2 *)
let parse2 input =

  let p_choice (p1 : 'a parser) (p2 : 'a parser) : 'a parser =
    fun text ->
      match p1 text with
      | Some result -> Some result
      | None -> p2 text
  in



  (* Define parsers for each instruction type *)
  let parse_mul text = 
    match parse_mul_expr text with
    | Some ((x, y), rest) -> Some (Multiply (x, y), rest)
    | None -> None
  in
  
  let parse_do text =
    match parse_prefix "do()" text with
    | Some (_, rest) -> Some (Enable, rest)
    | None -> None
  in
  
  let parse_dont text =
    match parse_prefix "don't()" text with
    | Some (_, rest) -> Some (Disable, rest)
    | None -> None
  in

  let rec p text acc =
    if String.length text = 0 then
      List.rev acc
    else
      match p_choice parse_mul (p_choice parse_do parse_dont) text with
      | Some (ins, rest) -> p rest (ins :: acc)
      | None -> p (String.sub text 1 (String.length text - 1)) acc
  in
  p input []



(* Part 1: Sum of products *)
let part1 multiples =
  List.fold_left (fun acc (x, y) -> acc + x * y) 0 multiples

  

(* Part 2: Process instructions with enabled/disabled state *)
let part2 instructions =
  List.fold_left (fun (sum, on) ins ->
    match ins with
    | Multiply(x, y) ->
        let new_sum = if on then sum + x * y else sum in
        (new_sum, on)
    | Enable -> (sum, true)
    | Disable -> (sum, false)
  ) (0, true) instructions
  |> fst
(** Day 04: Printing Department

    [Brief description of the problem and what this module solves]

    {2 Problem Summary:}
    - {b Part 1:} [Description of part 1]
    - {b Part 2:} [Description of part 2]

    See details at:
    {{:https://adventofcode.com/2025/day/4} Advent of Code 2025, Day 04} *)

type grid = {
  data : Bytes.t;
  rows : int;
  cols : int;
}

(** Helper function to parse input string into a structured data format

    @param input Raw input string from the puzzle
    @return Parsed data structure *)
let parse input =
  let lines =
    input |> String.split_on_char '\n'
    |> List.filter (fun line -> String.length line > 0 && line.[0] <> ' ')
    |> Array.of_list
  in
  let rows = Array.length lines in
  if rows = 0 then { data = Bytes.empty; rows = 0; cols = 0 }
  else
    let cols = String.length lines.(0) in
    let data = Bytes.create (rows * cols) in
    for i = 0 to rows - 1 do
      let line = lines.(i) in
      for j = 0 to cols - 1 do
        Bytes.unsafe_set data ((i * cols) + j) (String.unsafe_get line j)
      done
    done;
    { data; rows; cols }

(** Count adjacent '@' cells — fully unrolled for speed *)
let[@inline] count_adjacent data rows cols i j =
  let c = ref 0 in
  let row_off = i * cols in
  let i_m1 = i - 1 and i_p1 = i + 1 in
  let j_m1 = j - 1 and j_p1 = j + 1 in
  (* Top row *)
  if i_m1 >= 0 then begin
    let off = i_m1 * cols in
    if j_m1 >= 0 && Bytes.unsafe_get data (off + j_m1) = '@' then incr c;
    if Bytes.unsafe_get data (off + j) = '@' then incr c;
    if j_p1 < cols && Bytes.unsafe_get data (off + j_p1) = '@' then incr c
  end;
  (* Middle row *)
  if j_m1 >= 0 && Bytes.unsafe_get data (row_off + j_m1) = '@' then incr c;
  if j_p1 < cols && Bytes.unsafe_get data (row_off + j_p1) = '@' then incr c;
  (* Bottom row *)
  if i_p1 < rows then begin
    let off = i_p1 * cols in
    if j_m1 >= 0 && Bytes.unsafe_get data (off + j_m1) = '@' then incr c;
    if Bytes.unsafe_get data (off + j) = '@' then incr c;
    if j_p1 < cols && Bytes.unsafe_get data (off + j_p1) = '@' then incr c
  end;
  !c

(** [part1 input] solves part 1 of the challenge

    @param input Raw input string from the puzzle
    @return Solution for part 1 *)
let part1 input =
  let grid = parse input in
  let rows = grid.rows in
  let cols = grid.cols in
  let data = grid.data in
  if rows = 0 then 0
  else
    let count = ref 0 in
    for i = 0 to rows - 1 do
      let row_offset = i * cols in
      for j = 0 to cols - 1 do
        if Bytes.unsafe_get data (row_offset + j) = '@' then
          if count_adjacent data rows cols i j < 4 then incr count
      done
    done;
    !count

(** [part2 input] solves part 2 of the challenge

    @param input Raw input string from the puzzle
    @return Solution for part 2 *)
let part2 input =
  let grid = parse input in
  let rows = grid.rows in
  let cols = grid.cols in
  let data = grid.data in
  if rows = 0 then 0L
  else
    let size = rows * cols in

    (* Neighbor count cache — use bytes (0-8 fits in a byte) *)
    let cache = Bytes.make size '\000' in

    (* Array-based circular queue storing flat indices *)
    let q_data = Array.make size 0 in
    let q_head = ref 0 in
    let q_tail = ref 0 in

    let[@inline] q_push idx =
      q_data.(!q_tail) <- idx;
      q_tail := !q_tail + 1
    in

    let[@inline] q_pop () =
      let idx = q_data.(!q_head) in
      q_head := !q_head + 1;
      idx
    in

    let[@inline] q_empty () = !q_head = !q_tail in

    (* Build cache and initial queue *)
    for i = 0 to rows - 1 do
      for j = 0 to cols - 1 do
        let idx = (i * cols) + j in
        if Bytes.unsafe_get data idx = '@' then begin
          let cnt = count_adjacent data rows cols i j in
          Bytes.unsafe_set cache idx (Char.unsafe_chr cnt);
          if cnt < 4 then q_push idx
        end
      done
    done;

    let removed = ref 0 in

    (* Process queue *)
    while not (q_empty ()) do
      let idx = q_pop () in
      if Bytes.unsafe_get data idx = '@' then begin
        Bytes.unsafe_set data idx '.';
        incr removed;

        (* Compute i,j from idx — use multiplication check instead of division *)
        let i = idx / cols in
        let j = idx - (i * cols) in
        (* Faster than idx mod cols *)

        (* Update neighbors *)
        let i_m1 = i - 1 and i_p1 = i + 1 in
        let j_m1 = j - 1 and j_p1 = j + 1 in

        (* Inline neighbor update to avoid loop overhead *)
        let[@inline] update_neighbor ni nj =
          if ni >= 0 && ni < rows && nj >= 0 && nj < cols then
            let nidx = (ni * cols) + nj in
            if Bytes.unsafe_get data nidx = '@' then begin
              let old_cnt = Char.code (Bytes.unsafe_get cache nidx) in
              let new_cnt = old_cnt - 1 in
              Bytes.unsafe_set cache nidx (Char.unsafe_chr new_cnt);
              if old_cnt = 4 then q_push nidx
            end
        in

        update_neighbor i_m1 j_m1;
        update_neighbor i_m1 j;
        update_neighbor i_m1 j_p1;
        update_neighbor i j_m1;
        update_neighbor i j_p1;
        update_neighbor i_p1 j_m1;
        update_neighbor i_p1 j;
        update_neighbor i_p1 j_p1
      end
    done;

    Int64.of_int !removed

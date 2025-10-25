open Base

let split_updates rules updates =
  let pages =
    List.fold rules ~init:[] ~f:(fun acc (p, q) -> p :: q :: acc)
    |> List.dedup_and_sort ~compare:Int.compare
  in

  List.iter (List.cartesian_product pages pages) ~f:(fun (p, q) ->
      assert (
        p = q
        || List.exists rules ~f:(fun (x, y) -> x = p && y = q)
        || List.exists rules ~f:(fun (x, y) -> x = q && y = p)));

  let following =
    List.fold rules
      ~init:(Map.empty (module Int))
      ~f:(fun acc (p, q) ->
        let v =
          Map.find acc p |> Option.value ~default:(Set.empty (module Int))
        in
        Map.set acc ~key:p ~data:(Set.add v q))
  in

  let partition_updates updates =
    let check_updates updates =
      List.for_alli updates ~f:(fun i p ->
          List.for_all
            (List.drop updates (i + 1))
            ~f:(fun q ->
              let p_q =
                Map.find following p
                |> Option.value_map ~default:true ~f:(fun v -> Set.mem v q)
              in
              let q_p =
                Map.find following q
                |> Option.value_map ~default:true ~f:(fun v ->
                    not (Set.mem v p))
              in
              p_q && q_p))
    in
    List.partition_tf updates ~f:check_updates
  in

  let correct, incorrect = partition_updates updates in
  (following, (correct, incorrect))

let part1 rules updates =
  let _, (correct_updates, _) = split_updates rules updates in
  List.fold correct_updates ~init:0 ~f:(fun acc updates ->
      acc + List.nth_exn updates (List.length updates / 2))

let part2 rules updates =
  let following, (_, incorrect_updates) = split_updates rules updates in
  List.fold incorrect_updates ~init:0 ~f:(fun acc updates ->
      let sorted =
        List.sort updates ~compare:(fun p q ->
            match Map.find following p with
            | Some v -> if Set.mem v q then -1 else 1
            | None -> 1)
      in
      acc + List.nth_exn sorted (List.length sorted / 2))

let parse input =
  (* Split input into rules and updates blocks *)
  let blocks = String.split input ~on:'\n' in
  let rules_block, updates_block =
    match
      List.group blocks ~break:(fun _ x -> String.is_empty (String.strip x))
    with
    | [ rules; updates ] ->
        (String.concat ~sep:"\n" rules, String.concat ~sep:"\n" updates)
    | _ -> ("", "")
  in

  (* Parse rules *)
  let rules =
    String.split rules_block ~on:'\n'
    |> List.filter ~f:(fun s -> not (String.is_empty (String.strip s)))
    |> List.filter_map ~f:(fun line ->
        try
          let parts = String.split (String.strip line) ~on:'|' in
          match parts with
          | [ p; q ] ->
              Some
                (Int.of_string (String.strip p), Int.of_string (String.strip q))
          | _ -> None
        with _ -> None)
  in

  (* Parse updates *)
  let updates =
    String.split updates_block ~on:'\n'
    |> List.filter ~f:(fun s -> not (String.is_empty (String.strip s)))
    |> List.map ~f:(fun line ->
        String.split (String.strip line) ~on:','
        |> List.filter_map ~f:(fun x ->
            try Some (Int.of_string (String.strip x)) with _ -> None))
  in

  (rules, updates)

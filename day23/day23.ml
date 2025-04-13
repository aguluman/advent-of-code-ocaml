(** {1 Day 23: LAN Party - Graph Connection and Clique Analysis}

    Solves the Advent of Code Day 23 challenges focused on analyzing network 
    connections to locate a LAN party.
    
    This module performs two main tasks:
    - {b Part 1:} Count all connected triples (sets of 3 nodes) with at least 
      one node name starting with 't'
    - {b Part 2:} Find the largest clique, which is the biggest subset of nodes 
      where each node connects directly with every other node
    
    {2 Problem details:}
    
    - {b Input:} Network connections represented as bidirectional strings
    - {b Output (Part 1):} Number of triple-connected sets containing at least 
      one node starting with 't'
    - {b Output (Part 2):} Comma-separated node names sorted alphabetically 
      forming the largest clique (password)
    
    {2 Used concepts:}
    
    - Graph representation as adjacency maps
    - Search algorithms to identify triangles and larger cliques
    - String and collection manipulations for efficiency and clarity
    
    See details at: {{:https://adventofcode.com/2024/day/23} Advent of Code 2024, Day 23}
*)



module StringMap = Map.Make(String)
module StringSet = Set.Make(String)


(** Efficiently checks if three nodes form a connected triple (triangle) in the graph.

    The function is considered a triple/triange if each node connects directly 
    to the other two.

    @param a First node identifier
    @param b Second node identifier
    @param c Third node identifier
    @param connections Adjacency map representing node connections
    @return [true] if nodes form a triangle; otherwise, [false]
*)
let are_triangle a b c connections =
  StringSet.mem b (StringMap.find a connections) &&
  StringSet.mem c (StringMap.find b connections) &&
  StringSet.mem a (StringMap.find c connections)



(** Counts all sets of three interconnected nodes (triangles) in the network,
    ensuring that at least one node name starts with 't'.

    Algorithm:
  - 1. Iterate through each node and its neighbors
  - 2. For each pair of neighbors, verify if they form a connected triple
  - 3. Ensure no triple is counted more than once by using a sorted tuple set
  - 4. Increment count only if at least one node in the set begins with 't'

    @param connections Adjacency map representing node connections
    @return The total count of qualifying connected triples
*)
let part1 connections =
  let discovered = ref StringSet.empty in
  let count = ref 0 in

  StringMap.iter (fun node neighbors ->
    StringSet.iter (fun neighbor ->
      (* Early filtering: Only allow lexically greater nodes to avoid recounts *)
      if String.compare node neighbor < 0 then
        let next_neighbors = StringMap.find neighbor connections in
        StringSet.iter (fun next_neighbor ->
          (* Ensure consistent ordering and uniqueness *)
          if String.compare neighbor next_neighbor < 0 &&
             are_triangle node neighbor next_neighbor connections then
            let triple = [| node; neighbor; next_neighbor |] in
            Array.sort String.compare triple;
            let key = String.concat "," (Array.to_list triple) in
            
            let is_newly_added = not (StringSet.mem key !discovered) in
            discovered := StringSet.add key !discovered;
            
            if is_newly_added && 
               Array.exists (fun x -> String.length x > 0 && x.[0] = 't') triple then
              incr count
        ) next_neighbors
    ) neighbors
  ) connections;
  
  !count



(** Finds the largest clique (fully connected group) in the network.

    The clique identification involves the following steps:
  - 1. Begin with each node and iteratively attempt to add other nodes that 
       connect to every node in the existing group
  - 2. Continue recursively until no further nodes can be added
  - 3. Keep track of the largest clique found throughout the process
  - 4. Return this largest clique sorted alphabetically as the final password

    @param connections Adjacency map representing node connections
    @return A comma-separated, alphabetically sorted string of the nodes 
            constituting the largest clique, serving as the LAN party password
*)
let part2 connections =
  let nodes = StringMap.bindings connections |> List.map fst |> Array.of_list in
  let largest_clique = ref [||] in

  (* Efficiently checks if the candidate can be added to the current clique *)
  let can_add_to_clique clique candidate =
    Array.for_all (fun n -> 
      StringSet.mem n (StringMap.find candidate connections)
    ) clique 
  in

  (* Recursive search for clique with aggressive early termination *)
  let rec grow_clique current_clique candidates =
    match candidates with
    | [||] ->
        if Array.length current_clique > Array.length !largest_clique then
          largest_clique := current_clique
    | _ ->
        (* Helper function to process candidates one by one in a tail-recursive manner *)
        let rec process_candidates index =
          if index >= Array.length candidates then
            () (* Done processing all candidates *)
          else
            let candidate = candidates.(index) in

            (* Early termination check *)
            if Array.length current_clique + (Array.length candidates - index) <= Array.length !largest_clique then
              (* Skip remaining candidates as they can't improve our result *)
              ()
            else
              let next_clique = Array.append current_clique [| candidate |] in

              let next_candidates =
                Array.sub candidates (index + 1) (Array.length candidates - index - 1)
                |> Array.to_list
                |> List.filter (can_add_to_clique next_clique)
                |> Array.of_list
              in

              (* Process this branch *)
              grow_clique next_clique next_candidates;

              (* Continue with the next candidate *)
              process_candidates (index + 1)
        in

        (* Start processing from the first candidate *)
        process_candidates 0
  in

  (* Sort nodes to improve clique-finding performance through informed pruning *)
  let sorted_nodes = Array.copy nodes in
  Array.sort (fun n1 n2 -> 
    compare 
      (StringSet.cardinal (StringMap.find n2 connections)) 
      (StringSet.cardinal (StringMap.find n1 connections))
  ) sorted_nodes;

  (* Process each starting node (sequential version, no parallel processing) *)
  Array.iter (fun node ->
    let candidates =
      StringMap.find node connections
      |> StringSet.elements
      |> List.filter (fun n -> String.compare n node > 0)
      |> Array.of_list
    in

    grow_clique [| node |] candidates
  ) sorted_nodes;

  (* Return the largest clique sorted alphabetically for the password *)
  let result = Array.copy !largest_clique in
  Array.sort String.compare result;
  String.concat "," (Array.to_list result)



(** Parses the input into a map of connections.

    @param input String containing the list of connections
    @return Map of nodes to their connected neighbors
*)
let parse input =
  let lines = String.split_on_char '\n' input 
            |> List.filter (fun s -> String.length s > 0) in
  
  List.fold_left (fun connections line ->
    let parts = String.trim line |> String.split_on_char '-' in
    let src = List.nth parts 0 in
    let dest = List.nth parts 1 in
    
    (* Add bidirectional connections *)
    let connections' = 
      match StringMap.find_opt src connections with
      | Some neighbors -> StringMap.add src (dest :: neighbors) connections
      | None -> StringMap.add src [dest] connections
    in
    
    match StringMap.find_opt dest connections' with
    | Some neighbors -> StringMap.add dest (src :: neighbors) connections'
    | None -> StringMap.add dest [src] connections'
  ) StringMap.empty lines



(** Converts the connection map from lists to optimized sets for faster lookups.

    @param connections Map where keys are node identifiers and values are lists of connected nodes
    @return Map where keys are node identifiers and values are sets of connected nodes
*)
let to_optimized_dict connections =
  StringMap.fold (fun key values acc ->
    let set = List.fold_left (fun set elem -> 
      StringSet.add elem set
    ) StringSet.empty values in
    StringMap.add key set acc
  ) connections StringMap.empty




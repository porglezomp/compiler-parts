module SMap = Map.Make(struct
    type t = string
    let compare = compare
  end)

let iota n =
    let rec go acc n = if n < 0 then acc else go (n::acc) (n-1)
    in go [] (n - 1)

let () =
    Random.self_init () ;
    let length = Random.int 10 + 30 in
    Printf.printf "%d\n" length ;
    let intervals = iota length |> List.map (fun label ->
        let label = string_of_int label in
        let start = Random.int 75 in
        let finish = start + Random.int 29 + 1 in
        Printf.printf "%s %d %d\n" label start finish ;
        Conflict.({ label; start; finish })
    ) in
    let interval_map =
      intervals
      |> List.fold_left (fun map x -> map |> SMap.add x.Conflict.label x)
        SMap.empty
    in
    let label x = ["label", x] in

    let svg = open_out "target/intervals.svg" in
    intervals
        |> Conflict.svg label
        |> output_string svg ;
    flush svg ;

    let graph = intervals |> Conflict.conflict in

    let dot = open_out "target/conflict.dot" in
    graph
        |> Graph.graphviz label (fun () -> [])
        |> output_string dot ;
    flush dot ;
    flush_all () ;

    match graph |> Graph.color 8 with
    | Error node ->
        Printf.printf "failed coloring graph: conflict in %s\n"
            (Graph.node_label node) ;
        exit 1
    | Ok colored ->
      let dot = open_out "target/color.dot" in
      colored
      |> Graph.graphviz (Graph.fmt_color label) (fun () -> [])
      |> output_string dot ;

      let get_colored_interval color =
        let label = Graph.color_label color in
        let interval = interval_map |> SMap.find label in
        { interval with label = color }
      in
      let nodes = Graph.nodes colored in
      let svg = open_out "target/color-intervals.svg" in
      nodes
      |> List.map (fun n -> n |> Graph.node_label |> get_colored_interval)
      |> Conflict.svg (Graph.fmt_color label)
      |> output_string svg

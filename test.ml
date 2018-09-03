let iota n =
    let rec go acc n = if n < 0 then acc else go (n::acc) (n-1)
    in go [] (n - 1)

let () =
    Random.self_init () ;
    let length = Random.int 10 + 10 in
    Printf.printf "%d\n" length ;
    let intervals = iota length |> List.map (fun label ->
        let label = string_of_int label in
        let start = Random.int 75 in
        let finish = start + Random.int 29 + 1 in
        Printf.printf "%s %d %d\n" label start finish ;
        Conflict.({ label; start; finish })
    ) in

    let svg = open_out "target/intervals.svg" in
    intervals
        |> Conflict.svg (fun x -> x)
        |> output_string svg ;
    flush svg ;

    let graph = intervals |> Conflict.conflict in

    let label x = ["label", x] in
    let dot = open_out "target/conflict.dot" in
    graph
        |> Graph.graphviz label (fun () -> [])
        |> output_string dot ;
    flush dot ;
    flush_all () ;

    let dot = open_out "target/color.dot" in
    match graph |> Graph.color 6 with
    | Error node ->
        Printf.printf "failed coloring graph: conflict in %s\n"
            (Graph.node_label node)
    | Ok colored -> colored
        |> Graph.graphviz (Graph.fmt_color label) (fun () -> [])
        |> output_string dot

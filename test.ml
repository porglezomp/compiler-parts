let iota n =
    let rec go acc n = if n < 0 then acc else go (n::acc) (n-1)
    in go [] (n - 1)

let () =
    Random.self_init () ;
    let length = Random.int 10 + 10 in
    Printf.printf "%d\n" length ;
    let intervals = iota length |> List.map (fun label ->
        let start = Random.int 75 in
        let finish = start + Random.int 29 + 1 in
        Printf.printf "%d %d %d\n" label start finish ;
        Conflict.({ label; start; finish })
    ) in
    (*
    let intervals: int Conflict.interval list = [
        { label = 0; start =  0; finish = 15 };
        { label = 1; start =  5; finish = 10 };
        { label = 2; start = 13; finish = 20 };
    ] in
    *)
    let svg = open_out "target/out.svg" in
    intervals |> Conflict.svg string_of_int |> output_string svg ;
    let dot = open_out "target/out.dot" in
    intervals
        |> Conflict.conflict
        |> Graph.graphviz string_of_int (fun () -> "")
        |> output_string dot

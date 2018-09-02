module IntMap = Map.Make(struct type t = int let compare = compare end)
module IntIntMap = Map.Make(struct type t = int * int let compare = compare end)

type 'n node = { lbl: 'n; id: int }
type 'e edge = { lbl: 'e; a: int; b: int }
type ('n, 'e) graph = {
    nodes: 'n node IntMap.t;
    edges: 'e edge IntIntMap.t;
    mutable fresh: int;
}
type 'n color = 'n * int

let empty () = {
    nodes = IntMap.empty;
    edges = IntIntMap.empty;
    fresh = 0;
}

let fresh (graph: ('n, 'e) graph): int =
    let x = graph.fresh in
    graph.fresh <- x + 1 ; x

let mk_node (lbl: 'n) (graph: ('n, 'e) graph): 'n node =
    let id = fresh graph in
    { lbl; id }

let add_node (node: 'n node) (graph: ('n, 'e) graph): ('n, 'e) graph =
    { graph with nodes = graph.nodes |> IntMap.add node.id node }

let add_edge (a: 'n node) (b: 'n node) (lbl: 'e) (graph: ('n, 'e) graph): ('n, 'e) graph =
    let a, b = min a.id b.id, max a.id b.id in
    let edge = { lbl; a; b } in
    { graph with edges = graph.edges |> IntIntMap.add (a, b) edge }

let color (k: int) (graph: ('n, 'e) graph): (('n color, 'e) graph, 'n node) result =
    failwith "unimplemented"

let graphviz
        (show_node: 'n -> string) (show_edge: 'e -> string)
        (graph: ('n, 'e) graph): string =
    "graph {\noverlap=false\n"
    ^ (IntMap.fold (fun _ node acc ->
            let text = Printf.sprintf "%d[label=\"%s\""
                node.id (show_node node.lbl) in
            text :: acc) graph.nodes [] |> String.concat "\n")
    ^ (IntIntMap.fold (fun _ edge acc ->
            let text = Printf.sprintf "%d -- %d [xlabel=\"%s\"]"
                edge.a edge.b (show_edge edge.lbl) in
            text :: acc) graph.edges [] |> String.concat "\n")
    ^ "\n}"

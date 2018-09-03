module IntSet = Set.Make(struct type t = int let compare = compare end)
module IntMap = Map.Make(struct type t = int let compare = compare end)
module IntIntMap = Map.Make(struct type t = int * int let compare = compare end)

type 'n node = { lbl: 'n; id: int }
type 'e edge = { lbl: 'e; a: int; b: int }
type ('n, 'e) graph = {
    mutable fresh: int;
    nodes: 'n node IntMap.t;
    edges: 'e edge IntIntMap.t;
    neighbors: IntSet.t IntMap.t;
}
type 'n color = {
    lbl: 'n;
    color: int;
}
type attrs = (string * string) list

let empty () = {
    fresh = 0;
    nodes = IntMap.empty;
    edges = IntIntMap.empty;
    neighbors = IntMap.empty;
}

let fresh (graph: ('n, 'e) graph): int =
    let x = graph.fresh in
    graph.fresh <- x + 1 ; x

let mk_node (lbl: 'n) (graph: ('n, 'e) graph): 'n node =
    let id = fresh graph in
    { lbl; id }

let add_node (node: 'n node) (graph: ('n, 'e) graph): ('n, 'e) graph =
    { graph with
      nodes = graph.nodes |> IntMap.add node.id node;
      neighbors = graph.neighbors |> IntMap.add node.id IntSet.empty;
    }

let add_edge (a: 'n node) (b: 'n node) (lbl: 'e) (graph: ('n, 'e) graph): ('n, 'e) graph =
    let a, b = min a.id b.id, max a.id b.id in
    let edge = { lbl; a; b } in
    { graph with
      edges = graph.edges |> IntIntMap.add (a, b) edge;
      neighbors = graph.neighbors
        |> IntMap.update a (function
            | None -> failwith (Printf.sprintf "a unreachable %d -- %d" a b)
            | Some s -> Some (s |> IntSet.add b))
        |> IntMap.update b (function
            | None -> failwith (Printf.sprintf "b unreachable %d -- %d" a b)
            | Some s -> Some (s |> IntSet.add a))
    }

let node_label ({ lbl }: 'n node): 'n = lbl
let edge_label ({ lbl }: 'e edge): 'e = lbl

let fmt_color (f: 'n -> attrs) (color: 'n color): attrs =
    let colors = [|
        "red"; "green"; "blue";
        "magenta"; "yellow"; "cyan";
        "purple"; "orange"; "turquoise";
        "sienna"; "black"; "white";
    |] in
    ("fillcolor", colors.(color.color)) :: f color.lbl

let iota n =
    let rec go acc n = if n < 0 then acc else go (n::acc) (n-1)
    in go [] (n - 1)

let option_map (f: 'a -> 'b) (x: 'a option): 'b option =
  match x with
  | None -> None
  | Some x -> Some (f x)

let color (k: int) (graph: ('n, 'e) graph): (('n color, 'e) graph, 'n node) result =
    (* Select the next node to be considered*)
    let pick_node degree =
        let node = degree |> IntMap.find_first_opt (fun id ->
            degree |> IntMap.find id < k
        ) in
        let node = match node with
          (* If there is a node with fewer than k neighbors, then it will be
             able to be assigned a color*)
        | Some node -> node |> fst
          (* If there isn't, then we need to pick an arbitrary node and hope for
             the best. *)
        | None -> (* TODO: Use a sensible heuristic *)
            degree |> IntMap.choose |> fst
        in
        let node = graph.nodes |> IntMap.find node in
        let neighbors = graph.neighbors |> IntMap.find node.id in
        let degree = IntSet.fold (fun v acc ->
            degree |> IntMap.update v (option_map (fun x -> x - 1))
        ) neighbors degree |> IntMap.remove node.id in
        degree, node
    in
    (* Create the stack of nodes to assign colors to *)
    let rec mk_stack stack degree =
        if IntMap.is_empty degree then stack
        else
        let degree, node = pick_node degree in
        mk_stack (node::stack) degree
    in
    (* Assign colors in the stack order *)
    let rec assign_colors colors stack =
        match stack with
        (* There are no more nodes to assign, we've succeeded *)
        | [] -> Ok colors
        (* Next we assign `node`, we pick a color that conflicts with no
           neighbors. *)
        | node::stack ->
            let neighbors = graph.neighbors |> IntMap.find node.id in
            let free = IntSet.fold (fun i free ->
                match colors |> IntMap.find_opt i with
                | None -> free
                | Some i -> free |> IntSet.remove i
            ) neighbors (iota k |> IntSet.of_list) in
            match IntSet.min_elt_opt free with
            | None -> Error node
            | Some color -> assign_colors (colors |> IntMap.add node.id color) stack
    in
    (* print_endline "degree" ; *)
    let degree = graph.neighbors |> IntMap.map IntSet.cardinal in
    (* print_endline "make stack" ; *)
    let stack = degree |> mk_stack [] in
    (* print_endline "assign colors" ; *)
    match stack |> assign_colors IntMap.empty with
    | Ok colors ->
        (* print_endline "color graph" ; *)
        let nodes = graph.nodes |> IntMap.mapi (fun i (node: 'n node) ->
            { node with lbl = { color = colors |> IntMap.find i; lbl = node.lbl }}) in
        Ok { graph with nodes = nodes }
    | Error node -> Error node

let graphviz
        (show_node: 'n -> attrs) (show_edge: 'e -> attrs)
        (graph: ('n, 'e) graph): string =
    let fmt attrs = attrs
        |> List.map (fun (k, v) -> Printf.sprintf "%s=\"%s\"" k v)
        |> String.concat " "
    in
    "graph {\noverlap=false;\nnode[style=\"filled\"];\n"
    ^ (IntMap.fold (fun _ { id; lbl } acc ->
            let text = match show_node lbl with
            | [] -> Printf.sprintf "%d;" id
            | attrs -> Printf.sprintf "%d[%s];" id (fmt attrs) in
            text :: acc) graph.nodes [] |> String.concat "\n")
    ^ (IntIntMap.fold (fun _ { a; b; lbl } acc ->
            let text = match show_edge lbl with
            | [] -> Printf.sprintf "%d -- %d;" a b
            | attrs -> Printf.sprintf "%d -- %d [%s];" a b (fmt attrs) in
            text :: acc) graph.edges [] |> String.concat "\n")
    ^ "\n}"

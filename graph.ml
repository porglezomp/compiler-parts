type 'n node = unit
type 'e edge = unit
type ('n, 'e) graph = unit
type 'n color = 'n * int

let empty = ()

let mk_node (label: 'n) (graph: ('n, 'e) graph): 'n node =
    failwith "unimplemented"

let mk_edge (a: 'n node) (b: 'n node) (label: 'e): 'e edge =
    failwith "unimplemented"

let add_node (node: 'n node) (graph: ('n, 'e) graph): ('n, 'e) graph =
    failwith "unimplemented"

let add_edge (edge: 'e edge) (graph: ('n, 'e) graph): ('n, 'e) graph =
    failwith "unimplemented"

let color (k: int) (graph: ('n, 'e) graph): (('n color, 'e) graph, 'n node) result =
    failwith "unimplemented"

let graphviz
        (show_node: 'n -> string) (show_edge: 'e -> string)
        (graph: ('n, 'e) graph): string =
    failwith "unimplemented"

(*
The heuristic graph coloring algorithm:

We attempt to k-color a graph by selecting nodes that we know for sure can be colored, that is, ones with fewer than k neighbors.
We remove them from consideration, and discount their edges when considering the rest of the graph, pushing them onto the coloring stack.
If we ever run out of nodes that have fewer than k neighbors, then we have to pick one arbitrarily (we can use heuristics for this).
If we get lucky, the neighbors won't use *all* of the colors, and we'll still be able to color the graph.
Once we've finished selecting all of the nodes from the graph, we walk over the coloring stack and pop each node back into the graph, assigning it a valid color that doesn't conflict with any assigned neighbors.
If we are forced to assign a node that has no available coloring, then we abort the graph coloring and report the conflicting node.

Graph operations.
Must be able to efficiently:
- Mark nodes as seen
- Track color assignments
- Track how many unvisited edges a given node has
 *)

type 'n node
type 'e edge
type ('n, 'e) graph
type 'n color

val empty : unit -> ('n, 'e) graph
val mk_node : 'n -> ('n, 'e) graph -> 'n node
val add_node : 'n node -> ('n, 'e) graph -> ('n, 'e) graph
val add_edge : 'n node -> 'n node -> 'e -> ('n, 'e) graph -> ('n, 'e) graph

val color : int -> ('n, 'e) graph -> (('n color, 'e) graph, 'n node) result
val graphviz : ('n -> string) -> ('e -> string) -> ('n, 'e) graph -> string

(*
Conflict graphs:

These graphs are constructed by taking a list of intervals and adding conflict edges between any intervals that overlap with each other.
 *)

type 'n interval = {
  label: 'n;
  start: int;
  finish: int;
}

val conflict : 'n interval list -> ('n, unit) Graph.graph

val svg : ('n -> string) -> 'n interval list -> string

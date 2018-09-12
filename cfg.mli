type id
type edge = id * id
type t

module IdMap : Map.S with type key = id
module IdSet : Set.S with type elt = id

(* *)

val make : unit -> t
val new_block : t -> id
val string_of_block : id -> string
val string_of_block_set : IdSet.t -> string

(* *)

val blocks : t -> IdSet.t
val entry : t -> id
val final : t -> id
val pred : id -> t -> IdSet.t
val succ : id -> t -> IdSet.t

(* *)

val add_block : id -> t -> t
val add_succ : id -> id -> t -> t
val add_pred : id -> id -> t -> t

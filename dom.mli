type id

module IdSet : module type of Set.Make(struct
    type t = id
    let compare = compare
  end)
module IdMap : module type of Map.Make(struct
    type t = id
    let compare = compare
  end)

type cfg

(* *)

val make : unit -> cfg
val new_block : cfg -> id
val string_of_block : id -> string
val string_of_block_set : IdSet.t -> string

(* *)

val blocks : cfg -> IdSet.t
val entry : cfg -> id
val final : cfg -> id
val pred : id -> cfg -> IdSet.t
val succ : id -> cfg -> IdSet.t

(* *)

val add_block : id -> cfg -> cfg
val add_succ : id -> id -> cfg -> cfg
val add_pred : id -> id -> cfg -> cfg

(* *)

val dominators : cfg -> IdSet.t IdMap.t
val backedges : IdSet.t IdMap.t -> cfg -> (id * id) list
val graphviz : cfg -> string

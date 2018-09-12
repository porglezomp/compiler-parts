module type CFG = sig
  type id
  type edge = id * id
  type t

  module IdMap : Map.S with type key = id
  module IdSet : Set.S with type elt = id

  (* *)

  val string_of_block : id -> string
  val string_of_block_set : IdSet.t -> string

  (* *)

  val blocks : t -> IdSet.t
  val entry : t -> id
  val pred : id -> t -> IdSet.t
  val succ : id -> t -> IdSet.t
end

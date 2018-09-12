type name
type var
type block_id
type edge_id

module BlockSet : module type of Set.Make(struct
    type t = block_id
    let compare = compare
  end)

module EdgeSet : module type of Set.Make(struct
    type t = edge_id
    let compare = compare
  end)

type addr
  = Stack of int
  | Mem of var * int

type op
  = Int of int
  | Var of var
  | Add of var * var
  | Sub of var * var
  | Mul of var * var
  | Div of var * var
  | Lt of var * var
  | Le of var * var
  | Eq of var * var
  | Not of var
  | Load of addr
  | Call of name * var list

type instr
  = Assign of var * op
  | Store of addr * var

type succ
  = Goto of block_id
  | GotoIf of var * block_id * block_id
  | Return of var

type def

(* *)

val new_def : unit -> def
val new_var : def -> var
val new_block : def -> block_id

(* *)

val focus : block_id -> def -> def
val add_instr : instr -> def -> def
val set_succ : succ -> def -> def
val add_param : var -> def -> def

(* *)

val blocks : def -> BlockSet.t
val entry : def -> block_id
val focused : def -> block_id

val succ_instr : block_id -> def -> succ option
val pred : block_id -> def -> BlockSet.t
val succ : block_id -> def -> BlockSet.t
val pred_edges : block_id -> def -> EdgeSet.t
val succ_edges : block_id -> def -> EdgeSet.t

(* *)

val edge_src : edge_id -> def -> block_id
val edge_dst : edge_id -> def -> block_id

(* *)

val remove_empty_blocks : def -> def

(* *)

val string_of_block : block_id -> string
val string_of_block_set : BlockSet.t -> string
val graphviz : def -> string

module Cfg : S.CFG

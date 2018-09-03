type name
type var
type block_id

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

type block
type def

val new_def : unit -> def
val new_var : def -> var
val new_block : def -> block

val add_instr : instr -> block -> block
val set_succ : succ -> block -> block
val block_id : block -> block_id

val add_block : block -> def -> def
val add_param : var -> def -> def
val set_entry : block -> def -> def

val graphviz : def -> string

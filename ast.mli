type var = V of int

type aexp
  = ToInt of bexp
  | Int of int
  | Var of var
  | Add of aexp * aexp
  | Sub of aexp * aexp
  | Mul of aexp * aexp
  | Div of aexp * aexp
  | Read of aexp
and bexp
  = Bool of bool
  | Lt of aexp * aexp
  | Le of aexp * aexp
  | Eq of aexp * aexp
  | Not of bexp
  | And of bexp * bexp

type stmt
  = While of bexp * stmt list
  | For of var * aexp * aexp * stmt list
  | If of bexp * stmt list * stmt list
  | Assign of var * aexp
  | Write of aexp * aexp
  | Return of aexp

type def = {
  name: string;
  params: var list;
  vars: var list;
  body: stmt list;
}

val compile : def -> Tac.def

val string_of_aexp : aexp -> string
val string_of_bexp : bexp -> string
val string_of_stmt : stmt -> string
val string_of_def : def -> string

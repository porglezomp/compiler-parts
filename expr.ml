type var = V of int

type aexp
  = ToInt of bexp
  | Int of int
  | Var of var
  | Add of aexp * aexp
  | Sub of aexp * aexp
  | Mul of aexp * aexp
  | Div of aexp * aexp
and bexp
  = Bool of bool
  | Lt of aexp * aexp
  | Le of aexp * aexp
  | Eq of aexp * aexp
  | Not of bexp
  | And of bexp * bexp

type stmt
  = While of bexp * stmt list
  | If of bexp * stmt list * stmt list
  | Assign of var * aexp
  | Return of aexp

type def = {
  name: string;
  params: var list;
  body: stmt list;
}

let a_prec (exp: aexp): int =
  match exp with
  | Var _ | Int _ | ToInt _ -> 30
  | Mul _ | Div _ -> 20
  | Add _ | Sub _ -> 10

let rec string_of_aexp (exp: aexp): string =
  let p = a_prec exp in
  match exp with
  | ToInt b -> Printf.sprintf "int(%s)" (string_of_bexp b)
  | Int i -> string_of_int i
  | Var (V x) -> Printf.sprintf "x%d" x
  | Add (l, r) ->
    Printf.sprintf "%s + %s" (pastring p l) (pastring (p+1) r)
  | Sub (l, r) ->
    Printf.sprintf "%s - %s" (pastring p l) (pastring (p+1) r)
  | Mul (l, r) ->
    Printf.sprintf "%s * %s" (pastring p l) (pastring (p+1) r)
  | Div (l, r) ->
    Printf.sprintf "%s / %s" (pastring p l) (pastring (p+1) r)
and pastring (prec: int) (exp: aexp): string =
  let prec' = a_prec exp in
  let text = string_of_aexp exp in
  if prec > prec' then "(" ^ text ^ ")" else text
and string_of_bexp (exp: bexp): string =
  match exp with
  | Bool true -> "true"
  | Bool false -> "false"
  | Lt (l, r) ->
    Printf.sprintf "%s < %s" (string_of_aexp l) (string_of_aexp r)
  | Le (l, r) ->
    Printf.sprintf "%s <= %s" (string_of_aexp l) (string_of_aexp r)
  | Eq (l, r) ->
    Printf.sprintf "%s = %s" (string_of_aexp l) (string_of_aexp r)
  | Not b -> Printf.sprintf "not (%s)" (string_of_bexp b)
  | And (l, r) ->
    Printf.sprintf "%s and %s" (string_of_bexp l) (string_of_bexp r)


let indent (width: int) (text: string): string =
  let pad = String.make width ' ' in
  pad ^ Str.global_replace (Str.regexp_string "\n") ("\n" ^ pad) text

let rec string_of_stmt (stmt: stmt): string =
  match stmt with
  | While (b, s) ->
    Printf.sprintf "while %s do
%s
end"
      (string_of_bexp b) (string_of_stmt_list s |> indent 2)
  | If (c, t, f) ->
    Printf.sprintf "if %s then
%s
else
%s
end"
      (string_of_bexp c)
      (string_of_stmt_list t |> indent 2)
      (string_of_stmt_list f |> indent 2)
  | Assign (V x, e) ->
    Printf.sprintf "x%d := %s" x (string_of_aexp e)
  | Return e ->
    Printf.sprintf "return %s" (string_of_aexp e)
and string_of_stmt_list (stmts: stmt list): string =
  stmts |> List.map string_of_stmt |> String.concat "\n"

let string_of_def (def: def): string =
  Printf.sprintf "def %s(%s)
%s
end"
    def.name
    (def.params
     |> List.map (fun (V x) -> Printf.sprintf "x%d" x)
     |> String.concat ", ")
    (string_of_stmt_list def.body |> indent 2)

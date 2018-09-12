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

module VarMap = Map.Make(struct type t = var let compare = compare end)
type var_map = Tac.var VarMap.t

let binop (comp: Tac.def -> Tac.var -> 'e -> Tac.def)
    (tac: Tac.def) (dest: Tac.var) (l: 'e) (r: 'e)
    (op : Tac.var -> Tac.var -> Tac.op): Tac.def =
  let ldest = Tac.new_var tac in
  let state = l |> comp tac ldest in
  let tac = r |> comp state dest in
  let tac = tac |> Tac.add_instr (Tac.Assign (dest, op ldest dest)) in
  tac

let rec compile_aexp
    (vars: var_map) (tac: Tac.def) (dest: Tac.var) (exp: aexp): Tac.def =
  let abinop = abinop vars tac dest in
  match exp with
  | ToInt b -> compile_bexp vars tac dest b
  | Int i ->
    let tac = tac |> Tac.add_instr (Tac.Assign (dest, Tac.Int i)) in
    tac
  | Var v ->
    let v = vars |> VarMap.find v in
    if v = dest then
      tac
    else
      tac |> Tac.add_instr (Tac.Assign (dest, Tac.Var v))
  | Add (l, r) -> abinop l r (fun l r -> Tac.Add (l, r))
  | Sub (l, r) -> abinop l r (fun l r -> Tac.Sub (l, r))
  | Mul (l, r) -> abinop l r (fun l r -> Tac.Mul (l, r))
  | Div (l, r) -> abinop l r (fun l r -> Tac.Div (l, r))
  | Read addr ->
    let tac = addr |> compile_aexp vars tac dest in
    let tac = tac |> Tac.add_instr (Tac.Assign (dest, Load (Mem (dest, 0)))) in
    tac
and compile_bexp
    (vars: var_map) (tac: Tac.def) (dest: Tac.var) (exp: bexp): Tac.def =
  match exp with
  | Bool b -> compile_aexp vars tac dest (Int (if b then 1 else 0))
  | Lt (l, r) -> abinop vars tac dest l r (fun l r -> Tac.Lt (l, r))
  | Le (l, r) -> abinop vars tac dest l r (fun l r -> Tac.Le (l, r))
  | Eq (l, r) -> abinop vars tac dest l r (fun l r -> Tac.Eq (l, r))
  | Not b -> compile_aexp vars tac dest (Sub (Int 1, ToInt b))
  | And (l, r) -> bbinop vars tac dest l r (fun l r -> Tac.Mul (l, r))
and abinop vars tac dest l r op: Tac.def =
  binop (compile_aexp vars) tac dest l r op
and bbinop vars tac dest l r op: Tac.def =
  binop (compile_bexp vars) tac dest l r op

let rec compile_stmt (vars: Tac.var VarMap.t) (tac: Tac.def) (stmt: stmt): Tac.def =
  match stmt with
  | While (c, b) ->
    let block = Tac.focused tac in

    let cond = Tac.new_var tac in
    let head = Tac.new_block tac in
    let tac = tac |> Tac.focus head in
    let tac = c |> compile_bexp vars tac cond in
    let head = Tac.focused tac in

    let body_head = Tac.new_block tac in
    let tac = tac |> Tac.focus body_head in
    let tac = b |> List.fold_left (compile_stmt vars) tac in
    let body = Tac.focused tac in

    let tail = Tac.new_block tac in
    let tac = Tac.(
        tac
        |> focus block |> set_succ (Goto head)
        |> focus head |> set_succ (GotoIf (cond, body_head, tail))
        |> focus body |> set_succ (Goto head)
        |> focus tail
      ) in
    tac
  | For (i, b, e, s) ->
    let tac = compile_stmt vars tac (Assign (i, b)) in
    let cond = Lt(Var i, e) in
    let body = s @ [Assign (i, Add(Var i, Int 1))] in
    compile_stmt vars tac (While (cond, body))
  | If (c, t, f) ->
    let cond = Tac.new_var tac in
    let tac = c |> compile_bexp vars tac cond in
    let block = Tac.focused tac in

    let tac = tac |> Tac.focus (Tac.new_block tac) in
    let tac = t |> List.fold_left (compile_stmt vars) tac in
    let t_block = Tac.focused tac in

    let tac = tac |> Tac.focus (Tac.new_block tac) in
    let tac = f |> List.fold_left (compile_stmt vars) tac in
    let f_block = Tac.focused tac in

    let tail = Tac.new_block tac in
    Tac.(
      tac
      |> focus block |> set_succ (GotoIf (cond, t_block, f_block))
      |> focus t_block |> set_succ (Goto tail)
      |> focus f_block |> set_succ (Goto tail)
      |> focus tail
    )
  | Assign (v, e) ->
    let var = vars |> VarMap.find v in
    e |> compile_aexp vars tac var
  | Write (a, e) ->
    let dst = Tac.new_var tac in
    let tac = a |> compile_aexp vars tac dst in
    let res = Tac.new_var tac in
    let tac = e |> compile_aexp vars tac res in
    tac |> Tac.add_instr (Store (Mem (dst, 0), res))
  | Return e ->
    let res = Tac.new_var tac in
    let tac = e |> compile_aexp vars tac res in
    let tac = tac |> Tac.set_succ (Tac.Return res) in
    (* After the return, we're in an unreachable block. *)
    tac |> Tac.focus (Tac.new_block tac)

exception InvalidSuccessor of Tac.block_id * Tac.def
let compile (def: def): Tac.def =
  let open Tac in
  let tac = new_def () in
  let vars = (def.params @ def.vars) |> List.fold_left (fun vars param ->
      vars |> VarMap.add param (new_var tac)
    ) VarMap.empty in
  let body = match List.rev def.body with
    | Return _ :: _ -> def.body
    | _ -> def.body @ [Return (Int 0)]
  in
  let tac = body |> List.fold_left (compile_stmt vars) tac in
  tac |> remove_empty_blocks

let a_prec (exp: aexp): int =
  match exp with
  | Var _ | Int _ | ToInt _ -> 40
  | Read _ -> 30
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
  | Read addr ->
    Printf.sprintf "[%s]" (string_of_aexp addr)
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
  | For (V i, b, e, s) ->
    Printf.sprintf "for x%d := %s to %s do
%s
end"
      i
      (string_of_aexp b)
      (string_of_aexp e)
      (string_of_stmt_list s |> indent 2)
  | Assign (V x, e) ->
    Printf.sprintf "x%d := %s" x (string_of_aexp e)
  | Write (dst, e) ->
    Printf.sprintf "[%s] := %s" (string_of_aexp dst) (string_of_aexp e)
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

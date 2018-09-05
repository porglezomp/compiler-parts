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
  vars: var list;
  body: stmt list;
}

module VarMap = Map.Make(struct type t = var let compare = compare end)
type compile_state = Tac.def * Tac.var VarMap.t * Tac.block * bool

let binop (comp: compile_state -> Tac.var -> 'e -> compile_state)
    (state: compile_state) (dest: Tac.var) (l: 'e) (r: 'e)
    (op : Tac.var -> Tac.var -> Tac.op): compile_state =
  let tac, _, _, _ = state in
  let ldest = Tac.new_var tac in
  let state = l |> comp state ldest in
  let tac, vars, block, alive = r |> comp state dest in
  let block = block |> Tac.add_instr (Tac.Assign (dest, op ldest dest)) in
  tac, vars, block, alive

let rec compile_aexp
    (state: compile_state) (dest: Tac.var) (exp: aexp): compile_state =
  let tac, vars, block, alive = state in
  match exp with
  | ToInt b -> compile_bexp state dest b
  | Int i ->
    let block = block |> Tac.add_instr (Tac.Assign (dest, Tac.Int i)) in
    tac, vars, block, alive
  | Var v ->
    let v = vars |> VarMap.find v in
    if v = dest then
      state
    else
      let block = block |> Tac.add_instr (Tac.Assign (dest, Tac.Var v)) in
      tac, vars, block, alive
  | Add (l, r) -> abinop state dest l r (fun l r -> Tac.Add (l, r))
  | Sub (l, r) -> abinop state dest l r (fun l r -> Tac.Sub (l, r))
  | Mul (l, r) -> abinop state dest l r (fun l r -> Tac.Mul (l, r))
  | Div (l, r) -> abinop state dest l r (fun l r -> Tac.Div (l, r))
and compile_bexp
    (state: compile_state) (dest: Tac.var) (exp: bexp): compile_state =
  match exp with
  | Bool b -> compile_aexp state dest (Int (if b then 1 else 0))
  | Lt (l, r) -> abinop state dest l r (fun l r -> Tac.Lt (l, r))
  | Le (l, r) -> abinop state dest l r (fun l r -> Tac.Le (l, r))
  | Eq (l, r) -> abinop state dest l r (fun l r -> Tac.Eq (l, r))
  | Not b -> compile_aexp state dest (Sub (Int 1, ToInt b))
  | And (l, r) -> bbinop state dest l r (fun l r -> Tac.Mul (l, r))
and abinop state dest l r op: compile_state =
  binop compile_aexp state dest l r op
and bbinop state dest l r op: compile_state =
  binop compile_bexp state dest l r op

let rec compile_stmt (state: compile_state) (stmt: stmt): compile_state =
  let tac, vars, block, alive = state in
  match stmt with
  | While (c, b) ->
    let cond = Tac.new_var tac in
    let head = Tac.new_block tac in
    let tac, _, head, _ = c |> compile_bexp (tac, vars, head, true) cond in

    let body_head = Tac.new_block tac in
    let tac, _, body, _ = b |> List.fold_left compile_stmt
                         (tac, vars, body_head, true) in

    let tail = Tac.new_block tac in
    let tac = Tac.(
        tac
        |> add_block (block |> set_succ (Goto (block_id head)))
        |> add_block (head |> set_succ
                        (GotoIf (cond, block_id body_head, block_id tail)))
        |> add_block (body |> set_succ (Goto (block_id head)))
      ) in
    tac, vars, tail, true
  | If (c, t, f) ->
    let cond = Tac.new_var tac in
    let tac, _, block, _ = c |> compile_bexp state cond in

    let t_block = Tac.new_block tac in
    let tac, _, t_block, _ =
      t |> List.fold_left compile_stmt (tac, vars, t_block, true) in

    let f_block = Tac.new_block tac in
    let tac, _, f_block, _ =
      f |> List.fold_left compile_stmt (tac, vars, f_block, true) in

    let tail = Tac.new_block tac in
    let tac = Tac.(
        tac
        |> add_block (block |> set_succ
                        (GotoIf (cond, block_id t_block, block_id f_block)))
        |> add_block (t_block |> set_succ (Goto (block_id tail)))
        |> add_block (f_block |> set_succ (Goto (block_id tail)))
      ) in
    tac, vars, tail, true
  | Assign (v, e) ->
    let var = vars |> VarMap.find v in
    e |> compile_aexp state var
  | Return e ->
    let res = Tac.new_var tac in
    let tac, vars, block, alive = e |> compile_aexp state res in
    let block = block |> Tac.set_succ (Tac.Return res) in
    let tac = tac |> Tac.add_block block in
    (* After the return, we're in an unreachable block. *)
    let block = Tac.new_block tac in
    tac, vars, block, false

exception InvalidSuccessor of Tac.block * Tac.def
let compile (def: def): Tac.def =
  let open Tac in
  let tac = new_def () in
  let vars = (def.params @ def.vars) |> List.fold_left (fun vars param ->
      vars |> VarMap.add param (new_var tac)
    ) VarMap.empty in
  let entry = new_block tac |> set_tag "Entry" in
  let tac = tac |> set_entry entry in
  let tac, _, block, alive =
    def.body |> List.fold_left compile_stmt (tac, vars, entry, true) in
  match succ block with
  | Some (Return _) -> tac |> add_block block
  | Some _ -> raise (InvalidSuccessor (block, tac))
  | None when not alive -> tac
  | None ->
    let res = new_var tac in
    tac |> add_block (block
                      |> add_instr (Assign (res, Int 0))
                      |> set_succ (Return res))

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

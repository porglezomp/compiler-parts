type name = Name of string
type var = Var of int
type block_id = BlockId of int

module IdMap = Map.Make(struct
    type t = block_id
    let compare = compare
  end)

module IdSet = Set.Make(struct
    type t = block_id
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

type block = {
  block_id: block_id;
  (* The instruction list is backwards, the last instruction is at the
     front of the list (for more efficient construction). *)
  instrs: instr list;
  succ: succ option;
  tag: string;
}

type def = {
  mutable fresh_var: int;
  mutable fresh_block: int;
  params: var list;
  blocks: block IdMap.t;
  entry: block_id option;
}

let new_def () = {
  fresh_var = 0;
  fresh_block = 0;
  params = [];
  blocks = IdMap.empty;
  entry = None;
}

let new_var (def: def): var =
  let var = def.fresh_var in
  def.fresh_var <- var + 1 ;
  Var var

let new_block (def: def): block =
  let id = def.fresh_block in
  def.fresh_block <- id + 1 ;
  {
    block_id = BlockId id;
    instrs = [];
    succ = None;
    tag = "";
  }

let add_instr (instr: instr) (block: block): block =
  { block with instrs = instr :: block.instrs }

let set_succ (succ: succ) (block: block): block =
  { block with succ = Some succ }

let set_tag (tag: string) (block: block): block =
  { block with tag }

let block_id (block: block): block_id =
  block.block_id

let succ (block: block): succ option =
  block.succ

let add_block (block: block) (def: def): def =
  { def with blocks = def.blocks |> IdMap.add (block_id block) block }

let add_param (var: var) (def: def): def =
  assert (not (def.params |> List.mem var)) ;
  { def with params = var :: def.params }

let set_entry (block: block) (def: def): def =
  { def with entry = Some (block_id block) }

let graphviz (def: def): string =
  let fmt_op op =
    match op with
    | Int i -> string_of_int i
    | Var (Var x) -> Printf.sprintf "x%d" x
    | Add (Var l, Var r) -> Printf.sprintf "x%d + x%d" l r
    | Sub (Var l, Var r) -> Printf.sprintf "x%d - x%d" l r
    | Mul (Var l, Var r) -> Printf.sprintf "x%d * x%d" l r
    | Div (Var l, Var r) -> Printf.sprintf "x%d / x%d" l r
    | Lt (Var l, Var r) -> Printf.sprintf "x%d &lt; x%d" l r
    | Le (Var l, Var r) -> Printf.sprintf "x%d &lt;= x%d" l r
    | Eq (Var l, Var r) -> Printf.sprintf "x%d = x%d" l r
    | Not (Var x) -> Printf.sprintf "not x%d" x
    | Load (Stack idx) -> Printf.sprintf "load stack[%d]" idx
    | Load (Mem (Var x, idx)) -> Printf.sprintf "load x%d[%d]" x idx
    | Call (Name name, vars) ->
      Printf.sprintf "call %s(%s)" name
        (vars
         |> List.map (fun (Var x: var) -> Printf.sprintf "x%d" x)
         |> String.concat ", ")
  in
  let fmt_instr instr =
    match instr with
    | Assign (Var dst, src) -> Printf.sprintf "x%d := %s" dst (fmt_op src)
    | Store (Stack slot, Var src) ->
      Printf.sprintf "store stack[%d] := x%d" slot src
    | Store (Mem (Var dst, idx), Var src) ->
      Printf.sprintf "store x%d[%d] := x%d" dst idx src

  in
  let fmt_succ succ =
    match succ with
    | None -> "!!! NO SUCCESSOR !!!"
    | Some (Goto (BlockId id)) -> Printf.sprintf "goto bb%d\\l" id
    | Some (GotoIf (Var x, BlockId t, BlockId f)) ->
      Printf.sprintf "goto if x%d bb%d bb%d\\l|{<true> T |<false> F}" x t f
    | Some (Return (Var x)) -> Printf.sprintf "return x%d\\l" x
  in
  let fmt_block block =
    let (BlockId id) = block.block_id in
    let instrs = block.instrs |> List.map fmt_instr in
    let succ = fmt_succ block.succ in
    Printf.sprintf "%d[label=\"{%sbb%d|%s%s}\"]"
      id (if block.tag <> "" then block.tag ^ ": " else "") id
      (if def.entry = Some block.block_id && def.params <> [] then
         "(" ^ (def.params |> List.map (fun (Var v: var) -> Printf.sprintf "x%d" v) |> String.concat ", ") ^ ")|"
       else
         "")
      (succ :: instrs |> List.rev |> String.concat "\\l")
  in
  let rec build_edges edges to_visit id =
    if to_visit |> IdSet.is_empty then
      edges
    else if to_visit |> IdSet.mem id then
      let to_visit = to_visit |> IdSet.remove id in
      let block = def.blocks |> IdMap.find id in
      let (BlockId from) = id in
      let edges = match block.succ with
        | None | Some (Return _) -> edges
        | Some (Goto (BlockId next)) ->
          Printf.sprintf "%d -> %d" from next
          :: edges
        | Some (GotoIf (Var x, BlockId t, BlockId f)) ->
          Printf.sprintf "%d:true -> %d" from t
          :: Printf.sprintf "%d:false -> %d" from f
          :: edges
      in
      build_edges edges to_visit id
    else
      let id = to_visit |> IdSet.choose in
      build_edges edges to_visit id
  in
  let nodes = IdMap.fold (fun _ block acc ->
      fmt_block block :: acc
    ) def.blocks [] in
  let to_visit = IdMap.fold (fun k _ -> IdSet.add k)
      def.blocks IdSet.empty in
  let edges = match def.blocks |> IdMap.choose_opt with
    | None -> []
    | Some (id, _) ->  build_edges [] to_visit id
  in
  "digraph {
overlap=false;
node [shape=\"record\"]
" ^ (nodes |> String.concat "\n")
  ^ (edges |> String.concat "\n")
  ^ "\n}"

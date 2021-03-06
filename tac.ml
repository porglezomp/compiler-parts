type name = Name of string
type var = Var of int
type block_id = Block of int
type edge_id = Edge of int

module BlockMap = Map.Make(struct type t = block_id let compare = compare end)
module BlockSet = Set.Make(struct type t = block_id let compare = compare end)
module EdgeMap = Map.Make(struct type t = edge_id let compare = compare end)
module EdgeSet = Set.Make(struct type t = edge_id let compare = compare end)
module VarMap = Map.Make(struct type t = var let compare = compare end)

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
}

type edge = {
  edge_id: edge_id;
  src: block_id;
  dst: block_id;
}

type def = {
  mutable fresh_var: int;
  mutable fresh_block: int;
  mutable fresh_edge: int;
  focus_id: block_id;
  params: var list;
  block_ids: BlockSet.t;
  blocks: block BlockMap.t;
  edges: edge EdgeMap.t;
  succ_edges: EdgeSet.t BlockMap.t;
  pred_edges: EdgeSet.t BlockMap.t;
  current_def: var BlockMap.t VarMap.t;
  sealed: BlockSet.t;
  filled: BlockSet.t;
}

(* Helpful for debugging *)

let string_of_block (Block id: block_id): string =
  match id with
  | -1 -> "Final"
  | 0 -> "Entry"
  | id -> Printf.sprintf "BB%d" id

let string_of_block_set (set: BlockSet.t): string =
  Printf.sprintf "{%s}"
    ([]
     |> BlockSet.fold (fun block set -> string_of_block block :: set) set
     |> String.concat ", ")

(* *)

let new_def (): def =
  let block_ids = BlockSet.of_list [Block (-1); Block 0] in
  let blocks =
    BlockMap.empty
    |> BlockMap.add (Block 0) {
      block_id = Block 0;
      instrs = []; succ = None;
    }
    |> BlockMap.add (Block (-1)) {
      block_id = Block (-1);
      instrs = []; succ = None;
    }
  in
  let edges = EdgeMap.empty in
  {
    fresh_var = 0;
    fresh_block = 1;
    fresh_edge = 0;
    focus_id = Block 0;
    params = [];
    block_ids; blocks; edges;
    succ_edges = BlockMap.empty;
    pred_edges = BlockMap.empty;
    current_def = VarMap.empty;
    sealed = BlockSet.singleton (Block 0);
    filled = BlockSet.empty;
  }

let new_var (def: def): var =
  let var = def.fresh_var in
  def.fresh_var <- var + 1 ;
  Var var

let new_block (def: def): block_id =
  let id = def.fresh_block in
  def.fresh_block <- id + 1 ;
  Block id

(* *)

let focus (focus_id: block_id) (def: def): def =
  let blocks = def.blocks |> BlockMap.update focus_id (function
      | None -> Some { block_id = focus_id; instrs = []; succ = None }
      | x -> x)
  in
  let block_ids = def.block_ids |> BlockSet.add focus_id in
  { def with focus_id; blocks; block_ids }

let add_instr (instr: instr) (def: def): def =
  assert (not (def.filled |> BlockSet.mem def.focus_id)) ;
  let blocks = def.blocks |> BlockMap.update def.focus_id (function
      | None -> failwith "invariant failure in add_instr, no block focused"
      | Some block -> Some { block with instrs = instr :: block.instrs })
  in
  { def with blocks }

let set_succ (succ: succ) (def: def): def =
  let add_edge src dst edges =
    let id = def.fresh_edge in
    def.fresh_edge <- id + 1 ;
    let edge = { edge_id = Edge id; src; dst } in
    let edges = edges |> EdgeMap.add (Edge id) edge in
    Edge id, edges
  in
  let block = def.focus_id in
  let blocks = def.blocks |> BlockMap.update block (function
      | None -> failwith "invariant failure in set_succ, no block focused"
      | Some block -> Some { block with succ = Some succ })
  in
  let succ = match succ with | Return _ -> Goto (Block (-1)) | _ -> succ in
  (* TODO: fix to handle already set successor *)
  let succ_edges, pred_edges, edges = match succ with
    | Goto next ->
      assert (not (def.sealed |> BlockSet.mem next)) ;
      let edge, edges = def.edges |> add_edge block next in
      let succ_edges =
        def.succ_edges
        |> BlockMap.add block (EdgeSet.singleton edge)
      in
      let pred_edges = def.pred_edges |> BlockMap.update next (function
          | None -> Some (EdgeSet.singleton edge)
          | Some pred -> Some (pred |> EdgeSet.add edge))
      in
      succ_edges, pred_edges, edges
    | GotoIf (_, t, f) ->
      assert (not (def.sealed |> BlockSet.mem t)) ;
      assert (not (def.sealed |> BlockSet.mem f)) ;
      let t_edge, edges = def.edges |> add_edge block t in
      let f_edge, edges = edges |> add_edge block f in
      let succ_edges =
        def.succ_edges
        |> BlockMap.add block (EdgeSet.of_list [t_edge; f_edge])
      in
      let pred_edges =
        def.pred_edges
        |> BlockMap.update t (function
            | None -> Some (EdgeSet.singleton t_edge)
            | Some pred -> Some (pred |> EdgeSet.add t_edge)
          )
        |> BlockMap.update f (function
            | None -> Some (EdgeSet.singleton f_edge)
            | Some pred -> Some (pred |> EdgeSet.add f_edge)
          )
      in
      succ_edges, pred_edges, edges
    | Return _ -> failwith "Return case unreachable"
  in
  { def with blocks; edges; succ_edges; pred_edges }


let add_param (var: var) (def: def): def =
  assert (not (def.params |> List.mem var)) ;
  { def with params = var :: def.params }

let seal (id: block_id) (def: def): def =
  { def with sealed = def.sealed |> BlockSet.add id }

let fill (id: block_id) (def: def): def =
  { def with filled = def.filled |> BlockSet.add id }

(* *)

let blocks (def: def): BlockSet.t = def.block_ids
let entry (def: def): block_id = Block 0
let final (def: def): block_id = Block (-1)
let focused (def: def): block_id = def.focus_id

let pred_edges (id: block_id) (def: def): EdgeSet.t =
  try def.pred_edges |> BlockMap.find id
  with Not_found -> EdgeSet.empty

let succ_edges (id: block_id) (def: def): EdgeSet.t =
  try def.succ_edges |> BlockMap.find id
  with Not_found -> EdgeSet.empty

let edge_src (id: edge_id) (def: def): block_id =
  flush_all () ;
  (def.edges |> EdgeMap.find id).src

let edge_dst (id: edge_id) (def: def): block_id =
  (def.edges |> EdgeMap.find id).dst

let pred (id: block_id) (def: def): BlockSet.t =
  BlockSet.empty |> EdgeSet.fold (fun edge set ->
      set |> BlockSet.add (def |> edge_src edge)
    ) (def |> pred_edges id)

let succ (id: block_id) (def: def): BlockSet.t =
  BlockSet.empty |> EdgeSet.fold (fun edge set ->
      set |> BlockSet.add (def |> edge_dst edge)
    ) (def |> succ_edges id)

let succ_instr (id: block_id) (def: def): succ option =
  (def.blocks |> BlockMap.find id).succ

(* *)

let remove_empty_blocks (def: def): def =
  { def with
    blocks = def.blocks |> BlockMap.filter (fun id block -> not (
        block.instrs = []
        && block.block_id <> Block (-1)
        && block.succ = None
        && (try def.succ_edges |> BlockMap.find id |> EdgeSet.is_empty
            with Not_found -> true)
        && (try def.pred_edges |> BlockMap.find id |> EdgeSet.is_empty
            with Not_found -> true)
      ));
  }


let validate (def: def): (def, string) result =
  (* TODO: Actual validation here *)
  Ok def

(* *)

module Cfg = struct
  type id = block_id
  type edge = id * id
  type t = def

  module IdMap = BlockMap
  module IdSet = BlockSet

  let string_of_block = string_of_block
  let string_of_block_set = string_of_block_set

  let blocks = blocks
  let entry = entry
  let final = final
  let pred = pred
  let succ = succ
end

(* *)

let graphviz (def: def): string =
  let module Dom = Dom.Make(Cfg) in
  let dom = Dom.dom def in
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
    | Some (Goto block) -> Printf.sprintf "goto %s\\l" (string_of_block block)
    | Some (GotoIf (Var x, t, f)) ->
      Printf.sprintf "goto if x%d %s %s\\l|{<true> T |<false> F}" x
        (string_of_block t) (string_of_block f)
    | Some (Return (Var x)) -> Printf.sprintf "return x%d\\l" x
  in
  let fmt_block block =
    let instrs = block.instrs |> List.map fmt_instr in
    let succ = fmt_succ block.succ in
    match block.block_id with
    | Block (-1) ->
      Printf.sprintf "-1[shape=oval,label=\"%s\"]"
        (string_of_block block.block_id)
    | Block 0 ->
      Printf.sprintf "0[label=\"{%s|%s%s}\"]"
        (string_of_block block.block_id)
        (if def.params <> [] then
           "(" ^ (def.params |> List.map (fun (Var v: var) -> Printf.sprintf "x%d" v) |> String.concat ", ") ^ ")|"
         else "")
        (succ :: instrs |> List.rev |> String.concat "\\l")
    | Block id ->
      Printf.sprintf "%d[label=\"{%s|%s}\"]"
        id
        (string_of_block block.block_id)
        (succ :: instrs |> List.rev |> String.concat "\\l")
  in
  let rec build_edges edges to_visit id =
    if to_visit |> BlockSet.is_empty then
      edges
    else if to_visit |> BlockSet.mem id then
      let to_visit = to_visit |> BlockSet.remove id in
      let block = def.blocks |> BlockMap.find id in
      let (Block from) = id in
      let fmt_edge a b label =
        let is_dom = try
            dom |> Cfg.IdMap.find (Block a) |> Cfg.IdSet.mem (Block b)
          with Not_found -> false
        in
        if is_dom then
          Printf.sprintf "%d -> %d%s [dir=back, penwidth=2]" b a label
        else
          Printf.sprintf "%d%s -> %d" a label b
      in
      let edges = match block.succ with
        | None -> edges
        | Some (Return _) ->
          fmt_edge from (-1) ""
          :: edges
        | Some (Goto (Block next)) ->
          fmt_edge from next ""
          :: edges
        | Some (GotoIf (Var x, Block t, Block f)) ->
          fmt_edge from t ":true"
          :: fmt_edge from f ":false"
          :: edges
      in
      build_edges edges to_visit id
    else
      let id = to_visit |> BlockSet.choose in
      build_edges edges to_visit id
  in
  let nodes = BlockMap.fold (fun (Block id) block acc ->
      fmt_block block :: acc
    ) def.blocks [] in
  let to_visit = BlockMap.fold (fun k _ -> BlockSet.add k)
      def.blocks BlockSet.empty in
  let edges = match def.blocks |> BlockMap.choose_opt with
    | None -> []
    | Some (id, _) ->  build_edges [] to_visit id
  in
  "digraph {
overlap=false;
node [shape=\"record\"]
" ^ (nodes |> String.concat "\n")
  ^ (edges |> String.concat "\n")
  ^ "\n}"

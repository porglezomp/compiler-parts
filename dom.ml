type id = Id of int

module IdSet = Set.Make(struct type t = id let compare = compare end)
module IdMap = Map.Make(struct type t = id let compare = compare end)

type cfg = {
  mutable fresh: id;
  entry: id;
  final: id;
  blocks: IdSet.t;
  succ: IdSet.t IdMap.t;
  pred: IdSet.t IdMap.t;
}

let make (): cfg =
  let entry = Id 0 in
  let final = Id 1 in
  {
    fresh = Id 2;
    entry; final;
    blocks = IdSet.empty;
    succ = IdMap.empty;
    pred = IdMap.empty;
  }

let new_block (cfg: cfg): id =
  let Id id = cfg.fresh in
  cfg.fresh <- Id (id + 1) ;
  Id id

let string_of_block (Id block: id): string =
  match block with
  | 0 -> "Entry"
  | 1 -> "Exit"
  | n -> Printf.sprintf "BB%d" (n-1)

let string_of_block_set (blocks: IdSet.t): string =
  Printf.sprintf "{%s}"
  ([] |> IdSet.fold (fun block list ->
        string_of_block block :: list
      ) blocks |> String.concat ", ")

(* *)

let blocks (cfg: cfg): IdSet.t =
  cfg.blocks

let entry (cfg: cfg): id =
  cfg.entry

let final (cfg: cfg): id =
  cfg.final

let pred (block: id) (cfg: cfg): IdSet.t =
  try cfg.pred |> IdMap.find block
  with Not_found -> IdSet.empty

let succ (block: id) (cfg: cfg): IdSet.t =
  try cfg.succ |> IdMap.find block
  with Not_found -> IdSet.empty

(* *)

let add_block (block: id) (cfg: cfg): cfg =
  { cfg with
    blocks = cfg.blocks |> IdSet.add block;
  }

let add_succ (block: id) (other: id) (cfg: cfg): cfg =
  let cfg = cfg |> add_block block |> add_block other in
  { cfg with
    pred = cfg.pred |> IdMap.add other (cfg |> pred other |> IdSet.add block);
    succ = cfg.succ |> IdMap.add block (cfg |> succ block |> IdSet.add other);
  }

let add_pred (block: id) (other: id) (cfg: cfg): cfg =
  cfg |> add_succ other block

(* *)

let dominators (cfg: cfg): IdSet.t IdMap.t =
  let blocks = blocks cfg in
  let rec step_dom dom =
    let rec go changed blocks dom =
      match IdSet.choose_opt blocks with
      | None -> dom, changed
      | Some block ->
        let blocks = blocks |> IdSet.remove block in
        let preds = cfg |> pred block in
        let pred_dom = try
            let pred = IdSet.choose preds in
            dom |> IdMap.find pred
          with Not_found -> IdSet.empty
        in
        let pred_dom = pred_dom |> IdSet.fold (fun pred set ->
            set |> IdSet.inter (dom |> IdMap.find pred)
          ) preds |> IdSet.add block
        in
        let changed =
          changed || not (dom |> IdMap.find block |> IdSet.equal pred_dom)
        in
        go changed blocks (dom |> IdMap.add block pred_dom)
    in
    let dom, changed = go false blocks dom in
    if changed then step_dom dom else dom
  in
  let entry = entry cfg in
  let dom =
    IdMap.empty |> IdSet.fold (fun block dom ->
        dom |> IdMap.add block blocks
      ) blocks
    |> IdMap.add entry (IdSet.singleton entry)
  in
  step_dom dom

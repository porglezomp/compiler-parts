type id = Id of int
type edge = id * id

module IdSet = Set.Make(struct type t = id let compare = compare end)
module IdMap = Map.Make(struct type t = id let compare = compare end)

type t = {
  mutable fresh: id;
  entry: id;
  final: id;
  blocks: IdSet.t;
  succ: IdSet.t IdMap.t;
  pred: IdSet.t IdMap.t;
}

let make (): t =
  let entry = Id 0 in
  let final = Id 1 in
  {
    fresh = Id 2;
    entry; final;
    blocks = IdSet.empty;
    succ = IdMap.empty;
    pred = IdMap.empty;
  }

let new_block (cfg: t): id =
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

let blocks (cfg: t): IdSet.t =
  cfg.blocks

let entry (cfg: t): id =
  cfg.entry

let final (cfg: t): id =
  cfg.final

let pred (block: id) (cfg: t): IdSet.t =
  try cfg.pred |> IdMap.find block
  with Not_found -> IdSet.empty

let succ (block: id) (cfg: t): IdSet.t =
  try cfg.succ |> IdMap.find block
  with Not_found -> IdSet.empty

(* *)

let add_block (block: id) (cfg: t): t =
  { cfg with
    blocks = cfg.blocks |> IdSet.add block;
  }

let add_succ (block: id) (other: id) (cfg: t): t =
  let cfg = cfg |> add_block block |> add_block other in
  { cfg with
    pred = cfg.pred |> IdMap.add other (cfg |> pred other |> IdSet.add block);
    succ = cfg.succ |> IdMap.add block (cfg |> succ block |> IdSet.add other);
  }

let add_pred (block: id) (other: id) (cfg: t): t =
  cfg |> add_succ other block

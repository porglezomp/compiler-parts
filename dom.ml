module IdMap = Cfg.IdMap
module IdSet = Cfg.IdSet

type loop = {
  header: Cfg.id;
  body: IdSet.t;
  back_edges: Cfg.edge list;
  exit_edges: Cfg.edge list;
}

let dominators (cfg: Cfg.t): IdSet.t IdMap.t =
  let blocks = Cfg.blocks cfg in
  let rec step_dom dom =
    let rec go changed blocks dom =
      match IdSet.choose_opt blocks with
      | None -> dom, changed
      | Some block ->
        let blocks = blocks |> IdSet.remove block in
        let preds = cfg |> Cfg.pred block in
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
  let entry = Cfg.entry cfg in
  let dom =
    IdMap.empty |> IdSet.fold (fun block dom ->
        dom |> IdMap.add block blocks
      ) blocks
    |> IdMap.add entry (IdSet.singleton entry)
  in
  step_dom dom

let backedges (dom: IdSet.t IdMap.t) (cfg: Cfg.t): Cfg.edge list =
  [] |> IdSet.fold (fun block backedges ->
      let succ = cfg |> Cfg.succ block in
      let dom = dom |> IdMap.find block in
      backedges |> IdSet.fold (fun next edges ->
          if dom |> IdSet.mem next then
            (block, next) :: edges
          else edges
        ) succ
    ) (Cfg.blocks cfg)

let loops (backedges: Cfg.edge list) (cfg: Cfg.t): loop IdMap.t =
  let rec fill header body blocks =
    match blocks with
    | [] -> body
    | blocks ->
      let body, blocks = blocks |> List.fold_left (fun (body, blocks) block ->
          if block = header then
            body, blocks
          else
            let pred = cfg |> Cfg.pred block in
            let pred = IdSet.diff pred body in
            let blocks = blocks |> IdSet.fold (fun h t -> h :: t) pred in
            let body = body |> IdSet.add block in
            body, blocks
        ) (body, [])
      in
      fill header body blocks
  in
 let loops = backedges |> List.fold_left (fun loops (src, dst) ->
      let body = fill dst IdSet.empty [src] in
      loops |> IdMap.update dst (function
          | None -> Some {
              header = dst;
              body;
              back_edges = [src, dst];
              exit_edges = [];
            }
          | Some loop -> Some {
              loop with
              body = loop.body |> IdSet.union body;
              back_edges = (src, dst) :: loop.back_edges;
            }
        )
    ) IdMap.empty
  in
  loops

(* *)

let graphviz (cfg: Cfg.t): string =
  let dom = dominators cfg in
  let nodes, edges = ([], []) |> IdSet.fold (fun block (nodes, edges) ->
      let succ = cfg |> Cfg.succ block in
      let dom = dom |> IdMap.find block in
      let nodes = Cfg.string_of_block block :: nodes in
      let edges = edges |> IdSet.fold (fun next edges ->
          let edge = if dom |> IdSet.mem next then
              Printf.sprintf "%s -> %s [dir=back, penwidth=2]"
                (Cfg.string_of_block next)
                (Cfg.string_of_block block)
            else
              Printf.sprintf "%s -> %s"
                (Cfg.string_of_block block)
                (Cfg.string_of_block next)
          in
          edge :: edges
        ) succ
      in
      nodes, edges
    ) (Cfg.blocks cfg)
  in
  String.concat "\n" (["digraph {"] @ nodes @ edges @ ["}"])

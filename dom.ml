module ComputeDom (Cfg: S.CFG) = struct
  module IdSet = Cfg.IdSet
  module IdMap = Cfg.IdMap

  type domsets = Cfg.IdSet.t Cfg.IdMap.t
  type idomset = Cfg.id Cfg.IdMap.t

  let dominators (cfg: Cfg.t): domsets =
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

  let immediate (dom: domsets) (cfg: Cfg.t): idomset =
    let add_idom block map =
      Printf.printf "Add idom for %s\n" (Cfg.string_of_block block) ;
      let dom = dom |> IdMap.find block in
      let rec find_idom search =
        Printf.printf "Searching idom(%s), candidates: %s\n"
          (Cfg.string_of_block block)
          (Cfg.string_of_block_set search) ;
        match IdSet.choose_opt search with
        | None -> None
        | Some pred ->
          let search =
            search |> IdSet.remove pred |> IdSet.union (cfg |> Cfg.pred pred)
          in
          if dom |> IdSet.mem pred then
            Some pred
          else
            find_idom search
      in
      match find_idom (cfg |> Cfg.pred block) with
      | None ->
        Printf.printf "idom(%s) = {}\n"
          (Cfg.string_of_block block) ;
        map
      | Some idom ->
        Printf.printf "idom(%s) = %s\n"
          (Cfg.string_of_block block)
          (Cfg.string_of_block idom) ;
        map |> IdMap.add block idom
    in
    IdMap.empty |> IdSet.fold add_idom (Cfg.blocks cfg)
end

module Reversed (Cfg: S.CFG) = struct
  include Cfg

  let entry = Cfg.final
  let final = Cfg.entry
  let pred = Cfg.succ
  let succ = Cfg.pred
end

module Make (Cfg: S.CFG) = struct
  module IdSet = Cfg.IdSet
  module IdMap = Cfg.IdMap

  type loop = {
    header: Cfg.id;
    body: IdSet.t;
    back_edges: Cfg.edge list;
    exit_edges: Cfg.edge list;
  }

  type domsets = Cfg.IdSet.t Cfg.IdMap.t
  type idomset = Cfg.id Cfg.IdMap.t
  type domtree = Cfg.id * domsets

  module C = ComputeDom(Cfg)
  module RC = ComputeDom(Reversed(Cfg))

  let dom (cfg: Cfg.t): domsets =
    C.dominators cfg

  let pdom (cfg: Cfg.t): domsets =
    RC.dominators cfg

  let idom (dom: domsets) (cfg: Cfg.t): idomset =
    C.immediate dom cfg

  let ipdom (dom: domsets) (cfg: Cfg.t): idomset =
    RC.immediate dom cfg

  let domtree (idom: idomset): domtree =
    let rec add tree idom =
      match IdMap.choose_opt idom with
      | None -> tree
      | Some (child, parent) ->
        let idom = idom |> IdMap.remove child in
        let tree = tree |> IdMap.update parent (function
            | None -> Some (IdSet.singleton child)
            | Some children -> Some (children |> IdSet.add child)
          )
        in
        idom |> add tree
    in
    let rec find_root node =
      match idom |> IdMap.find_opt node with
      | None -> node
      | Some node -> find_root node
    in
    let root = find_root (IdMap.choose idom |> snd) in
    let tree = idom |> add IdMap.empty in
    root, tree

  let backedges (dom: domsets) (cfg: Cfg.t): Cfg.edge list =
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
    let dom = dom cfg in
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

  let domtree_graphviz (root, tree: domtree): string =
    let rec build tree acc =
      match IdMap.choose_opt tree with
      | None ->
        "digraph {" ::
        "node [shape=\"ellipse\"]" ::
        Printf.sprintf "%s [peripheries=2]" (Cfg.string_of_block root) ::
        acc
      | Some (parent, children) ->
        let tree = tree |> IdMap.remove parent in
        let parent = Cfg.string_of_block parent in
        let acc = acc |> IdSet.fold (fun child acc ->
            Printf.sprintf "%s -> %s" parent (Cfg.string_of_block child) :: acc
          ) children
        in
        acc |> build tree
    in
    ["}"] |> build tree |> String.concat "\n"

end

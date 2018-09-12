module Make (Cfg: S.CFG) : sig
  type loop = {
    header: Cfg.id;
    body: Cfg.IdSet.t;
    back_edges: Cfg.edge list;
    exit_edges: Cfg.edge list;
  }

  val dominators : Cfg.t -> Cfg.IdSet.t Cfg.IdMap.t
  val backedges : Cfg.IdSet.t Cfg.IdMap.t -> Cfg.t -> Cfg.edge list
  val loops : Cfg.edge list -> Cfg.t -> loop Cfg.IdMap.t

  val graphviz : Cfg.t -> string
end

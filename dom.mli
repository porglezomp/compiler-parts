module Make (Cfg: S.CFG) : sig
  type loop = {
    header: Cfg.id;
    body: Cfg.IdSet.t;
    back_edges: Cfg.edge list;
    exit_edges: Cfg.edge list;
  }

  type domsets = Cfg.IdSet.t Cfg.IdMap.t
  type idomset = Cfg.id Cfg.IdMap.t

  val dom : Cfg.t -> domsets
  val pdom : Cfg.t -> domsets
  val idom : domsets -> Cfg.t -> idomset
  val ipdom : domsets -> Cfg.t -> idomset

  val backedges : Cfg.IdSet.t Cfg.IdMap.t -> Cfg.t -> Cfg.edge list
  val loops : Cfg.edge list -> Cfg.t -> loop Cfg.IdMap.t

  val graphviz : Cfg.t -> string
end

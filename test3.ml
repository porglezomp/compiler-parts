module Dom = Dom.Make(Cfg)

let () =
  let cfg = Cfg.make () in
  let entry = Cfg.entry cfg in
  let bb1 = Cfg.new_block cfg in
  let bb2 = Cfg.new_block cfg in
  let bb3 = Cfg.new_block cfg in
  let bb4 = Cfg.new_block cfg in
  let bb5 = Cfg.new_block cfg in
  let bb6 = Cfg.new_block cfg in
  let bb7 = Cfg.new_block cfg in
  let bb8 = Cfg.new_block cfg in
  let final = Cfg.final cfg in
  let cfg =
    cfg
    |> Cfg.add_succ entry bb1
    |> Cfg.add_succ bb1 bb2
    |> Cfg.add_succ bb1 bb3
    |> Cfg.add_succ bb2 bb4
    |> Cfg.add_succ bb2 bb5
    |> Cfg.add_succ bb3 bb3
    |> Cfg.add_succ bb3 bb6
    |> Cfg.add_succ bb4 bb7
    |> Cfg.add_succ bb5 bb7
    |> Cfg.add_succ bb6 bb1
    |> Cfg.add_succ bb6 bb2
    |> Cfg.add_succ bb7 bb8
    |> Cfg.add_succ bb8 final
  in
  let dominators = Dom.dom cfg in
  let backedges = cfg |> Dom.backedges dominators in
  let loops = cfg |> Dom.loops backedges in

  print_endline "\nDominators:" ;
  dominators |> Cfg.IdMap.iter (fun block dom ->
      Printf.printf "Block %s: %s\n"
        (Cfg.string_of_block block)
        (Cfg.string_of_block_set dom)
    ) ;

  print_endline "\nBackedges:" ;
  backedges |> List.iter (fun (x, y) ->
      Printf.printf "%s -> %s\n"
        (Cfg.string_of_block x)
        (Cfg.string_of_block y)
    ) ;

  print_endline "\nLoops:" ;
  loops |> Cfg.IdMap.iter (fun head loop ->
      Printf.printf "%s: %s\n%s"
        (Cfg.string_of_block loop.Dom.header)
        (Cfg.string_of_block_set loop.Dom.body)
        (loop.Dom.back_edges |> List.map (fun (a, b) ->
             Printf.sprintf "  %s -> %s\n"
               (Cfg.string_of_block a)
               (Cfg.string_of_block b)
           ) |> String.concat "")
    ) ;

  let out = open_out "target/dom.dot" in
  cfg |> Dom.graphviz |> output_string out

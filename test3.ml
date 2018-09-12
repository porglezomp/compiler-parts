let () =
  let cfg = Dom.make () in
  let entry = Dom.entry cfg in
  let bb1 = Dom.new_block cfg in
  let bb2 = Dom.new_block cfg in
  let bb3 = Dom.new_block cfg in
  let bb4 = Dom.new_block cfg in
  let bb5 = Dom.new_block cfg in
  let bb6 = Dom.new_block cfg in
  let bb7 = Dom.new_block cfg in
  let bb8 = Dom.new_block cfg in
  let final = Dom.final cfg in
  let cfg =
    cfg
    |> Dom.add_succ entry bb1
    |> Dom.add_succ bb1 bb2
    |> Dom.add_succ bb1 bb3
    |> Dom.add_succ bb2 bb4
    |> Dom.add_succ bb2 bb5
    |> Dom.add_succ bb3 bb3
    |> Dom.add_succ bb3 bb6
    |> Dom.add_succ bb4 bb7
    |> Dom.add_succ bb5 bb7
    |> Dom.add_succ bb6 bb1
    |> Dom.add_succ bb6 bb2
    |> Dom.add_succ bb7 bb8
    |> Dom.add_succ bb8 final
  in
  let dominators = Dom.dominators cfg in
  dominators |> Dom.IdMap.iter (fun block dom ->
      Printf.printf "Block %s: %s\n"
        (Dom.string_of_block block)
        (Dom.string_of_block_set dom)
    ) ;
  cfg |> Dom.backedges dominators |> List.iter (fun (x, y) ->
      Printf.printf "%s -> %s\n"
        (Dom.string_of_block x)
        (Dom.string_of_block y)
    ) ;
  let out = open_out "target/dom.dot" in
  cfg |> Dom.graphviz |> output_string out

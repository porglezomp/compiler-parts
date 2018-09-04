type 'n interval = {
  label: 'n;
  start: int;
  finish: int;
}

let rec intersects (int1: 'n interval) (int2: 'n interval): bool =
    (* We ensure that int1 starts before int2. *)
    if int1.start > int2.start then
        intersects int2 int1
    else
        (* Since int1 starts first, int2 doesn't contain int1, so they overlap
           as long as either point of int2 is inside int1. *)
        (int1.start <= int2.start && int2.start <= int1.finish)
        || (int1.start <= int2.finish && int2.finish <= int1.finish)

(* TODO: Use an interval tree to accelerate this *)
let conflict (intervals: 'n interval list): ('n, unit) Graph.graph =
    let graph = Graph.empty () in
    let intervals = intervals |> List.map (fun { label; start; finish } ->
        { label = graph |> Graph.mk_node label; start; finish }) in
    let graph = intervals |> List.fold_left (fun graph { label } ->
        graph |> Graph.add_node label
    ) graph in
    intervals |> List.fold_left (fun graph int1 ->
            intervals |> List.fold_left (fun graph int2 ->
                if int1 <> int2 && intersects int1 int2 then
                    graph |> Graph.add_edge int1.label int2.label ()
                else
                    graph
            ) graph
        ) graph

let svg (show_label: 'n -> Graph.attrs) (intervals: 'n interval list): string =
    let box i ({label; start; finish}: 'n interval) =
      let attrs = show_label label in
      let get' key f default  =
        match attrs |> List.assoc_opt key with
        | None -> default
        | Some value -> f value
      in
      let get key default = get' key (fun x -> x) default in
      let width = (finish - start) * 10 in
      let x = start * 10 in
      let y = get' "colorid" (fun id -> int_of_string id * 25) (i * 25) in
      let text_x, text_y = (start + finish) * 5, y + 15 in
      let fill = get' "fillcolor" (fun c ->
          Printf.sprintf " style=\"fill:%s;\"" c) "" in
      let label = get "label" "" in
      Printf.sprintf "<rect width=\"%d\" height=\"20\" x=\"%d\" y=\"%d\"%s></rect>
<text x=\"%d\" y=\"%d\">%s</text>"
        width x y
        fill
        text_x text_y
        label
    in
    "<svg xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" version=\"1.1\">
  <style>rect { fill: none; stroke: black; } text { text-anchor: middle; }</style>
"
    ^ (intervals |> List.mapi box |> String.concat "\n") ^
    "\n</svg>"

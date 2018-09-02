type 'n interval = {
  label: 'n;
  start: int;
  finish: int;
}

let conflict (intervals: 'n interval list): ('n, unit) Graph.graph =
    failwith "unimplemented"

let svg (show_label: 'n -> string) (intervals: 'n interval list): string =
    let box i ({label; start; finish}: 'n interval) =
        let width = (finish - start) * 10 in
        let x, y = start * 10, i * 25 in
        let text_x, text_y = (start + finish) * 5, y + 15 in
        Printf.sprintf "<rect width=\"%d\" height=\"20\" x=\"%d\" y=\"%d\"></rect>
<text x=\"%d\" y=\"%d\">%s</text>"
            width x y
            text_x text_y
            (show_label label)
    in
    "<svg xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" version=\"1.1\">
  <style>rect { fill: none; stroke: black; } text { text-anchor: middle; }</style>
"
    ^ (intervals |> List.mapi box |> String.concat "\n") ^
    "\n</svg>"

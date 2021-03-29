type html = string
type content = html
type cell = html

type align = Left | Right | Center

type font = string
type size = int
type color = string

let nocontent : content = ""

let div
      (align: align)
      (font: string option)
      (size: int option)
      (color: color option)
      (border: bool)
      (text: string)
  : content
  =
  Printf.sprintf "<div style='text-align: %s; color: %s; font: %s;%s'>%s</div>"
    (match align with Left -> "left" | Right -> "right" | Center -> "center")
    (match color with None -> "white" | Some c -> c)
    (Printf.sprintf "%dpt %s"
       (match size with None -> 40 | Some s -> s)
       (match font with None -> "sans-serif" | Some f -> f))
    (if border then
       " text-shadow: -1px 0 black, 0 1px black, 1px 0 black, 0 -1px black;"
     else "")
    text

let tabcell (background: string option) (content: content) : cell =
  match background with
  | Some img ->
     Printf.sprintf "<td align=\"center\"><div style='position: relative;'>\n<img src=\"%s\" \\>\n<div style='position: absolute; left:0px; top: 0px; width: 100%%;'>%s</div></div></td>"
       img
       content
  | None -> "<td>" ^ content ^ "</td>"

let table (cells: cell list list) : content =
  let row r =
    "<tr>"
    ^ (String.concat "\n" r)
    ^ "</tr>"
  in
  "<table border=0>\n"
  ^ (String.concat "\n" (List.map row cells))
  ^ "\n</table>"

let table_one_row (cells: cell list) : content =
  table [cells]

let table_one_col (cells: cell list) : content =
  table (List.map (fun x -> [x]) cells)

let output (html: html) (file: string) =
  let outchan = open_out file in
  let _ = output_string outchan html in
  close_out outchan

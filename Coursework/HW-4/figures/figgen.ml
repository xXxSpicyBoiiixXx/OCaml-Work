let fname = Array.get Sys.argv 1

let basename =
  try Filename.chop_extension fname
  with Invalid_argument _ -> fname

let chan = open_in fname
let lexbuf = Lexing.from_channel chan

let fig = Figparser.figure Figlexer.token lexbuf
let _ = Figures.output fig (basename ^ ".html")

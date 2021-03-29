let _ = if Array.length Sys.argv < 2 then
          (Printf.printf "Usage: ./microml <filename>\n";
           exit 1)
let fname = Array.get Sys.argv 1

let basename =
  try Filename.chop_extension fname
  with Invalid_argument _ -> fname

let chan = open_in fname
let lexbuf = Lexing.from_channel chan

let exp = Parser.expr Lexer.token lexbuf
let v = Interp.eval_exp exp
let _ = Print.fprint_val Format.std_formatter v
let _ = Format.pp_print_newline Format.std_formatter ()

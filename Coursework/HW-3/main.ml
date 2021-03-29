let _ = if Array.length Sys.argv < 2 then
          (Printf.printf "Usage: ./microml <filename>\n";
           exit 1)
let fname = Array.get Sys.argv 1

let basename =
  try Filename.chop_extension fname
  with Invalid_argument _ -> fname

let exp = Parse.parse_file fname
let outchan = open_out (basename ^ ".py")
let _ = output_string outchan "print("
let _ = output_string outchan (Topy.exp_to_python exp)
let _ = output_string outchan ")"
let _ = close_out outchan

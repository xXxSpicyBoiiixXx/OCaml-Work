{

  open Figparser

}

let digit = ['0'-'9']
let identchar = ['a'-'z' 'A'-'z' '.' '/' '-']
let opencomment = "(*"
let closecomment = "*)"
let ws = [' ' '\t' '\n']
                     
rule token = parse
       | ws { token lexbuf }
       | opencomment _* closecomment { token lexbuf }
       | digit+ as n { NUM (int_of_string n) }
       | '\"' ((_ # '\"')* as s) '\"' { STRING s }
       | "border" { BORDER }
       | "horiz" { HORIZ }
       | "vert" { VERT }
       | "left" { LEFT }
       | "right" { RIGHT }
       | "above" { ABOVE }
       | "below" { BELOW }
       | "over" { OVER }
       | "caption" { CAPTION }
       | "[" { LBRACE }
       | "]" { RBRACE }
       | "(" { LPAREN }
       | ")" { RPAREN }

       | identchar+ as s { IDENT s }

       | eof { EOF }

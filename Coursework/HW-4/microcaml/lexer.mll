{

  open Parser

}

let digit = ['0'-'9']
let identchar = ['a'-'z' 'A'-'Z' '\'' '_' '0'-'9']
let ident = ['a'-'z'] identchar*
let opencomment = "(*"
let closecomment = "*)"
let ws = [' ' '\t' '\n']

rule comment = parse
       | closecomment { token lexbuf }
       | _ { comment lexbuf}
and token = parse
       | ws { token lexbuf }
       | opencomment { comment lexbuf }
       | digit+ as n { NUM (int_of_string n) }
       | "true" { TRUE }
       | "false" { FALSE }

       | "(+)" { PLUS }
       | "(-)" { MINUS }
       | "(*)" { TIMES }
       | "(/)" { DIV }
       | "(&&)" { AND }
       | "(||)" { OR }
       | "(<=)" { LE }
       | "(<)" { LT }
       | "(>=)" { GE }
       | "(>)" { GT } 
       | "(=)" { EQOP }

       | "=" { EQUAL}

       | "fun" { FUN }
       | "->" { ARROW }

       | "if" { IF }
       | "then" { THEN }
       | "else" { ELSE }

       | "app" { APP }
       | "to" { TO }

       | "let" { LET }
       | "in" { IN }
               
       | ident as s { IDENT s }

       | eof { EOF }
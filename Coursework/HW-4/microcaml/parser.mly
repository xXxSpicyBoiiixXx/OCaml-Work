%{
    open Types
%}

%token <int> NUM
%token <string> IDENT
%token TRUE FALSE
%token PLUS MINUS TIMES DIV
%token AND OR
%token LT LE GT GE EQOP
%token EQUAL
%token IF THEN ELSE
%token APP TO
%token LET IN
%token FUN ARROW
%token EOF

%start expr
%type <Types.exp> expr

%%
expr:
  const                                    { Value (Const $1) }
| IDENT                                    { Var $1 }
| op                                       { Value (Op $1) }
| FUN IDENT ARROW expr                     { Value (Fun ($2, $4)) }
| IF expr THEN expr ELSE expr              { If ($2, $4, $6) }
| LET IDENT EQUAL expr IN expr             { Let ($2, $4, $6) }
| APP expr TO expr                         { App ($2, $4) }
;

op:
  PLUS  { Plus }
| MINUS { Minus }
| TIMES { Times }
| DIV   { Div }
| AND   { And }
| OR    { Or }
| LT    { Lt }
| LE    { Le }
| GT    { Gt }
| GE    { Ge }
| EQOP  { Eq }
;

const:
  NUM                                      { Int $1 }
| TRUE                                     { True }
| FALSE                                    { False }
;

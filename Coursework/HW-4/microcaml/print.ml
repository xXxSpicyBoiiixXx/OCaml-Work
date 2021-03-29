open Types
open Format
   
let string_of_const =
  function True -> "true"
         | False ->"false"
         | Int n -> string_of_int n
let string_of_op =
  function
    Plus -> "(+)"
  | Minus -> "(-)"
  | Times -> "(*)"
  | Div -> "(/)"
  | Lt -> "(<)"
  | Gt -> "(>)"
  | Le -> "(<=)"
  | Ge -> "(>=)"
  | Eq -> "(=)"
  | And -> "(&&)"
  | Or -> "(||)"

let rec fprint_val f =
  function Const c -> fprintf f "%s" (string_of_const c)
         | Fun (x, e) -> fprintf f "@[<hv 1>fun %s ->@ %a@]" x fprint_exp e
         | Op o -> fprintf f "%s" (string_of_op o)
         | Absfun _ -> fprintf f "<fun>"
and fprint_exp f =
  function Var v -> fprintf f "%s" v
         | Value v -> fprint_val f v
         | Let (x, e1, e2) ->
            fprintf f "@[<hv 1>let %s =@ %a@ in@ @[<hv 1>%a@]@]"
              x
              fprint_exp e1
              fprint_exp e2
         | If (e1, e2, e3) ->
            fprintf f "@[<hv 1>if@ %a@ then@ %a@ else@ %a@]"
              fprint_exp e1
              fprint_exp e2
              fprint_exp e3
         | App (e1, e2) ->
            fprintf f "@[<hv 1>app@ %a@ to@ %a@]"
              fprint_exp e1
              fprint_exp e2
let pprint_exp e =
  (fprint_exp Format.std_formatter e;
   Format.print_newline ())

open Types

exception ImplementMe
   
type token = TRUE
           | FALSE
           | NUM of int
           | FUN
           | VAR of string
           | ARROW
           | LET
           | EQUAL
           | IN
           | IF
           | THEN
           | ELSE
           | APP
           | TO
           | PLUS
           | MINUS
           | TIMES
           | DIV
           | LT
           | GT
           | LE
           | GE
           | EQOP
           | AND
           | OR

type loc = int * int (* line, column *)
type tl = token * loc
           
let token_of_string s =
  match s with
  | "true" -> TRUE
  | "false" -> FALSE
  | "fun" -> FUN
  | "->" -> ARROW
  | "let" -> LET
  | "=" -> EQUAL
  | "in" -> IN
  | "if" -> IF
  | "then" -> THEN
  | "else" -> ELSE
  | "app" -> APP
  | "to" -> TO
  | "(+)" -> PLUS
  | "(-)" -> MINUS
  | "(*)" -> TIMES
  | "(/)" -> DIV
  | "(<)" -> LT
  | "(>)" -> GT
  | "(<=)" -> LE
  | "(>=)" -> GE
  | "(=)" -> EQOP
  | "(&&)" -> AND
  | "(||)" -> OR
  | _ ->
     (match int_of_string_opt s with
      | Some n -> NUM n
      | None -> VAR s)

let string_of_token =
  function TRUE -> "true"
         | FALSE -> "false"
         | NUM n -> string_of_int n
         | FUN -> "fun"
         | VAR x -> x
         | ARROW -> "->"
         | LET -> "let"
         | EQUAL -> "="
         | IN -> "in"
         | IF -> "if"
         | THEN -> "then"
         | ELSE -> "else"
         | APP -> "app"
         | TO -> "to"
         | PLUS -> "(+)"
         | MINUS -> "(-)"
         | TIMES -> "(*)"
         | DIV -> "(/)"
         | LT -> "(<)"
         | GT -> "(>)"
         | LE -> "(<=)"
         | GE -> "(>=)"
         | EQOP -> "(=)"
         | AND -> "(&&)"
         | OR -> "(||)"
               
let implode l = String.init (List.length l) (List.nth l)

let is_ws c = match c with
  | ' ' | '\t' | '\n' -> true
  | _ -> false
       
let rec lex chars curr stream (currline, currcol) lastloc =
  let curr_token curr =
    if List.length curr > 0 then
      (* We have a nonempty token *)
      Some (token_of_string (implode (List.rev curr)), lastloc)
    else
      None
  in
  match chars with
  | [] -> (match curr_token curr with
           | Some t -> List.rev (t::stream)
           | None -> List.rev stream)
  | []::lines ->
     (* Whitespace ends tokens *)
       (match curr_token curr with
        | Some tok ->
           lex lines [] (tok::stream) (currline + 1, 0) (currline + 1, 0)
        | None -> lex lines [] stream (currline + 1, 0) (currline + 1, 0))
  | (c::t)::lines ->
     let chars' = t::lines in
     if is_ws c then
       (* Whitespace ends tokens *)
       (match curr_token curr with
        | Some tok ->
           lex chars' [] (tok::stream) (currline, currcol + 1)
             (currline, currcol + 1)
        | None -> lex chars' [] stream (currline, currcol + 1)
                    (currline, currcol + 1))
     else
       lex chars' (c::curr) stream (currline, currcol + 1) lastloc
;;

exception SyntaxError of loc option * string

let is_op (t: token) : bool =
  match t with
  | PLUS | MINUS | TIMES | DIV | LT | GT | LE | GE | EQOP | AND | OR -> true
  | _ -> false

(*>* Problem 2.6 *>*)
let is_const (t: token) : bool =
  raise ImplementMe
       
let is_first_val (t: token) : bool =
  raise ImplementMe

let parse_const (tokens : tl list) : const * tl list =
  match tokens with
  | [] -> raise (SyntaxError (None, "expected constant"))
  | (TRUE, _)::t -> (True, t)
  | (FALSE, _)::t -> (False, t)
  | (NUM n, _)::t -> (Int n, t)
  | (_, l)::t -> raise (SyntaxError (Some l, "expected constant"))

(*>* Problem 2.7 *>*)
let parse_var (tokens : tl list) : var * tl list =
  raise ImplementMe

let parse_op (tokens : tl list) : op * tl list =
  let op_of_token (t, l) =
    match t with
    | PLUS -> Plus | MINUS -> Minus | TIMES -> Times | DIV -> Div | LT -> Lt
    | GT -> Gt | LE -> Le | GE -> Ge | EQOP -> Eq | AND -> And | OR -> Or
    | _ -> raise (SyntaxError (Some l, "expected operator"))
  in
  raise ImplementMe

(*>* Problem 2.8 *>*)
let parse_exact (t: token) (tokens: tl list) : tl list =
  raise ImplementMe

(*>* Problem 2.9 *>*)
let rec parse_value (tokens : tl list) : value * tl list =
  match tokens with
  | [] -> raise (SyntaxError (None, "expected value"))
  | (FUN, _)::t -> let (v, t) = parse_var t in
                   let t = parse_exact ARROW t in
                   let (e, t) = parse_exp t in
                   (Fun (v, e), t)
  | (h, l)::t ->
     (if is_op h then raise ImplementMe
      else if is_const h then raise ImplementMe
      else raise (SyntaxError (Some l, "expected value"))
     )

and parse_exp (tokens : tl list) : exp * tl list =
  raise ImplementMe

let rec explode n s =
  if n >= String.length s then
    []
  else
    (String.get s n)::(explode (n + 1) s)
       
let parse (s: string list) : exp =
  match parse_exp (lex (List.map (explode 0) s) [] [] (1, 0) (1, 0)) with
  | (e, []) -> e
  | _ -> raise (SyntaxError (None, "expected EOF"))

let string_of_file f =
  let ic = open_in f in
  let rec read_it l =
    try read_it ((input_line ic)::l)
    with End_of_file -> List.rev l
  in
  let s = read_it [] in
  let _ = close_in ic in
  s

let parse_file f =
  parse (string_of_file f)
  
let _ = assert (parse ["let f = fun x -> app x to x in app f to f"] =
                  Let ("f", Value (Fun ("x", App (Var "x", Var "x"))),
                       App (Var "f", Var "f")))

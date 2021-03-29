open Types
open Print

let rec rename_value x x' v =
  match v with
  | Const _ -> v
  | Fun (x'', e) ->
     if x = x'' then Fun (x'', e)
     else Fun (x'', rename_exp x x' e)
  | Op _ -> v
and rename_exp x x' e =
  match e with
  | Var x'' -> Var (if x = x'' then x' else x'')
  | Value v -> Value (rename_value x x' v)
  | Let (x'', e1, e2) ->
     Let (x'', rename_exp x x' e1,
          if x = x'' then e2
          else rename_exp x x' e2)
  | If (e1, e2, e3) ->
     If (rename_exp x x' e1, rename_exp x x' e2, rename_exp x x' e3)
  | App (e1, e2) ->
     App (rename_exp x x' e1, rename_exp x x' e2)

let uniq_vars e =
  let ctr = ref 0 in
  let new_var () =
    let n = !ctr in
    let _ = ctr := n + 1 in
    "x" ^ (string_of_int n)
  in
  let rec uv_value v =
    match v with
    | Const _ | Op _ -> v
    | Fun (x, e) ->
       let x' = new_var () in
       Fun (x', uv_exp (rename_exp x x' e))
  and uv_exp e =
    match e with
    | Var _ -> e
    | Value v -> Value (uv_value v)
    | Let (x, e1, e2) ->
       let x' = new_var () in
       Let (x', uv_exp e1, uv_exp (rename_exp x x' e2))
    | If (e1, e2, e3) ->
       If (uv_exp e1, uv_exp e2, uv_exp e3)
    | App (e1, e2) ->
       App (uv_exp e1, uv_exp e2)
  in
  uv_exp e

let rec desugar_value v =
  match v with
  | Const _ | Op _ -> v
  | Fun (x, e) -> Fun (x, desugar_exp e)
and desugar_exp e =
  match e with
  | Var _ -> e
  | Value v -> Value (desugar_value v)
  | Let (x, e1, e2) -> App (Value (Fun (x, desugar_exp e2)), desugar_exp e1)
  | If (e1, e2, e3) ->
     If (desugar_exp e1, desugar_exp e2, desugar_exp e3)
  | App (e1, e2) -> App (desugar_exp e1, desugar_exp e2)
  
let const_to_python =
  function True -> "True"
         | False -> "False"
         | Int n -> string_of_int n

let op_to_python =
  function Plus -> "(lambda x: lambda y: x + y)"
         | Minus -> "(lambda x: lambda y: x - y)"
         | Times -> "(lambda x: lambda y: x * y)"
         | Div -> "(lambda x: lambda y: x / y)"
         | Lt -> "(lambda x: lambda y: x < y)"
         | Gt -> "(lambda x: lambda y: x > y)"
         | Le -> "(lambda x: lambda y: x <= y)"
         | Ge -> "(lambda x: lambda y: x >= y)"
         | Eq -> "(lambda x: lambda y: x == y)"
         | And -> "(lambda x: lambda y: x & y)"
         | Or -> "(lambda x: lambda y: x | y)"
                  
let exp_to_python e =
  let rec val_to_python v =
    match v with
    | Const c -> const_to_python c
    | Op o -> op_to_python o
    | Fun (x, e) -> Printf.sprintf "(lambda %s: %s)" x (exp_to_python e)
  and exp_to_python e =
    match e with
    | Var x -> x
    | Value v -> val_to_python v
    | Let _ -> failwith "Should be desugared"
    | If (e1, e2, e3) ->
       Printf.sprintf "(%s) if (%s) else (%s)"
         (exp_to_python e2)
         (exp_to_python e1)
         (exp_to_python e3)
    | App (e1, e2) -> Printf.sprintf "(%s)(%s)"
                        (exp_to_python e1)
                        (exp_to_python e2)
  in
  let e' = uniq_vars e in
  let e'' = desugar_exp e' in
  exp_to_python e''

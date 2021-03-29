open Types

exception TypeError of string
exception UnboundVariable of string
exception ImplementMe

(*>* Problem 3.3 *>*)
(* [v/x]v' *)
let rec sub_val (v: value) (x: string) (v': value) =
  match v' with
    (* Add some cases here. You can keep the catch-all case below if you
     * want it, but you don't need to. *)
  | _ -> v'
(* [v/x]e *)
and sub_exp (v: value) (x: string) (e: exp)  =
  raise ImplementMe

let bool_of b = if b then True else False

let denote_op (o: op) : value -> value -> value =
  match o with
  | Plus | Minus | Times | Div ->
     (fun x y ->
       match (o, x, y) with
       | (Plus, Const (Int n1), Const (Int n2)) -> Const (Int (n1 + n2))
       | (Minus, Const (Int n1), Const (Int n2)) -> Const (Int (n1 - n2))
       | (Times, Const (Int n1), Const (Int n2)) -> Const (Int (n1 * n2))
       | (Div, Const (Int n1), Const (Int n2)) -> Const (Int (n1 / n2))
       | _ -> raise (TypeError "type mismatch"))
  | Lt | Gt | Le | Ge | Eq ->
     (fun x y ->
       match (o, x, y) with
       | (Lt, Const (Int n1), Const (Int n2)) -> Const (bool_of (n1 < n2))
       | (Gt, Const (Int n1), Const (Int n2)) -> Const (bool_of (n1 > n2))
       | (Le, Const (Int n1), Const (Int n2)) -> Const (bool_of (n1 <= n2))
       | (Ge, Const (Int n1), Const (Int n2)) -> Const (bool_of (n1 >= n2))
       | (Eq, Const (Int n1), Const (Int n2)) -> Const (bool_of (n1 = n2))
       | _ -> raise (TypeError "type mismatch"))
  | And | Or ->
     (fun x y ->
       match (o, x, y) with
       | (And, Const True, Const True) -> Const True
       | (And, _, _) -> Const False
       | (Or, Const False, Const False) -> Const False
       | (Or, _, _) -> Const True
       | _ -> raise (TypeError "type mismatch"))

(*>* Problem 3.4 *>*)
let rec eval_exp (e: exp) : value =
  match e with
  | Var x -> raise ImplementMe
  | Value v -> raise ImplementMe
  | If (e1, e2, e3) -> raise ImplementMe
  | Let (x, e1, e2) -> raise ImplementMe
  | App (e1, e2) ->
     let v2 = eval_exp e2 in
     (match eval_exp e1 with
      | Fun (x, e) -> raise ImplementMe
      | Op o -> Absfun ((denote_op o) v2)
      | Absfun f -> f v2
      | _ -> raise (TypeError "expected function"))
                

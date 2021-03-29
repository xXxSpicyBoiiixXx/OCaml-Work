type const = True | False | Int of int
type var = string
type op = Plus | Minus | Times | Div | Lt | Gt | Le | Ge | Eq | And | Or
type value = Const of const
           | Fun of var * exp
           | Op of op
           | Absfun of (value -> value)
and exp = Var of var
        | Value of value
        | Let of var * exp * exp
        | If of exp * exp * exp
        | App of exp * exp

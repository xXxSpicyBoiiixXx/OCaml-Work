(* val fib : int -> int = <fun>  *)

let rec fib n = 
        if n < 2 then n else fib (n - 1) + fib (n - 2);; 

(* int = 55 *) 
fib 10;; 

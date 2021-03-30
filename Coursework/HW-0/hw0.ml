(*** Homework 0 ***)
(*** Md Ali ***) 

(** Lists **)

(***********************************************************************)

let rec stutter n x = 
        if n <= 0 then []
        else x::(stutter (n - 1) x)

let _= assert (stutter 0 5 = []);; 
let _= assert (stutter 1 5 = [5]);;
let _= assert (stutter 3 3 = [3; 3; 3]);;

(***********************************************************************)

let rec filter f l = 
        match l with 
        | [] -> [] 
        | h::t -> if f h then h::(filter f t) else filter f t 
;;

assert (filter ((<) 5) [1;7;2;0;9] = [7;9]);;
assert (filter ((<) 10) [1;7;2;0;9] = []);;
assert (filter ((<) 15) [1;17;2;0;8] = [17]);;
assert (filter (fun _ -> true) [] =[]);; 

(***********************************************************************)

let rec find f l = 
        match l with 
        | [] -> None 
        | h::t -> if f h then Some h else find f t 
;;

assert (find ((<) 5) [1;7;2;0;9] = Some 7);; 
assert (find ((<) 10) [1;7;2;0;9] = None);;
assert (find ((<) 10) [1;17;2;0;8] = Some 17);; 
assert (find (fun _ -> true) [] = None);; 

(***********************************************************************)

let parens l = 
        let rec parens_rec l opened = 
                match l with 
                | [] -> opened = 0 
                | '('::t -> parens_rec t (opened + 1) 
                | ')'::t -> opened > 0 && parens_rec t (opened - 1) 
                | _::t -> parens_rec t opened
             in 
             parens_rec l 0
;;

assert (parens [] = true);;
assert (parens ['('; ')'] = true);;
assert (parens ['('] = false);; 
assert (parens [')'] = false);; 
assert (parens ['('; ')'; '('] = false);; 
assert (parens ['('; '('; ')'] = false);; 
assert (parens ['('; '('; ')'; ')'] = true);;
assert (parens ['('; '('; ')'; '('; ')'; ')'] = true);;
assert (parens ['('; '('; ')'; '('; ')'] = false);;

(***********************************************************************)

let find2D f lines = 
        let rec find_line line (r, c) = 
                match line with 
                | [] -> None 
                | h::t -> if f h then Some (r, c) 
                        else find_line t (r, c + 1) 
        in 
        let rec find2D_rec lines r = 
                match lines with 
                | [] -> None 
                | l::t ->
                                (match find_line l (r, 0) with 
                                | None -> find2D_rec t (r + 1) 
                                | Some rc -> Some rc) 
        in 
        find2D_rec lines 0
;;

assert (find2D (fun _ -> true) [] = None);;
assert (find2D (fun _ -> true) [[]; []] = None);;

let file = 
        [[1;2;3;4;5];
         [5;4;3;2;1];
         [10;9;8;7;6];
         [2;4;6;8;10;12]]
;;

assert (find2D ((=) 5) file = Some(0,4));;
assert (find2D ((<) 5) file = Some(2,0));;
assert (find2D ((<) 10) file = Some(3,5));;


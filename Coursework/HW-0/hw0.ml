(*** Homework 0 ***)
(*** Md Ali ***) 

(** Lists **)

(*
 * List Operations 
 *
 * module List: sig .. end 
 *
 * --- Some Operations ---
 *
 * val length : 'a list -> int 
 *      Returns the length of the given list 
 *
 * val hd : 'a list -> 'a
 *      Returns the first element of the given list. This rasies a failure "hd" if the list empty 
 *
 * val tl : 'a list -> 'a list 
 *      Returns the given list without its first element. This raises a failure "tl" if the list is empty.
 *
 * val nth : 'a list -> int -> 'a 
 *      Returns the nth element of the given list. The first element is at postion 0. 
 *      Raises failure "nth" if the list is too too short. 
 *      Raises invalid_argument "List.nth" if n is negative 
 *
 * val rev : 'a list -> 'a list 
 *      Reverses the list 
 *
 * val append : 'a list -> 'a list -> 'a list 
 *      Cantenat two lists. 
 *
 *      COME BACK TO FOR OPERATION NOTES  
 *)

(*
 * assert
 *
 * This built in function takes an expression as an argument and throws an
 * excpetion if the provided expression evalutes to false. This is usually used 
 * for debugging and testing e.g. 
 *
 * assert e 
 *
 * this will trigger an exeption if e evaluates to false, will do nothing if e 
 * evaluates to true.
 *
 *
 *)

(***********************************************************************)

(*
 * This function << stutter: int -> 'a -> 'a list: >> 
 * So the call << stutter n x >> should result in a list 
 * with x repeted n times, e.g. 
 *
 * For all x, stutter 0 x = [] 
 * stutter 2 5 = [5; 5]
 * stutter 3 "yadda" = ["yadda"; "yadda"; "yadda"]
 *)

let rec stutter n x = 
        if n <= 0 then []
        else x::(stutter (n - 1) x)

(***Testing***)
let _= assert (stutter 0 5 = []);; 
let _= assert (stutter 1 5 = [5]);;
let _= assert (stutter 3 3 = [3; 3; 3]);;

(***********************************************************************)

(*
 * This function << filter : ('a -> bool) -> 'a list -> 'a list >> 
 * So the call << filter f l >> should result "l" wiht all and only 
 * the items for which "f" returns true; These elements should appear in the 
 * same order in which the appear in the orginal list. 
 *
 * filter (fun x -> x > 2) [5; 3; 1; 2; 4] = [5; 3; 4] 
 * filter (fun x -> x > 5) = [] 
 *)

let rec filter f l = 
        match l with 
        | [] -> [] 
        | h::t -> if f h then h::(filter f t) else filter f t 
;;

(***Testing***)
assert (filter ((<) 5) [1;7;2;0;9] = [7;9]);;
assert (filter ((<) 10) [1;7;2;0;9] = []);;
assert (filter ((<) 15) [1;17;2;0;8] = [17]);;
assert (filter (fun _ -> true) [] =[]);; 

(***********************************************************************)

(*
 * The function << find: ('a -> bool) -> 'a list -> 'a option >> 
 * this will take some aguments as filter. If x is the first element in 
 * the list for which f x = true, the find should return Some x. If 
 * there is no such element, it should return None 
 *
 * find (fun x -> x > 2) [5; 3; 1; 2; 4] = Some 5 
 * find (fun x -> x < 3) [5; 3; 1; 2; 4] = Some 1 
 * find (fun x -> x > 5) [5; 3; 1; 2; 4] = None 
 *)

let rec find f l = 
        match l with 
        | [] -> None 
        | h::t -> if f h then Some h else find f t 
;;

(***Testing***)
assert (find ((<) 5) [1;7;2;0;9] = Some 7);; 
assert (find ((<) 10) [1;7;2;0;9] = None);;
assert (find ((<) 10) [1;17;2;0;8] = Some 17);; 
assert (find (fun _ -> true) [] = None);; 

(***********************************************************************)

(*
 * The below function makes sure that your parenthese match. 
 * Here, we wrote a function parens that makes a list of 
 * characters inccluding open parens '(' and close parens ')'. 
 *
 * here we will use a recursive helper function that scans the list 
 * left to right and tracks addtional information.  
 *)
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

(***Testing***)
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

(*
 * This find function finds items occuring in a list. It also find and item
 * in a list of list, represting a 2D array of characters. 
 * 
 * Both rows and columns should be indexed from 0. When determining which 
 * item is "first: We move from left to right within each row, then move 
 * to the next row, then move to the next row (just like reading). 
 * 
 * find2D ((=) 5 file = Some (0, 4) because the first 5 appears in row 0
 * at character 4 
 * 
 * find2D ((=) 15 file = None because 15 does not appear in the file 
 *)

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

(***Testing***)
assert (find2D (fun _ -> true) [] = None);;
assert (find2D (fun _ -> true) [[]; []] = None);;

(***Test File***) 
let file = 
        [[1;2;3;4;5];
         [5;4;3;2;1];
         [10;9;8;7;6];
         [2;4;6;8;10;12]]
;;

(***Testing***)
assert (find2D ((=) 5) file = Some(0,4));;
assert (find2D ((<) 5) file = Some(2,0));;
assert (find2D ((<) 10) file = Some(3,5));;


(*** IIT CS 440, Spring 2021 ***)
(*** Homework 0 ***)

exception ImplementMe

(** Lists *)

(* IMPORTANT: Do not remove any lines beginning *>*, like the following one.
 * These are used by our autograder and need to be there for you to get points. 
 *)
(*>* Task 4.1 *>*)

let rec stutter (n: int) (x: 'a) : 'a list =
  (* Remove the line "raise ImplementMe" and write your code for Task 4.1
   * here. Do not change the signature above. *)
  raise ImplementMe
;;

(* A couple example unit tests. Write these for each function you implement.
 * Uncomment them once you've implemented stutter. *)
(*
assert (stutter 0 5 = []);;
assert (stutter 2 5 = [5; 5]);;
*)

(*>* Task 4.2 *>*)

let rec filter (f: 'a -> bool) (l: 'a list) : 'a list =
  raise ImplementMe
;;

(*>* Task 4.3 *>*)

let rec find (f: 'a -> bool) (l: 'a list) : 'a option =
  raise ImplementMe
;;

(*>* Task 4.4 *>*)

let parens (l: char list) : bool =
  raise ImplementMe
;;

(*>* Task 4.5 *>*)

let find2D (f: 'a -> bool) (lines: 'a list list) : (int * int) option =
  raise ImplementMe
;;

(*
assert (find2D (fun _ -> true) [] = None);;
assert (find2D (fun _ -> true) [[]; []] = None);;
let file =
  [[1;2;3;4;5];
   [5;4;3;2;1];
   [10;9;8;7;6];
   [2;4;6;8;10;12]]
;;
assert (find2D ((=) 5) file = Some (0, 4));;
assert (find2D ((<) 5) file = Some (2, 0));;
assert (find2D ((<) 10) file = Some (3, 5));;
 *)

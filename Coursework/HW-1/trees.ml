(*** IIT CS 440, Spring 2021 ***)
(*** Homework 1 ***)

exception ImplementMe
;;

(** Trees **)

type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree
;;

let rec tree_fold (t: 'a tree) (u: 'b) (f: 'a -> 'b -> 'b -> 'b) =
  match t with
  | Leaf -> u
  | Node (v, l, r) -> f v (tree_fold l u f) (tree_fold r u f)

(*>* Problem 2.1 *>*)
let tree_map (t: 'a tree) (f: 'a -> 'b) : 'b tree =
  raise ImplementMe

(*>* Problem 2.2 *>*)
let inorder (t: 'a tree) : 'a list =
  raise ImplementMe

(*>* Problem 2.3 *>*)
let preorder (t: 'a tree) : 'a list =
  raise ImplementMe                                               

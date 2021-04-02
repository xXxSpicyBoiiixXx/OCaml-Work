(*** Md Ali ***) 
(*** trees.ml ***)

exception ImplementMe
;;

type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree
;;

let rec tree_fold (t: 'a tree) (u: 'b) (f: 'a -> 'b -> 'b -> 'b) =
  match t with
  | Leaf -> u
  | Node (v, l, r) -> f v (tree_fold l u f) (tree_fold r u f)

let tree_map (t: 'a tree) (f: 'a -> 'b) : 'b tree =
  tree_fold t Leaf (fun v l r -> Node (f v, l, r)) 

let inorder (t: 'a tree) : 'a list =
  tree_fold t [] (fun v l r -> l @ [v] @ r)

let preorder (t: 'a tree) : 'a list =
                         

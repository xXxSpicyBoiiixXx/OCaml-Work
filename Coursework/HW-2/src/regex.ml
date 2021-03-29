(*** IIT CS 440, Spring 2021 ***)
(*** Homework 2 ***)

(** Regular Expressions *)

open Nfa
open N

(* You only have to implement one function in this file. Search for
 * "symb_nfa" or keep scrolling
 *)

exception ImplementMe
   
type regex = Symbol of symbol
           | Or of regex * regex
           | Cat of regex * regex
           | Star of regex
           | Empty

let rec dedup l =
  match l with
  | [] -> []
  | x::t -> x::(dedup (List.filter (fun x' -> x' <> x) t))
;;

let rec explode s n =
    if n >= String.length s then
      []
    else
      (String.get s n)::(explode s (n + 1))
  ;;

(** Code for converting regular expressions to NFAs **)  
let retarget_transitions trans_list offset =
  List.map (fun (st, sy, st') -> (st + offset,  sy, st' + offset)) trans_list
;;

let or_nfas n1 n2 =
  {states = n1.states + n2.states + 2;
   accept = [n1.states + n2.states + 1];
   alpha = dedup (n1.alpha @ n2.alpha);
   trans =
     (0, None, 1)::(0, None, n1.states + 1)::
       ((retarget_transitions n1.trans 1)
        @ (retarget_transitions n2.trans (n1.states + 1))
        @ (List.map (fun st -> (st + 1, None, n1.states + n2.states + 1)) n1.accept)
        @ (List.map (fun st -> (st + n1.states + 1, None, n1.states + n2.states + 1)) n2.accept)
       )
  }
         
let concat_nfas n1 n2 = 
  {states = n1.states + n2.states;
   accept = List.map (fun n -> n + n1.states) n2.accept;
   alpha = dedup (n1.alpha @ n2.alpha);
   trans =
     (n1.trans) @
     (retarget_transitions n2.trans n1.states)
     @ (List.map (fun st -> (st, None, n1.states)) n1.accept)
  }

(*>* Problem 3.4 *>*)

let symb_nfa (s: symbol option) =
  raise ImplementMe

let star_nfa n =
  {states = n.states + 2;
   accept = [n.states + 1];
   alpha = n.alpha;
   trans =
     (0, None, 1)::(0, None, n.states + 1)::
       ((retarget_transitions n.trans 1) @
          (List.map (fun st -> (st + 1, None, 1)) n.accept)
          @ (List.map (fun st -> (st + 1, None, n.states + 1)) n.accept))
  }
   
  
let rec nfa_of_regex r =
  match r with
    Symbol s -> symb_nfa (Some s)
  | Or (n1, n2) -> or_nfas (nfa_of_regex n1) (nfa_of_regex n2)
  | Cat (n1, n2) -> concat_nfas (nfa_of_regex n1) (nfa_of_regex n2)
  | Star n -> star_nfa (nfa_of_regex n)
  | Empty -> symb_nfa None

(** Code for parsing regular expressions **)
exception InvalidRegex

let rec parse_regex0 s n =
  (* let _ = Printf.printf "regex0 %d\n" n in *)
  if n >= String.length s then raise InvalidRegex
  else
    match String.get s n with
    | '|' | '*' | ')' -> raise InvalidRegex
    | '(' ->
       let (r, n') = parse_regex s (n + 1) in
       if n' < String.length s && String.get s n' = ')' then
         (r, n' + 1)
       else raise InvalidRegex
    | '#' -> (Empty, n + 1)
    | c -> (Symbol c, n + 1)

and parse_star_or_0 s n =
  (* let _ = Printf.printf "star %d\n" n in *)
  let (r, n') = parse_regex0 s n in
  if n' < String.length s && String.get s n' = '*' then
    (Star r, n' + 1)
  else (r, n')

and parse_concat s n =
  (* let _ = Printf.printf "concat %d\n" n in *)
  let (r1, n') = parse_star_or_0 s n in
  try let (r2, n'') = parse_concat s n' in
      (Cat (r1, r2), n'')
  with InvalidRegex -> (r1, n')

and parse_regex s n =
  (* let _ = Printf.printf "regex %d\n" n in *)
  let (r1, n') = parse_concat s n in
  if n' < String.length s && String.get s n' = '|' then
    try let (r2, n'') = parse_regex s (n' + 1) in
        (Or (r1, r2), n'')
    with InvalidRegex -> (r1, n')
  else
    (r1, n')

let match_regex r s =
  let nfa = nfa_of_regex (fst (parse_regex r 0)) in
  let input = explode s 0 in
  nfa_sim nfa [0] input
;;
  

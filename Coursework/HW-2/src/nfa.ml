(*** IIT CS 440, Spring 2021 ***)
(*** Homework 2 ***)

(** NFA Simulator *)

(* Same deal here with ignoring some of these lines. *)
module N : ParseNFA.NFA_Types =
  struct
    
    type symbol = char
    type state = int
    (* Type is the same as before except now a transition has a
     * symbol option:
     * (m, Some s, n) is a transition from m to n on seeing symbol s
     * (m, None, n) is an epsilon-transition from m to n
     *)
    type nfa = { states : int;
                 accept : state list;
                 alpha  : symbol list;
                 trans  : (state * symbol option * state) list }

  end

open N
module Parser = ParseNFA.Parser(N)
  
exception IllformedInput of string
exception ImplementMe

(** Some useful helper functions **)

(* Sorts a list and removed duplicates; after you call norm on a list,
 * you can treat it like a set, that is, if (norm l1) = (norm l2), then l1
 * and l2 are equal as sets (have the same elements, regardless of order and
 * multiples)
 *)
let norm l =
  let rec dedup l =
    match l with
    | [] -> []
    | x::t -> x::(dedup (List.filter (fun x' -> x' <> x) t))
  in
  List.sort (fun a b -> a - b) (dedup l)
;;

(* Turns a list of states into a human-readable string. Useful for debugging *)
let string_of_states states =
  Printf.sprintf "{%s}"
    (String.concat ", " (List.map string_of_int states))
;;

(** Your code starts here **)

(*>* Problem 3.1 *>*)

(* Returns a list of states you can be in on seeing symbol symb (which is
 * either Some s for a symbol s, or None for epsilon) in state
 * "state". This is a list and not just a single state because this is
 * an NFA. Note that if symb is None, this should just be the states
 * reachable with one epsilon-transition. *)
let transitions (trans_list : (state * symbol option * state) list)
      (symb: symbol option) (state: state) : state list =
  raise ImplementMe

(*>* Problem 3.2 *>*)

(* Returns the list of states accessible by (possibly multiple) epsilon
 * transitions from "states" *)
let rec eps_clos (nfa: nfa) (states: state list) : state list =
  raise ImplementMe

(*>* Problem 3.3 *>*)

(* Returns true if nfa accepts input: "states" is the list of states we
 * might be in currently *)
let rec nfa_sim (nfa: nfa) (states: state list) (input: symbol list) : bool =
  let states' = eps_clos nfa states
  in
  raise ImplementMe
;;

(*** IIT CS 440, Spring 2021 ***)
(*** Homework 2 ***)

(** NFA Simulator Driver *)

module Parser = Nfa.Parser

let file = Array.get Sys.argv 1;;
let (nfa, input) = Parser.parse_file file;;
if Nfa.nfa_sim nfa [0] input
then Printf.printf "ACCEPTED\n"
else Printf.printf "REJECTED\n";;

(*** IIT CS 440, Spring 2021 ***)
(*** Homework 2 ***)

(** DFA Simulator Driver *)

module Parser = Dfa.Parser

let file = Array.get Sys.argv 1;;
let (dfa, input) = Parser.parse_file file;;
if Dfa.dfa_sim dfa 0 input
then Printf.printf "ACCEPTED\n"
else Printf.printf "REJECTED\n";;

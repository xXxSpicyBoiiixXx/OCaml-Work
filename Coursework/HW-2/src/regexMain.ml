open Regex

(** Main code **)
if match_regex (Array.get Sys.argv 1) (Array.get Sys.argv 2)
then Printf.printf "ACCEPTED\n"
else Printf.printf "REJECTED\n"

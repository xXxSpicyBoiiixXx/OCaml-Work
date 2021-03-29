(*** IIT CS 440, Spring 2021 ***)
(*** Homework 2 ***)

(** NFA parsing code, weirdly factored for convenience! **)

module type NFA_Types =
  sig

    type symbol = char;;
    type state = int;;
    type nfa = { states : int;
                 accept : state list;
                 alpha  : symbol list;
                 trans  : (state * symbol option * state) list }

  end

module Parser (N: NFA_Types) =
  struct

    exception ParseError of string
    
    let rec explode s n =
    if n >= String.length s then
      []
    else
      (String.get s n)::(explode s (n + 1))
  ;;

let parse_input in_chan =
  let rec readn n f err_str =
    if n <= 0 then []
    else
      try (f (input_line in_chan))::(readn (n - 1) f err_str)
      with _ -> raise (ParseError err_str)
  in
  let rec read_transitions () =
    try
      let line = input_line in_chan in
      let (st, sy, st') =
        try Scanf.sscanf line "%d %c %d" (fun a b c -> (a, Some b, c))
        with _ ->
          (try Scanf.sscanf line "%d %d" (fun a b -> (a, None, b))
           with _ ->
             raise (ParseError ("invalid transition: " ^ line)))
      in (st, sy, st')::(read_transitions ())
    with End_of_file -> []
  in
  let (num_states, num_acc_states, num_symbs) =
    try Scanf.sscanf (input_line in_chan) "%d %d %d" (fun a b c -> (a, b, c))
    with _ -> raise (ParseError "expected <integer> <integer> <integer>")
  in
  let accept_states = readn num_acc_states int_of_string "expected <integer>"
  in
  let alpha =
    try String.trim (input_line in_chan)
    with _ -> raise (ParseError "expected <string>")
  in
  let alpha =
    if String.length alpha <> num_symbs then
      raise (ParseError "wrong number of symbols")
    else
      explode alpha 0
  in
  let input =
    try String.trim (input_line in_chan)
    with _ -> raise (ParseError "expected <string>")
  in
  let trans = read_transitions ()
  in
  ({ N.states = num_states;
     N.accept = accept_states;
     N.alpha = alpha;
     N.trans = trans},
   explode input 0)

let parse_file f =
  let chan = open_in f in
  let (nfa, input) = parse_input chan in
  let _ = close_in chan in
  (nfa, input)

  end

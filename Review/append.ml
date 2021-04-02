external startbb : int -> unit = "startbb"
external endbb : unit -> unit = "endbb" 
let _ = startbb 3 
let f () = startbb 2; ()
let _= startbb 1

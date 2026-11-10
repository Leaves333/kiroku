type t = { year : int; month : int; day : int }

let to_string date =
  Printf.sprintf "%04d-%02d-%02d" date.year date.month date.day

(* parses the three ints from the string,
   then passes the values to a function that constructs a date object *)
let parse str =
  Scanf.sscanf_opt str "%04d-%02d-%02d%!" (fun year month day ->
      { year; month; day })

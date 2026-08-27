(* open Nottui *)
(* module W = Nottui_widgets *)
(**)
(* let () = Nottui_unix.run (Lwd.pure (W.printf "Hello world")) *)

open In_channel

let file = "todo.txt"
let print_list lines = Printf.printf "[%s]" @@ String.concat ", " lines

let () =
    let lines =
        In_channel.with_open_text file In_channel.input_all
        |> String.split_on_char '\n'
    in
    print_list lines

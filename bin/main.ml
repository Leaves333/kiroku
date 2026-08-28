open Nottui
module W = Nottui_widgets
open In_channel

let file = "todo.txt"
let print_list lines = Printf.printf "[%s]" @@ String.concat ", " lines

let lines =
    In_channel.with_open_text file In_channel.input_all
    |> String.split_on_char '\n'
    |> List.filter (fun line -> line <> "")
    |> List.map String.trim

let lines_view = String.concat "\n" lines |> W.printf "%s" |> Lwd.pure

(* lwd dynamic variables? *)
let count = Lwd.var 0
let quit = Lwd.var false

let keyboard_handler = function
    | `ASCII 'i', [] ->
        Lwd.update (fun n -> n + 1) count;
        `Handled
    | `ASCII 'd', [] ->
        Lwd.update (fun n -> n - 1) count;
        `Handled
    | `ASCII 'q', [] ->
        Lwd.set quit true;
        `Handled
    | _ -> `Unhandled

let view =
    Lwd.map (Lwd.get count) ~f:(fun value ->
        let content =
            Ui.vcat
              [
                W.printf "Count: %d" value;
                W.string "Press i to increment, d to decrement, q to quit";
              ]
        in

        Ui.keyboard_area keyboard_handler content)

let () = Nottui_unix.run ~quit view

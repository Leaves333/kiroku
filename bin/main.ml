open Nottui
module W = Nottui_widgets
open In_channel

let file = "todo.txt"
let print_list lines = String.concat ", " lines |> Printf.printf "[%s]"

(* reads all the lines from todo.txt *)
let lines =
    In_channel.with_open_text file In_channel.input_all
    |> String.split_on_char '\n'
    |> List.filter (fun line -> line <> "")
    |> List.map String.trim

(* returns only lines that contain pattern as a substring *)
let filter_lines pattern lines =
    List.filter
      (fun line -> Option.is_some (String.find_first ~sub:pattern line))
      lines

(* hi *)
let lines_view lines = String.concat "\n" lines |> W.printf "%s" |> Lwd.pure

(*lwd dynamic variables? *)
let count = Lwd.var 0
let quit = Lwd.var false
let filter_pattern = Lwd.var ""

let counter_keyboard_handler = function
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

let counter_view =
    Lwd.map (Lwd.get count) ~f:(fun value ->
        let content =
            Ui.vcat
              [
                W.printf "Count: %d" value;
                W.string "Press i to increment, d to decrement";
              ]
        in

        Ui.keyboard_area counter_keyboard_handler content)

let delete_last_char str =
    let len = String.length str in
    if String.length str = 0 then str else String.sub str 0 (len - 1)

let filter_keyboard_handler = function
    | `ASCII character, modifiers
      when (not (List.mem `Ctrl modifiers)) && not (List.mem `Meta modifiers) ->
        Lwd.update (fun str -> str ^ String.make 1 character) filter_pattern;
        `Handled
    | `Backspace, [] ->
        Lwd.update (fun str -> delete_last_char str) filter_pattern;
        `Handled
    | _ -> `Unhandled

let filter_view =
    Lwd.map (Lwd.get filter_pattern) ~f:(fun pattern ->
        let content =
            Ui.vcat
            @@ [
                 W.printf "matching pattern: %s" pattern;
                 W.string "type to edit the pattern! Ctrl-q to quit.";
                 W.string "";
               ]
            @ List.map W.string (filter_lines pattern lines)
        in

        Ui.keyboard_area filter_keyboard_handler content)

(*let () = Nottui_unix.run @@ lines_view @@ filter_lines "+ling" lines*)
let () = Nottui_unix.run filter_view

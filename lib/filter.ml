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

let highlight_attr = Notty.A.(fg black ++ bg yellow)

(* converts each line to a W.string component *)
(* highlighting the substrs matched by pattern *)
let highlight_matches pattern line =
  if pattern = "" then W.string line
  else
    let pattern_length = String.length pattern in
    let line_length = String.length line in
    let rec build start pieces =
      let index = String.find_first ~sub:pattern ~start line in
      match index with
      | Some match_start ->
          let before = String.sub line start (match_start - start) in
          let matched = String.sub line match_start pattern_length in
          build
            (match_start + pattern_length)
            (W.string ~attr:highlight_attr matched :: W.string before :: pieces)
      | None ->
          let remaining = String.sub line start (line_length - start) in
          Ui.hcat @@ List.rev (W.string remaining :: pieces)
    in
    build 0 []

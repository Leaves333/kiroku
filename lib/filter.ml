open Nottui
module W = Nottui_widgets
open In_channel

(* unused helper function*)
let print_list lines = String.concat ", " lines |> Printf.printf "[%s]"

(* returns if pattern is inside text *)
let matches pattern text = Option.is_some (String.find_first ~sub:pattern text)

(* returns all strings in lines that contain pattern *)
let filter_lines pattern lines = List.filter (matches pattern) lines

(* returns all tasks in tasks that match pattern *)
let filter_tasks pattern tasks =
  List.filter (fun task -> matches pattern (Task.to_string task)) tasks

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

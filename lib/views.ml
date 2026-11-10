open Nottui
module W = Nottui_widgets
open State

type handlers = {
  task : Ui.key -> Ui.may_handle;
  filter : Ui.key -> Ui.may_handle;
}

(* avoid depending on App's Focus_map *)
type statuses = { task : Focus.status; filter : Focus.status }

let mark_selected_task index lines =
  let rec helper acc index lines =
    match lines with
    | [] -> acc
    | x :: xs ->
        let select_text = if index = 0 then "> " else "  " in
        let select_widget = W.string select_text in
        helper (Ui.hcat [ select_widget; x ] :: acc) (index - 1) xs
  in
  List.rev (helper [] index lines)

(* views *)
let header_view ~(state : state) =
  Ui.vcat
    [
      W.string "epic ocaml tui app !!!";
      W.printf "current focus is: %s" (focus_to_string state.focus);
      W.string "";
    ]

let filter_view ~(state : state) ~(focus : Focus.status)
    ~(on_key : Ui.key -> Ui.may_handle) =
  let filter_desc = W.printf "filter: %s" state.filter_pattern in
  let filter_highlight = W.string ~attr:Filter.highlight_attr " " in
  let filter_line_components =
    if state.focus = Filter then [ filter_desc; filter_highlight ]
    else [ filter_desc ]
  in
  let filter_line = Ui.hcat filter_line_components in
  let content = Ui.vcat [ filter_line; W.string "" ] in
  Ui.keyboard_area ~focus on_key content

let task_view ~(state : state) ~(tasks : Task.task list) ~(focus : Focus.status)
    ~(on_key : Ui.key -> Ui.may_handle) =
  let content =
    List.map Task.task_to_string tasks
    |> List.map (Filter.highlight_matches state.filter_pattern)
    |> mark_selected_task state.task_index
    |> Ui.vcat
  in
  Ui.keyboard_area ~focus on_key content

let view ~(state : state) ~(tasks : Task.task list) ~(statuses : statuses)
    ~(handlers : handlers) =
  let header = header_view ~state in
  let filter =
    filter_view ~state ~focus:statuses.filter ~on_key:handlers.filter
  in
  let task =
    task_view ~state ~tasks ~focus:statuses.task ~on_key:handlers.task
  in
  Ui.vcat [ header; filter; task ]

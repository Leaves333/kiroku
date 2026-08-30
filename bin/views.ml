open Nottui
module W = Nottui_widgets
open In_channel

(*lwd dynamic variables? *)
type focus_type = Filter | Task

let current_focus = Lwd.var Filter
let task_focus = Focus.make ()
let filter_focus = Focus.make ()

type state_type = {
    current_focus : focus_type;
    task_index : int;
    filter_pattern : string;
  }

let cur_state =
    Lwd.var { current_focus = Filter; task_index = 0; filter_pattern = "" }

let resolve_focus focus =
    match focus with Task -> task_focus | Filter -> filter_focus

let focus_to_string focus =
    match focus with Task -> "Task" | Filter -> "Filter"

(* task view stuff *)

(* filter view stuff *)
let delete_last_char str =
    let len = String.length str in
    if String.length str = 0 then str else String.sub str 0 (len - 1)

let filter_keyboard_handler = function
    | `ASCII character, modifiers
      when (not (List.mem `Ctrl modifiers)) && not (List.mem `Meta modifiers) ->
        Lwd.update
          (fun state ->
            {
              state with
              filter_pattern = state.filter_pattern ^ String.make 1 character;
            })
          cur_state;
        `Handled
    | `Backspace, _ ->
        Lwd.update
          (fun state ->
            {
              state with
              filter_pattern = delete_last_char state.filter_pattern;
            })
          cur_state;
        `Handled
    | `Escape, _ ->
        Lwd.set current_focus Task;
        `Handled
    | _ -> `Unhandled

let filter_view =
    Lwd.map (Lwd.get cur_state) ~f:(fun state ->
        let content =
            Ui.vcat
            @@ [
                 W.printf "matching pattern: %s" state.filter_pattern;
                 W.string "type to edit the pattern! Ctrl-q to quit.";
                 W.string "";
               ]
            @ List.map
                (Filter.highlight_matches state.filter_pattern)
                (Filter.filter_lines state.filter_pattern Filter.lines)
        in
        Ui.keyboard_area filter_keyboard_handler content)

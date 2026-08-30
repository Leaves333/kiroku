open Nottui
module W = Nottui_widgets
open In_channel

(*lwd dynamic variables? *)
type focus = Task | Filter

let all_focuses = [ Task; Filter ]
let task_focus = Focus.make ()
let filter_focus = Focus.make ()

let resolve_focus focus =
    match focus with Task -> task_focus | Filter -> filter_focus

let release_all_focuses () =
    List.iter (fun focus -> Focus.release @@ resolve_focus focus) all_focuses

let set_focus focus =
    release_all_focuses ();
    Focus.request (resolve_focus focus)

type app_state = {
    current_focus : focus;
    task_index : int;
    filter_pattern : string;
  }

let app_state =
    Lwd.var { current_focus = Task; task_index = 0; filter_pattern = "" }

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
          app_state;
        `Handled
    | `Backspace, _ ->
        Lwd.update
          (fun state ->
            {
              state with
              filter_pattern = delete_last_char state.filter_pattern;
            })
          app_state;
        `Handled
    | `Escape, _ ->
        Lwd.update
          (fun state -> { state with current_focus = Filter })
          app_state;
        `Handled
    | _ -> `Unhandled

let filter_view ~(state : app_state) ~(focus : Focus.status) =
    let content =
        Ui.vcat
        @@ [
             W.printf "current focus is: %s"
               (focus_to_string state.current_focus);
             W.printf "matching pattern: %s" state.filter_pattern;
             W.string "type to edit the pattern! Ctrl-q to quit.";
             W.string "";
           ]
        @ List.map
            (Filter.highlight_matches state.filter_pattern)
            (Filter.filter_lines state.filter_pattern Filter.lines)
    in
    Ui.keyboard_area ~focus filter_keyboard_handler content

(* type for map, with focus_type as the key *)
module Focus_map = Map.Make (struct
    type t = focus

    let compare = Stdlib.compare
end)

(* constructs a Lwd.t "reactive value" of a Focus.status Focus_map.t *)
(* the `f` in List.fold_left takes our current reactive map, and a focus *)
(* in the function Lwd.bind unwraps the Lwd.t *)
(* then we map the focus to its Focus.status *)
(* and the final function adds this key value pair to the map *)
let focus_statuses =
    List.fold_left
      (fun statuses_lwd focus ->
        Lwd.bind statuses_lwd ~f:(fun statuses ->
            Lwd.map
              (Focus.status (resolve_focus focus))
              ~f:(fun status -> Focus_map.add focus status statuses)))
      (Lwd.pure Focus_map.empty) all_focuses

(* need a separate type for render context because state can be mutated by code... *)
(* but statuses is "independent" info that can't be directly mutated *)
type render_context = { state : app_state; statuses : Focus.status Focus_map.t }

(* combine the state and focus_statuses to a single render_context object *)
let render_context =
    Lwd.bind (Lwd.get app_state) ~f:(fun state ->
        Lwd.map focus_statuses ~f:(fun statuses -> { state; statuses }))

(* main app view run by main.ml *)
let app =
    Lwd.map render_context ~f:(fun context ->
        let state = context.state in
        let statuses = context.statuses in
        let filter =
            filter_view ~state ~focus:(Focus_map.find Filter statuses)
        in
        Ui.vcat [ filter ])

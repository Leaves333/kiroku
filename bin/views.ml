open Nottui
module W = Nottui_widgets
open In_channel
open Utils

(*lwd dynamic variables? *)
let all_focuses = [ Task; Filter; Edit ]
let task_focus = Focus.make ()
let filter_focus = Focus.make ()
let edit_focus = Focus.make ()
let state = Lwd.var { focus = Task; task_index = 0; filter_pattern = "" }

let resolve_focus focus =
    match focus with
    | Task -> task_focus
    | Filter -> filter_focus
    | Edit -> edit_focus

let release_all_focuses () =
    List.iter (fun focus -> Focus.release @@ resolve_focus focus) all_focuses

let set_focus focus =
    release_all_focuses ();
    Focus.request (resolve_focus focus);
    Lwd.update (fun state -> { state with focus }) state

let focus_to_string focus =
    match focus with Task -> "Task" | Filter -> "Filter" | Edit -> "Edit"

(* header *)
let header_view ~(state : state) ~(focus : Focus.status) =
    Ui.vcat
      [
        W.string "epic ocaml tui app !!!";
        W.printf "current focus is: %s" (focus_to_string state.focus);
        W.string "";
      ]

(* filter view *)
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
          state;
        `Handled
    | `Backspace, _ ->
        Lwd.update
          (fun state ->
            {
              state with
              filter_pattern = delete_last_char state.filter_pattern;
            })
          state;
        `Handled
    | `Escape, _ | `Enter, _ ->
        Lwd.update (fun state -> { state with task_index = 0 }) state;
        set_focus Task;
        `Handled
    | _ -> `Unhandled

let filter_view ~(state : state) ~(focus : Focus.status) =
    let filter_desc = W.printf "filter: %s" state.filter_pattern in
    (* this is defined both here and in views, can we have a common ui file? *)
    let filter_highlight = W.string ~attr:Filter.highlight_attr " " in
    let filter_line_components =
        if state.focus = Filter then [ filter_desc; filter_highlight ]
        else [ filter_desc ]
    in
    let filter_line = Ui.hcat filter_line_components in
    let content = Ui.vcat [ filter_line; W.string "" ] in
    Ui.keyboard_area ~focus filter_keyboard_handler content

(* task view *)
let move_task_selection amount =
    Lwd.update
      (fun state ->
        let visible_tasks =
            Filter.filter_lines state.filter_pattern Filter.lines
        in
        let max_index = List.length visible_tasks - 1 in
        let new_index = state.task_index + amount |> max 0 |> min max_index in
        { state with task_index = new_index })
      state

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

let task_keyboard_handler = function
    | `ASCII '/', _ ->
        set_focus Filter;
        Lwd.update (fun state -> { state with task_index = -1 }) state;
        `Handled
    | (`Arrow `Up | `ASCII 'k'), _ ->
        move_task_selection (-1);
        `Handled
    | (`Arrow `Down | `ASCII 'j'), _ ->
        move_task_selection 1;
        `Handled
    | _ -> `Unhandled

let task_view ~(state : state) ~(focus : Focus.status) =
    let content =
        Utils.get_filtered_lines ~state
        |> List.map (Filter.highlight_matches state.filter_pattern)
        |> mark_selected_task state.task_index
        |> Ui.vcat
    in
    Ui.keyboard_area ~focus task_keyboard_handler content

(* edit view *)
let get_ith_item ~state =
    List.nth (Utils.get_filtered_lines ~state) state.task_index

let edit_keyboard_handler = function
    | `Enter, _ ->
        set_focus Task;
        `Handled
    | _ -> `Unhandled

let edit_view ~(state : state) ~(focus : Focus.status) =
    let content =
        Filter.filter_lines state.filter_pattern Filter.lines
        |> List.sort Utils.compare_by_priority
        |> List.map (Filter.highlight_matches state.filter_pattern)
        |> mark_selected_task state.task_index
        |> Ui.vcat
    in
    Ui.keyboard_area ~focus edit_keyboard_handler content

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
type render_context = { state : state; statuses : Focus.status Focus_map.t }

(* combine the state and focus_statuses to a single render_context object *)
let render_context =
    Lwd.bind (Lwd.get state) ~f:(fun state ->
        Lwd.map focus_statuses ~f:(fun statuses -> { state; statuses }))

(* main app view run by main.ml *)
let app =
    Lwd.map render_context ~f:(fun context ->
        let state = context.state in
        let statuses = context.statuses in
        let header =
            header_view ~state ~focus:(Focus_map.find Filter statuses)
        in
        let filter =
            filter_view ~state ~focus:(Focus_map.find Filter statuses)
        in
        let task = task_view ~state ~focus:(Focus_map.find Task statuses) in
        Ui.vcat [ header; filter; task ])

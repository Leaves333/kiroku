open Nottui
module W = Nottui_widgets
open State

(*lwd dynamic variables? *)
let all_focuses = [ Task; Filter ]
let task_focus = Focus.make ()
let filter_focus = Focus.make ()
let state = Lwd.var { focus = Task; task_index = 0; filter_pattern = "" }

let resolve_focus focus =
  match focus with Task -> task_focus | Filter -> filter_focus

let release_all_focuses () =
  List.iter (fun focus -> Focus.release @@ resolve_focus focus) all_focuses

let set_focus focus =
  release_all_focuses ();
  Focus.request (resolve_focus focus);
  Lwd.update (fun state -> { state with focus }) state

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

(* filter view *)
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
      Lwd.update State.filter_pattern_backspace state;
      `Handled
  | `Escape, _ | `Enter, _ ->
      Lwd.update (fun state -> { state with task_index = 0 }) state;
      set_focus Task;
      `Handled
  | _ -> `Unhandled

(* task view *)
let move_task_selection amount =
  Lwd.update
    (fun state ->
      let visible_tasks =
        Todo_file.read_lines Todo_file.todo_file_path
        |> Filter.filter_lines state.filter_pattern
      in
      let max_index = List.length visible_tasks - 1 in
      let new_index = state.task_index + amount |> max 0 |> min max_index in
      { state with task_index = new_index })
    state

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

type handlers = {
  filter : Ui.key -> Ui.may_handle;
  task : Ui.key -> Ui.may_handle;
}

let handlers =
  { filter = filter_keyboard_handler; task = task_keyboard_handler }

(* views *)
let visible_tasks state () =
  Todo_file.read ()
  |> Filter.filter_tasks state.filter_pattern
  |> List.sort Task.compare_by_priority

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

let task_view ~(state : state) ~(focus : Focus.status)
    ~(on_key : Ui.key -> Ui.may_handle) =
  let content =
    visible_tasks state ()
    |> List.map Task.task_to_string
    |> Filter.filter_lines state.filter_pattern
    |> List.map (Filter.highlight_matches state.filter_pattern)
    |> Views.mark_selected_task state.task_index
    |> Ui.vcat
  in
  Ui.keyboard_area ~focus on_key content

let view ~state ~statuses ~handlers =
  let header = header_view ~state in
  let filter =
    filter_view ~state
      ~focus:(Focus_map.find Filter statuses)
      ~on_key:handlers.filter
  in
  let task =
    task_view ~state ~focus:(Focus_map.find Task statuses) ~on_key:handlers.task
  in
  Ui.vcat [ header; filter; task ]

let app =
  Lwd.map render_context ~f:(fun ctx ->
      view ~state:ctx.state ~statuses:ctx.statuses ~handlers)

let run () =
  set_focus Task;
  Nottui_unix.run ~quit_on_escape:false app

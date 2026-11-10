open Views
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

(* need a separate type for render context because state can be mutated by code... *)
(* but statuses is "independent" info that can't be directly mutated *)
type render_context = { state : state; statuses : statuses }

(* combine the state and focus_statuses to a single render_context object *)
let render_context =
  Lwd.bind (Lwd.get state) ~f:(fun state ->
      Lwd.bind (Focus.status task_focus) ~f:(fun task ->
          Lwd.map (Focus.status filter_focus) ~f:(fun filter ->
              { state; statuses = { task; filter } })))

let visible_tasks state =
  Todo_file.read ()
  |> Filter.filter_tasks state.filter_pattern
  |> List.sort Task.compare_by_priority

let task_max_index state = List.length (visible_tasks state) - 1

let filter_keyboard_handler = function
  | `ASCII character, modifiers
    when (not (List.mem `Ctrl modifiers)) && not (List.mem `Meta modifiers) ->
      Lwd.update (State.filter_pattern_append character) state;
      `Handled
  | `Backspace, _ ->
      Lwd.update State.filter_pattern_backspace state;
      `Handled
  | `Escape, _ | `Enter, _ ->
      Lwd.update (State.set_task_index 0) state;
      set_focus Task;
      `Handled
  | _ -> `Unhandled

let task_keyboard_handler = function
  | `ASCII '/', _ ->
      set_focus Filter;
      Lwd.update (State.set_task_index (-1)) state;
      `Handled
  | (`Arrow `Up | `ASCII 'k'), _ ->
      Lwd.update
        (fun s -> State.move_task_index (task_max_index s) (-1) s)
        state;
      `Handled
  | (`Arrow `Down | `ASCII 'j'), _ ->
      Lwd.update (fun s -> State.move_task_index (task_max_index s) 1 s) state;
      `Handled
  | _ -> `Unhandled

let (handlers : handlers) =
  { task = task_keyboard_handler; filter = filter_keyboard_handler }

let app =
  Lwd.map render_context ~f:(fun ctx ->
      Views.view ~state:ctx.state ~tasks:(visible_tasks ctx.state)
        ~statuses:ctx.statuses ~handlers)

let run () =
  set_focus Task;
  Nottui_unix.run ~quit_on_escape:false app

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
      Lwd.update (fun state -> { state with task_index = 0 }) state;
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

let (handlers : Views.handlers) =
  { filter = filter_keyboard_handler; task = task_keyboard_handler }

let app =
  Lwd.map render_context ~f:(fun ctx ->
      let statuses =
        {
          Views.task = Focus_map.find Task ctx.statuses;
          filter = Focus_map.find Filter ctx.statuses;
        }
      in
      Views.view ~state:ctx.state ~tasks:(visible_tasks ctx.state) ~statuses
        ~handlers)

let run () =
  set_focus Task;
  Nottui_unix.run ~quit_on_escape:false app

(* current view that the app is in *)
type focus = Task | Filter

(* current state for the app *)
type state = { focus : focus; task_index : int; filter_pattern : string }

let focus_to_string focus =
  match focus with Task -> "Task" | Filter -> "Filter"

let filter_pattern_backspace (s : state) =
  { s with filter_pattern = String.drop_last 1 s.filter_pattern }

let filter_pattern_append character (s : state) =
  { s with filter_pattern = s.filter_pattern ^ String.make 1 character }

let reset_task_index (s : state) = { s with task_index = 0 }
let set_task_index i (s : state) = { s with task_index = i }

let move_task_index max_index amount (s : state) =
  let index = s.task_index + amount |> max 0 |> min max_index in
  { s with task_index = index }

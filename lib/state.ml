(* current view that the app is in *)
type focus = Task | Filter

(* current state for the app *)
type state = { focus : focus; task_index : int; filter_pattern : string }

let focus_to_string focus =
  match focus with Task -> "Task" | Filter -> "Filter"

let filter_pattern_backspace state =
  { state with filter_pattern = String.drop_last 1 state.filter_pattern }

open Task
open Todo_file

type priority = char option

module Priority_map = Map.Make (struct
  type t = priority

  let compare = Stdlib.compare
end)

let generate_priority_map lines =
  let insert_into_map map line =
    let task = Option.get @@ Task.parse_task line in
    let prio = task.priority in
    Priority_map.add_to_list prio line map
  in
  List.fold_left insert_into_map Priority_map.empty lines

let print_string_list lines =
  match lines with
  | [] -> print_string "]\n"
  | [ x ] -> print_string (x ^ "\n")
  | x :: xs -> print_string x

let print_priority_map map = Priority_map.iter

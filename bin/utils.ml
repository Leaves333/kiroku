type focus = Task | Filter | Edit
type state = { focus : focus; task_index : int; filter_pattern : string }

let get_priority line =
    if
      String.length line >= 4
      && line.[0] = '('
      && line.[2] = ')'
      && line.[3] = ' '
      && line.[1] >= 'A'
      && line.[1] <= 'Z'
    then Some line.[1]
    else None

let compare_by_priority a b =
    match (get_priority a, get_priority b) with
    | Some prio_a, Some prio_b -> compare prio_a prio_b
    | Some _, None -> -1
    | None, Some _ -> 1
    | None, None -> 0

let get_filtered_lines ~state =
    Filter.filter_lines state.filter_pattern Filter.lines
    |> List.sort compare_by_priority

type priority = char option

module Priority_map = Map.Make (struct
    type t = priority

    let compare = Stdlib.compare
end)

let generate_priority_map lines =
    let insert_into_map map line =
        Priority_map.add_to_list (get_priority line) line map
    in
    List.fold_left insert_into_map Priority_map.empty lines

let print_string_list lines =
    match lines with
    | [] -> print_string "]\n"
    | [ x ] -> print_string (x ^ "\n")
    | x :: xs -> print_string x

let print_priority_map map = Priority_map.iter

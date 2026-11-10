let todo_file_path = "todo.txt"

let read_lines file =
  In_channel.with_open_text file In_channel.input_all
  |> String.split_on_char '\n'
  |> List.filter (fun line -> line <> "")
  |> List.map String.trim

let read () = read_lines todo_file_path |> List.filter_map Task.parse_task

let write_lines file lines =
  let data = String.concat "\n" lines in
  let data = if data = "" then "" else data ^ "\n" in
  Out_channel.with_open_text file (fun oc -> Out_channel.output_string oc data)

let write tasks =
  write_lines todo_file_path (List.map Task.task_to_string tasks)

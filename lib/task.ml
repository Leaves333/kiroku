type date = { year : int; month : int; day : int }

let null_date = { year = 0; month = 0; day = 0 }

let date_to_string date =
  Printf.sprintf "%04d-%02d-%02d" date.year date.month date.day

let parse_date str = raise @@ Failure "not yet implemented."

type status = Active | Completed of date | Archived of date

let status_to_char = function
  | Active -> None
  | Completed _ -> Some 'x'
  | Archived _ -> Some '~'

let status_of_char = function
  | 'x' -> Some (Completed null_date)
  | '~' -> Some (Archived null_date)
  | _ -> None

let status_to_string = function
  | Active -> ""
  | (Completed date | Archived date) as status ->
      let c = Option.get (status_to_char status) in
      Printf.sprintf "%c %s" c (date_to_string date)

type task = {
  status : status;
  priority : char option;
  due_date : date option; (* due:yyyy-mm-dd *)
  description : string; (* remainder of string *)
}

let to_string task =
  let priority_string =
    match task.priority with None -> "" | Some c -> Printf.sprintf "(%c)" c
  in
  let due_date_string =
    match task.due_date with
    | None -> ""
    | Some date -> "due:" ^ date_to_string date
  in
  let components =
    [
      status_to_string task.status;
      priority_string;
      due_date_string;
      task.description;
    ]
  in
  (* ignore empty parts in components *)
  List.filter (fun part -> part <> "") components |> String.concat " "

let parse_status line = raise @@ Failure "not yet implemented."
let parse_due_date line = raise @@ Failure "not yet implemented."
let parse_task line = raise @@ Failure "not yet implemented."

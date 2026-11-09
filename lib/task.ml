type date = { year : int; month : int; day : int }

let date_to_string date =
  Printf.sprintf "%04d-%02d-%02d" date.year date.month date.day

let parse_date str =
  (* parses the three ints from the string,
     then passes the values to a function that constructs a date object *)
  Scanf.sscanf_opt str "%04d-%02d-%02d%!" (fun year month day ->
      { year; month; day })

type status = Active | Completed of date | Archived of date

module Marker = struct
  type t = Active | Completed | Archived

  let mappings = [ (Completed, 'x'); (Archived, '~') ]
  let to_char marker = List.assoc_opt marker mappings
  let of_char c = List.find_opt (fun (_, m) -> m = c) mappings |> Option.map fst
end

(* turn a status into its Marker *)
let kind = function
  | Active -> Marker.Active
  | Completed _ -> Marker.Completed
  | Archived _ -> Marker.Archived

let status_to_char status = Marker.to_char (kind status)

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

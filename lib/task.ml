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
let marker_of_status = function
  | Active -> Marker.Active
  | Completed _ -> Marker.Completed
  | Archived _ -> Marker.Archived

let status_of_marker marker date =
  match marker with
  | Marker.Active -> Active
  | Marker.Completed -> Completed date
  | Marker.Archived -> Archived date

let status_to_char status = Marker.to_char (marker_of_status status)

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

(* returns the status if present, and the rest of the line *)
let parse_status line =
  (* active is the default return value, for valid no status *)
  let active = Some (Active, line) in
  if String.length line < 2 then active
  else
    (* if first char matches with a status char,
       and the second char is empty, we expect a date *)
    match Marker.of_char line.[0] with
    | None -> active
    | Some marker -> (
        if line.[1] <> ' ' then active
        else if String.length line < 12 then None
        else
          let date_option = parse_date (String.sub line 2 10) in
          match date_option with
          | None -> None
          | Some date ->
              let remaining = String.drop_first 13 line in
              Some (status_of_marker marker date, remaining))

(* returns the priority if present, and the rest of the line *)
let parse_priority line =
  if String.length line < 4 then Some (None, line)
  else if line.[0] <> '(' || line.[2] <> ')' || line.[3] <> ' ' then
    Some (None, line)
  else if line.[1] < 'A' || line.[1] > 'Z' then None
  else Some (Some line.[1], String.drop_first 4 line)

(* returns the due date if present, and the rest of the line *)
let parse_due_date line =
  (* line is too short to have due *)
  if String.length line < 4 then Some (None, line)
    (* line doesn't start with "due:" *)
  else if String.take_first 4 line <> "due:" then Some (None, line)
    (* line is too short to contain a whole "due:yyyy-mm-dd" *)
  else if String.length line < 14 then None
  else
    let date_option = parse_date (String.sub line 4 10) in
    match date_option with
    | None -> None
    | Some date ->
        let remaining = String.drop_first 15 line in
        Some (Some date, remaining)

(* parses a whole line as a task *)
let parse_task line =
  let line = String.trim line in
  if line = "" then None
  else
    let open Option.Syntax in
    let* status, remaining = parse_status line in
    let* priority, remaining = parse_priority remaining in
    let* due_date, remaining = parse_due_date remaining in
    Some { status; priority; due_date; description = remaining }

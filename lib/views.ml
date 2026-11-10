open Nottui
module W = Nottui_widgets

let mark_selected_task index lines =
  let rec helper acc index lines =
    match lines with
    | [] -> acc
    | x :: xs ->
        let select_text = if index = 0 then "> " else "  " in
        let select_widget = W.string select_text in
        helper (Ui.hcat [ select_widget; x ] :: acc) (index - 1) xs
  in
  List.rev (helper [] index lines)

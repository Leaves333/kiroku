open Nottui
module W = Nottui_widgets

let left_count = Lwd.var 0
let right_count = Lwd.var 0
let left_focus = Focus.make ()
let right_focus = Focus.make ()

let choose_focus focus =
    Focus.release left_focus;
    Focus.release right_focus;
    Focus.request focus

let counter ~label ~count ~focus ~next_focus =
    Lwd.map2 (Lwd.get count) (Focus.status focus) ~f:(fun value status ->
        let marker = if Focus.has_focus status then ">" else " " in

        let content = W.printf "%s %s: %d" marker label value in

        Ui.keyboard_area ~focus:status
          (function
            | `ASCII 'i', [] ->
                Lwd.update (fun n -> n + 1) count;
                `Handled
            | `ASCII 'd', [] ->
                Lwd.update (fun n -> n - 1) count;
                `Handled
            | `Tab, _ ->
                next_focus ();
                `Handled
            | _ -> `Unhandled)
          content)

let left =
    counter ~label:"Left" ~count:left_count ~focus:left_focus
      ~next_focus:(fun () -> choose_focus right_focus)

let right =
    counter ~label:"Right" ~count:right_count ~focus:right_focus
      ~next_focus:(fun () -> choose_focus left_focus)

let view =
    W.vbox
      [
        left;
        right;
        Lwd.pure (W.string "Tab: switch   i: increment   d: decrement");
      ]

let () =
    choose_focus left_focus;
    Nottui_unix.run view

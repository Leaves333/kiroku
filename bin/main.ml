open Nottui

let () =
    Views.set_focus Task;
    Nottui_unix.run ~quit_on_escape:false Views.app

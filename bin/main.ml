open Nottui

let () = Nottui_unix.run ~quit_on_escape:false Views.app

open Views

let run () =
  Views.set_focus Task;
  Nottui_unix.run ~quit_on_escape:false Views.app

open Kiroku

let make_state ?(focus = State.Task) ?(task_index = 0) ?(filter_pattern = "") ()
    =
  State.{ focus; task_index; filter_pattern }

let unhandled (_ : Nottui.Ui.key) : Nottui.Ui.may_handle = `Unhandled
let empty_status = Nottui.Focus.empty

(* Render a widget to plain text so tests can assert exact contents. *)
let render ui =
  let renderer = Nottui.Renderer.make () in
  let width = Nottui.Ui.layout_width ui in
  let height = Nottui.Ui.layout_height ui in
  Nottui.Renderer.update renderer (width, height) ui;
  let buffer = Buffer.create 256 in
  Notty.Render.to_buffer buffer Notty.Cap.dumb (0, 0) (width, height)
    (Nottui.Renderer.image renderer);
  Buffer.contents buffer

let rendered_lines ui =
  if Nottui.Ui.layout_height ui = 0 then []
  else String.split_on_char '\n' (render ui)

let check_lines name expected ui =
  Alcotest.(check (list string)) name expected (rendered_lines ui)

let task_of_line line = Option.get (Task.parse line)

(* delete_last_char *)

let test_delete_last_char () =
  let backspace pattern =
    State.filter_pattern_backspace
      { State.focus = Task; task_index = 0; filter_pattern = pattern }
    |> fun state -> state.filter_pattern
  in
  Alcotest.(check string) "empty stays empty" "" (backspace "");
  Alcotest.(check string) "single char" "" (backspace "a");
  Alcotest.(check string) "drops last char" "ab" (backspace "abc")

(* highlight_matches *)

let check_highlight_width name pattern line =
  Alcotest.(check int)
    name (String.length line)
    (Nottui.Ui.layout_width (Filter.highlight_matches pattern line))

let test_highlight_preserves_width () =
  check_highlight_width "empty pattern" "" "abc";
  check_highlight_width "single match" "a" "banana";
  check_highlight_width "repeated match" "ana" "banana";
  check_highlight_width "no match" "zz" "abc";
  check_highlight_width "pattern longer than line" "abcdef" "abc";
  check_highlight_width "empty line" "a" ""

(* mark_selected_task *)

let selected_lines index lines =
  List.map Nottui_widgets.string lines
  |> Views.mark_selected_task index
  |> Nottui.Ui.vcat |> rendered_lines

let check_selected name expected index lines =
  Alcotest.(check (list string)) name expected (selected_lines index lines)

let test_mark_selects_first () =
  check_selected "first line" [ "> a"; "  b"; "  c" ] 0 [ "a"; "b"; "c" ]

let test_mark_selects_middle () =
  check_selected "middle line" [ "  a"; "> b"; "  c" ] 1 [ "a"; "b"; "c" ]

let test_mark_selects_last () =
  check_selected "last line" [ "  a"; "  b"; "> c" ] 2 [ "a"; "b"; "c" ]

let test_mark_negative_index_selects_nothing () =
  check_selected "negative index" [ "  a"; "  b"; "  c" ] (-1) [ "a"; "b"; "c" ]

let test_mark_out_of_range_selects_nothing () =
  check_selected "out of range" [ "  a"; "  b" ] 2 [ "a"; "b" ]

let test_mark_empty_list () = check_selected "empty list" [] 0 []

let test_mark_preserves_width () =
  let widths =
    List.map Nottui_widgets.string [ "a"; "bbb"; "cc" ]
    |> Views.mark_selected_task 1
    |> List.map Nottui.Ui.layout_width
  in
  Alcotest.(check (list int)) "adds a two column marker" [ 3; 5; 4 ] widths

(* header_view *)

let test_header_task_focus () =
  check_lines "task focus"
    [ "epic ocaml tui app !!!"; "current focus is: Task"; "" ]
    (Views.header_view ~state:(make_state ~focus:State.Task ()))

let test_header_filter_focus () =
  check_lines "filter focus"
    [ "epic ocaml tui app !!!"; "current focus is: Filter"; "" ]
    (Views.header_view ~state:(make_state ~focus:State.Filter ()))

(* filter_view *)

let filter_view state =
  Views.filter_view ~state ~focus:empty_status ~on_key:unhandled

let test_filter_view_task_focus () =
  check_lines "task focus has no highlight space" [ "filter: abc"; "" ]
    (filter_view (make_state ~focus:State.Task ~filter_pattern:"abc" ()))

let test_filter_view_filter_focus () =
  check_lines "filter focus has highlight space" [ "filter: abc "; "" ]
    (filter_view (make_state ~focus:State.Filter ~filter_pattern:"abc" ()))

let test_filter_view_empty_pattern () =
  check_lines "empty pattern with filter focus" [ "filter:  "; "" ]
    (filter_view (make_state ~focus:State.Filter ~filter_pattern:"" ()))

(* task_view *)

let task_view state tasks =
  Views.task_view ~state ~tasks ~focus:empty_status ~on_key:unhandled

let test_task_view_selection () =
  let tasks = List.map task_of_line [ "(A) buy milk"; "call mom" ] in
  check_lines "first task selected"
    [ "> (A) buy milk"; "  call mom" ]
    (task_view (make_state ~task_index:0 ()) tasks)

let test_task_view_second_selection () =
  let tasks = List.map task_of_line [ "(A) buy milk"; "call mom" ] in
  check_lines "second task selected"
    [ "  (A) buy milk"; "> call mom" ]
    (task_view (make_state ~task_index:1 ()) tasks)

let test_task_view_negative_index () =
  let tasks = List.map task_of_line [ "buy milk"; "call mom" ] in
  check_lines "negative index selects nothing"
    [ "  buy milk"; "  call mom" ]
    (task_view (make_state ~task_index:(-1) ()) tasks)

let test_task_view_highlights_without_changing_text () =
  let tasks = List.map task_of_line [ "buy milk"; "call mom" ] in
  check_lines "highlighting leaves text intact"
    [ "> buy milk"; "  call mom" ]
    (task_view (make_state ~filter_pattern:"buy" ()) tasks)

let test_task_view_empty () =
  let ui = task_view (make_state ()) [] in
  Alcotest.(check int)
    "empty task view has no height" 0
    (Nottui.Ui.layout_height ui)

(* view *)

let handlers : Views.handlers = Views.{ filter = unhandled; task = unhandled }

let statuses : Views.statuses =
  Views.{ task = empty_status; filter = empty_status }

let test_view_combines_sections () =
  let tasks = List.map task_of_line [ "buy milk"; "call mom" ] in
  check_lines "header, filter and tasks"
    [
      "epic ocaml tui app !!!";
      "current focus is: Task";
      "";
      "filter: buy";
      "";
      "> buy milk";
      "  call mom";
    ]
    (Views.view
       ~state:(make_state ~filter_pattern:"buy" ())
       ~tasks ~statuses ~handlers)

let test_view_empty_tasks () =
  let ui = Views.view ~state:(make_state ()) ~tasks:[] ~statuses ~handlers in
  Alcotest.(check int)
    "header plus filter height" 5
    (Nottui.Ui.layout_height ui)

let test_view_filter_focus () =
  check_lines "filter focus in combined view"
    [
      "epic ocaml tui app !!!";
      "current focus is: Filter";
      "";
      "filter: abc ";
      "";
    ]
    (Views.view
       ~state:(make_state ~focus:State.Filter ~filter_pattern:"abc" ())
       ~tasks:[] ~statuses ~handlers)

let suites =
  [
    ( "delete_last_char",
      [
        Alcotest.test_case "deletes last character" `Quick test_delete_last_char;
      ] );
    ( "highlight_matches",
      [
        Alcotest.test_case "preserves width" `Quick
          test_highlight_preserves_width;
      ] );
    ( "mark_selected_task",
      [
        Alcotest.test_case "selects first" `Quick test_mark_selects_first;
        Alcotest.test_case "selects middle" `Quick test_mark_selects_middle;
        Alcotest.test_case "selects last" `Quick test_mark_selects_last;
        Alcotest.test_case "negative index selects nothing" `Quick
          test_mark_negative_index_selects_nothing;
        Alcotest.test_case "out of range selects nothing" `Quick
          test_mark_out_of_range_selects_nothing;
        Alcotest.test_case "empty list" `Quick test_mark_empty_list;
        Alcotest.test_case "preserves width" `Quick test_mark_preserves_width;
      ] );
    ( "header_view",
      [
        Alcotest.test_case "task focus" `Quick test_header_task_focus;
        Alcotest.test_case "filter focus" `Quick test_header_filter_focus;
      ] );
    ( "filter_view",
      [
        Alcotest.test_case "task focus" `Quick test_filter_view_task_focus;
        Alcotest.test_case "filter focus" `Quick test_filter_view_filter_focus;
        Alcotest.test_case "empty pattern" `Quick test_filter_view_empty_pattern;
      ] );
    ( "task_view",
      [
        Alcotest.test_case "selection" `Quick test_task_view_selection;
        Alcotest.test_case "second selection" `Quick
          test_task_view_second_selection;
        Alcotest.test_case "negative index" `Quick test_task_view_negative_index;
        Alcotest.test_case "highlighting keeps text" `Quick
          test_task_view_highlights_without_changing_text;
        Alcotest.test_case "empty list" `Quick test_task_view_empty;
      ] );
    ( "view",
      [
        Alcotest.test_case "combines sections" `Quick
          test_view_combines_sections;
        Alcotest.test_case "empty tasks" `Quick test_view_empty_tasks;
        Alcotest.test_case "filter focus" `Quick test_view_filter_focus;
      ] );
  ]

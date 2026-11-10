open Kiroku

let write_file path contents =
  Out_channel.with_open_text path (fun oc ->
      Out_channel.output_string oc contents)

let with_temp_dir f =
  let dir = Filename.temp_file "kiroku-app-test-" "" in
  Sys.remove dir;
  Unix.mkdir dir 0o700;
  Fun.protect
    ~finally:(fun () ->
      Array.iter
        (fun name -> Unix.unlink (Filename.concat dir name))
        (Sys.readdir dir);
      Unix.rmdir dir)
    (fun () -> f dir)

(* App.visible_tasks reads the hardcoded todo.txt path, so tests run in a temp cwd. *)
let with_temp_cwd f =
  let original = Sys.getcwd () in
  with_temp_dir (fun dir ->
      Sys.chdir dir;
      Fun.protect ~finally:(fun () -> Sys.chdir original) (fun () -> f dir))

let make_state pattern =
  State.{ focus = Task; task_index = 0; filter_pattern = pattern }

let visible_lines pattern =
  App.visible_tasks (make_state pattern) |> List.map Task.task_to_string

let check_visible name expected pattern =
  Alcotest.(check (list string)) name expected (visible_lines pattern)

(* visible_tasks *)

let test_visible_tasks_sorts_by_priority () =
  with_temp_cwd (fun _ ->
      write_file Todo_file.todo_file_path "no priority\n(B) medium\n(A) high\n";
      check_visible "prioritised tasks come first"
        [ "(A) high"; "(B) medium"; "no priority" ]
        "")

let test_visible_tasks_keeps_order_for_equal_priority () =
  with_temp_cwd (fun _ ->
      write_file Todo_file.todo_file_path "first\nsecond\nthird\n";
      check_visible "stable when priorities are equal"
        [ "first"; "second"; "third" ]
        "")

let test_visible_tasks_filters_by_description () =
  with_temp_cwd (fun _ ->
      write_file Todo_file.todo_file_path "buy milk\nsell milk\nwash car\n";
      check_visible "matches description" [ "buy milk" ] "buy")

let test_visible_tasks_filters_by_priority () =
  with_temp_cwd (fun _ ->
      write_file Todo_file.todo_file_path "(A) one\n(B) two\n(C) three\n";
      check_visible "matches priority marker" [ "(B) two" ] "(B)")

let test_visible_tasks_filters_by_due_date () =
  with_temp_cwd (fun _ ->
      write_file Todo_file.todo_file_path
        "due:2026-08-26 buy milk\nno due date\n";
      check_visible "matches due date"
        [ "due:2026-08-26 buy milk" ]
        "due:2026-08-26")

let test_visible_tasks_filters_by_status () =
  with_temp_cwd (fun _ ->
      write_file Todo_file.todo_file_path "x 2026-08-24 done\nstill active\n";
      check_visible "matches completion status" [ "x 2026-08-24 done" ]
        "x 2026-08-24")

let test_visible_tasks_filters_and_sorts () =
  with_temp_cwd (fun _ ->
      write_file Todo_file.todo_file_path
        "(C) buy milk\n(B) buy bread\n(A) wash car\n";
      check_visible "filters then sorts by priority"
        [ "(B) buy bread"; "(C) buy milk" ]
        "buy")

let test_visible_tasks_empty_file () =
  with_temp_cwd (fun _ ->
      write_file Todo_file.todo_file_path "";
      check_visible "empty file has no tasks" [] "")

let test_visible_tasks_ignores_invalid_lines () =
  with_temp_cwd (fun _ ->
      write_file Todo_file.todo_file_path "due:not-a-date\n\nbuy milk\n   \n";
      check_visible "keeps only valid tasks" [ "buy milk" ] "")

(* task_max_index *)

let check_max_index name expected pattern =
  Alcotest.(check int) name expected (App.task_max_index (make_state pattern))

let test_task_max_index_empty () =
  with_temp_cwd (fun _ ->
      write_file Todo_file.todo_file_path "";
      check_max_index "empty list is -1" (-1) "")

let test_task_max_index_single_task () =
  with_temp_cwd (fun _ ->
      write_file Todo_file.todo_file_path "buy milk\n";
      check_max_index "single task is 0" 0 "")

let test_task_max_index_multiple_tasks () =
  with_temp_cwd (fun _ ->
      write_file Todo_file.todo_file_path "one\ntwo\nthree\n";
      check_max_index "three tasks is 2" 2 "")

let test_task_max_index_respects_filter () =
  with_temp_cwd (fun _ ->
      write_file Todo_file.todo_file_path "buy milk\nsell milk\nwash car\n";
      check_max_index "one matching task is 0" 0 "buy")

let test_task_max_index_no_matches () =
  with_temp_cwd (fun _ ->
      write_file Todo_file.todo_file_path "buy milk\nsell milk\n";
      check_max_index "no matching tasks is -1" (-1) "zzz")

let suites =
  [
    ( "visible_tasks",
      [
        Alcotest.test_case "sorts by priority" `Quick
          test_visible_tasks_sorts_by_priority;
        Alcotest.test_case "stable for equal priority" `Quick
          test_visible_tasks_keeps_order_for_equal_priority;
        Alcotest.test_case "filters by description" `Quick
          test_visible_tasks_filters_by_description;
        Alcotest.test_case "filters by priority" `Quick
          test_visible_tasks_filters_by_priority;
        Alcotest.test_case "filters by due date" `Quick
          test_visible_tasks_filters_by_due_date;
        Alcotest.test_case "filters by status" `Quick
          test_visible_tasks_filters_by_status;
        Alcotest.test_case "filters then sorts" `Quick
          test_visible_tasks_filters_and_sorts;
        Alcotest.test_case "empty file" `Quick test_visible_tasks_empty_file;
        Alcotest.test_case "ignores invalid lines" `Quick
          test_visible_tasks_ignores_invalid_lines;
      ] );
    ( "task_max_index",
      [
        Alcotest.test_case "empty list" `Quick test_task_max_index_empty;
        Alcotest.test_case "single task" `Quick test_task_max_index_single_task;
        Alcotest.test_case "multiple tasks" `Quick
          test_task_max_index_multiple_tasks;
        Alcotest.test_case "respects filter" `Quick
          test_task_max_index_respects_filter;
        Alcotest.test_case "no matches" `Quick test_task_max_index_no_matches;
      ] );
  ]

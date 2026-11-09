open Kiroku

let make_date year month day = Task.{ year; month; day }

let make_task ?priority ?due_date status description =
  Task.{ status; priority; due_date; description }

let check_date_to_string name expected date =
  Alcotest.(check string) name expected (Task.date_to_string date)

let test_date_to_string_pads_components () =
  check_date_to_string "pads month and day" "2026-08-09" (make_date 2026 8 9)

let test_date_to_string_single_digit_month_and_day () =
  check_date_to_string "single digit month and day" "2026-01-05"
    (make_date 2026 1 5)

let check_status_char name expected status =
  Alcotest.(check (option char)) name expected (Task.status_to_char status)

let test_status_to_char_active () =
  check_status_char "active has no marker" None Task.Active

let test_status_to_char_completed () =
  check_status_char "completed uses x" (Some 'x')
    (Task.Completed (make_date 2026 8 24))

let test_status_to_char_archived () =
  check_status_char "archived uses ~" (Some '~')
    (Task.Archived (make_date 2026 8 24))

let check_status_to_string name expected status =
  Alcotest.(check string) name expected (Task.status_to_string status)

let test_status_to_string_active () =
  check_status_to_string "active is empty" "" Task.Active

let test_status_to_string_completed () =
  check_status_to_string "completed includes date" "x 2026-08-24"
    (Task.Completed (make_date 2026 8 24))

let test_status_to_string_archived () =
  check_status_to_string "archived includes date" "~ 2026-08-24"
    (Task.Archived (make_date 2026 8 24))

let check_task_to_string name expected task =
  Alcotest.(check string) name expected (Task.to_string task)

let test_task_to_string_active () =
  check_task_to_string "active task without optional fields" "buy milk"
    (make_task Task.Active "buy milk")

let test_task_to_string_priority () =
  check_task_to_string "active task with priority" "(A) buy milk"
    (make_task ~priority:'A' Task.Active "buy milk")

let test_task_to_string_due_date () =
  check_task_to_string "active task with due date" "due:2026-08-26 buy milk"
    (make_task ~due_date:(make_date 2026 8 26) Task.Active "buy milk")

let test_task_to_string_completed_with_all_fields () =
  check_task_to_string "completed task with all fields"
    "x 2026-08-24 (B) due:2026-08-26 read @ling413 paradigms in corpus ling"
    (make_task ~priority:'B' ~due_date:(make_date 2026 8 26)
       (Task.Completed (make_date 2026 8 24))
       "read @ling413 paradigms in corpus ling")

let test_task_to_string_archived () =
  check_task_to_string "archived task" "~ 2026-08-24 old task"
    (make_task (Task.Archived (make_date 2026 8 24)) "old task")

let test_task_to_string_empty_description () =
  check_task_to_string "drops empty description" "(A)"
    (make_task ~priority:'A' Task.Active "")

let test_task_to_string_pads_dates () =
  check_task_to_string "pads status and due dates"
    "x 2026-01-05 (A) due:2026-01-09 task"
    (make_task ~priority:'A' ~due_date:(make_date 2026 1 9)
       (Task.Completed (make_date 2026 1 5)) "task")

let suites =
  [
    ( "date_to_string",
      [
        Alcotest.test_case "pads components" `Quick
          test_date_to_string_pads_components;
        Alcotest.test_case "single digit month and day" `Quick
          test_date_to_string_single_digit_month_and_day;
      ] );
    ( "status",
      [
        Alcotest.test_case "to_char active" `Quick test_status_to_char_active;
        Alcotest.test_case "to_char completed" `Quick
          test_status_to_char_completed;
        Alcotest.test_case "to_char archived" `Quick test_status_to_char_archived;
        Alcotest.test_case "to_string active" `Quick test_status_to_string_active;
        Alcotest.test_case "to_string completed" `Quick
          test_status_to_string_completed;
        Alcotest.test_case "to_string archived" `Quick
          test_status_to_string_archived;
      ] );
    ( "task_to_string",
      [
        Alcotest.test_case "active" `Quick test_task_to_string_active;
        Alcotest.test_case "priority" `Quick test_task_to_string_priority;
        Alcotest.test_case "due date" `Quick test_task_to_string_due_date;
        Alcotest.test_case "completed with all fields" `Quick
          test_task_to_string_completed_with_all_fields;
        Alcotest.test_case "archived" `Quick test_task_to_string_archived;
        Alcotest.test_case "empty description" `Quick
          test_task_to_string_empty_description;
        Alcotest.test_case "pads dates" `Quick test_task_to_string_pads_dates;
      ] );
  ]

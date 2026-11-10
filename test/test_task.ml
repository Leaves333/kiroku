open Kiroku

let make_date year month day = Date.{ year; month; day }

let make_task ?priority ?due_date status description =
  Task.{ status; priority; due_date; description }

let check_date_to_string name expected date =
  Alcotest.(check string) name expected (Date.to_string date)

let test_date_to_string_pads_components () =
  check_date_to_string "pads month and day" "2026-08-09" (make_date 2026 8 9)

let test_date_to_string_single_digit_month_and_day () =
  check_date_to_string "single digit month and day" "2026-01-05"
    (make_date 2026 1 5)

let check_status_char name expected status =
  Alcotest.(check (option char)) name expected (Task.status_to_char status)

let test_status_to_char_active () =
  check_status_char "active has no marker" None Task.Status.Active

let test_status_to_char_completed () =
  check_status_char "completed uses x" (Some 'x')
    (Task.Status.Completed (make_date 2026 8 24))

let test_status_to_char_archived () =
  check_status_char "archived uses ~" (Some '~')
    (Task.Status.Archived (make_date 2026 8 24))

let check_status_to_string name expected status =
  Alcotest.(check string) name expected (Task.status_to_string status)

let test_status_to_string_active () =
  check_status_to_string "active is empty" "" Task.Status.Active

let test_status_to_string_completed () =
  check_status_to_string "completed includes date" "x 2026-08-24"
    (Task.Status.Completed (make_date 2026 8 24))

let test_status_to_string_archived () =
  check_status_to_string "archived includes date" "~ 2026-08-24"
    (Task.Status.Archived (make_date 2026 8 24))

let check_task_to_string name expected task =
  Alcotest.(check string) name expected (Task.to_string task)

let test_task_to_string_active () =
  check_task_to_string "active task without optional fields" "buy milk"
    (make_task Task.Status.Active "buy milk")

let test_task_to_string_priority () =
  check_task_to_string "active task with priority" "(A) buy milk"
    (make_task ~priority:'A' Task.Status.Active "buy milk")

let test_task_to_string_due_date () =
  check_task_to_string "active task with due date" "due:2026-08-26 buy milk"
    (make_task ~due_date:(make_date 2026 8 26) Task.Status.Active "buy milk")

let test_task_to_string_completed_with_all_fields () =
  check_task_to_string "completed task with all fields"
    "x 2026-08-24 (B) due:2026-08-26 read @ling413 paradigms in corpus ling"
    (make_task ~priority:'B' ~due_date:(make_date 2026 8 26)
       (Task.Status.Completed (make_date 2026 8 24))
       "read @ling413 paradigms in corpus ling")

let test_task_to_string_archived () =
  check_task_to_string "archived task" "~ 2026-08-24 old task"
    (make_task (Task.Status.Archived (make_date 2026 8 24)) "old task")

let test_task_to_string_empty_description () =
  check_task_to_string "drops empty description" "(A)"
    (make_task ~priority:'A' Task.Status.Active "")

let test_task_to_string_pads_dates () =
  check_task_to_string "pads status and due dates"
    "x 2026-01-05 (A) due:2026-01-09 task"
    (make_task ~priority:'A' ~due_date:(make_date 2026 1 9)
       (Task.Status.Completed (make_date 2026 1 5))
       "task")

(* parsing *)

let date_testable =
  Alcotest.testable
    (fun fmt date -> Format.pp_print_string fmt (Date.to_string date))
    ( = )

let status_testable =
  Alcotest.testable
    (fun fmt status ->
      match status with
      | Task.Status.Active -> Format.pp_print_string fmt "Active"
      | Task.Status.Completed date ->
          Format.fprintf fmt "Completed %s" (Date.to_string date)
      | Task.Status.Archived date ->
          Format.fprintf fmt "Archived %s" (Date.to_string date))
    ( = )

let task_testable =
  Alcotest.testable
    (fun fmt task -> Format.pp_print_string fmt (Task.to_string task))
    ( = )

let check_parsed_date name expected line =
  Alcotest.(check (option date_testable)) name expected (Date.parse line)

let check_parsed_status name expected line =
  Alcotest.(check (option (pair status_testable string)))
    name expected (Task.parse_status line)

let check_parsed_due_date name expected line =
  Alcotest.(check (option (pair (option date_testable) string)))
    name expected (Task.parse_due_date line)

let check_parsed_priority name expected line =
  Alcotest.(check (option (pair (option char) string)))
    name expected (Task.parse_priority line)

let check_parsed_task name expected line =
  Alcotest.(check (option task_testable)) name expected (Task.parse line)

let test_parse_date_valid () =
  check_parsed_date "parses a date" (Some (make_date 2026 8 24)) "2026-08-24"

let test_parse_date_padded () =
  check_parsed_date "parses padded date"
    (Some (make_date 2026 1 5))
    "2026-01-05"

let test_parse_date_invalid_text () =
  check_parsed_date "rejects non-date text" None "not-a-date"

let test_parse_date_missing_component () =
  check_parsed_date "rejects missing day" None "2026-08"

let test_parse_date_empty () = check_parsed_date "rejects empty string" None ""

let test_parse_date_trailing_text () =
  check_parsed_date "rejects trailing text" None "2026-08-24 rest"

let test_parse_status_active () =
  check_parsed_status "active has no marker"
    (Some (Task.Status.Active, "buy milk"))
    "buy milk"

let test_parse_status_completed () =
  check_parsed_status "completed consumes marker and date"
    (Some (Task.Status.Completed (make_date 2026 8 24), "buy milk"))
    "x 2026-08-24 buy milk"

let test_parse_status_archived () =
  check_parsed_status "archived consumes marker and date"
    (Some (Task.Status.Archived (make_date 2026 8 24), "old task"))
    "~ 2026-08-24 old task"

let test_parse_status_completed_without_description () =
  check_parsed_status "completed without description"
    (Some (Task.Status.Completed (make_date 2026 8 24), ""))
    "x 2026-08-24"

let test_parse_status_leaves_priority () =
  check_parsed_status "priority is left for later"
    (Some (Task.Status.Active, "(A) buy milk"))
    "(A) buy milk"

let test_parse_status_invalid_date () =
  check_parsed_status "rejects invalid status date" None "x not-a-date buy milk"

let test_parse_due_date_present () =
  check_parsed_due_date "parses due date"
    (Some (Some (make_date 2026 8 26), "buy milk"))
    "due:2026-08-26 buy milk"

let test_parse_due_date_absent () =
  check_parsed_due_date "no due date" (Some (None, "buy milk")) "buy milk"

let test_parse_due_date_without_description () =
  check_parsed_due_date "due date without description"
    (Some (Some (make_date 2026 8 26), ""))
    "due:2026-08-26"

let test_parse_due_date_invalid () =
  check_parsed_due_date "rejects invalid due date" None "due:not-a-date"

let test_parse_due_date_short_prefix () =
  check_parsed_due_date "rejects bare due prefix" None "due:";
  check_parsed_due_date "rejects truncated year" None "due:2026";
  check_parsed_due_date "rejects truncated date" None "due:2026-08"

let test_parse_priority_capital () =
  check_parsed_priority "accepts A" (Some (Some 'A', "buy milk")) "(A) buy milk";
  check_parsed_priority "accepts Z" (Some (Some 'Z', "task")) "(Z) task";
  check_parsed_priority "accepts prefix without description"
    (Some (Some 'A', ""))
    "(A) "

let test_parse_priority_rejects_lowercase () =
  check_parsed_priority "rejects (a)" None "(a) task"

let test_parse_priority_rejects_digit () =
  check_parsed_priority "rejects (1)" None "(1) task"

let test_parse_priority_rejects_symbol () =
  check_parsed_priority "rejects (_)" None "(_) task"

let test_parse_priority_ignores_malformed_prefix () =
  check_parsed_priority "multi-letter" (Some (None, "(AA) task")) "(AA) task";
  check_parsed_priority "no separator space" (Some (None, "(A)task")) "(A)task";
  check_parsed_priority "tab separator" (Some (None, "(A)\ttask")) "(A)\ttask";
  check_parsed_priority "leading space" (Some (None, " (A) task")) " (A) task"

let test_parse_task_active () =
  check_parsed_task "active task without optional fields"
    (Some (make_task Task.Status.Active "buy milk"))
    "buy milk"

let test_parse_task_priority () =
  check_parsed_task "active task with priority"
    (Some (make_task ~priority:'A' Task.Status.Active "buy milk"))
    "(A) buy milk"

let test_parse_task_due_date () =
  check_parsed_task "active task with due date"
    (Some
       (make_task ~due_date:(make_date 2026 8 26) Task.Status.Active "buy milk"))
    "due:2026-08-26 buy milk"

let test_parse_task_completed_with_all_fields () =
  check_parsed_task "completed task with all fields"
    (Some
       (make_task ~priority:'B' ~due_date:(make_date 2026 8 26)
          (Task.Status.Completed (make_date 2026 8 24))
          "read @ling413 paradigms in corpus ling"))
    "x 2026-08-24 (B) due:2026-08-26 read @ling413 paradigms in corpus ling"

let test_parse_task_archived () =
  check_parsed_task "archived task"
    (Some (make_task (Task.Status.Archived (make_date 2026 8 24)) "old task"))
    "~ 2026-08-24 old task"

let test_parse_task_empty () =
  check_parsed_task "empty line" None "";
  check_parsed_task "whitespace line" None "   "

let test_parse_task_trims () =
  check_parsed_task "trims surrounding whitespace"
    (Some (make_task ~priority:'A' Task.Status.Active "buy milk"))
    "  (A) buy milk  "

let test_parse_task_invalid_due_date () =
  check_parsed_task "rejects invalid due date" None "due:not-a-date buy milk"

let test_parse_task_short_due_date () =
  check_parsed_task "short due date does not raise" None "due:2026"

let test_parse_task_invalid_priority () =
  check_parsed_task "rejects lowercase priority" None "(a) task";
  check_parsed_task "rejects digit priority" None "(1) task"

let test_parse_task_roundtrip () =
  let tasks =
    [
      make_task Task.Status.Active "buy milk";
      make_task ~priority:'A' Task.Status.Active "buy milk";
      make_task ~due_date:(make_date 2026 8 26) Task.Status.Active "buy milk";
      make_task ~priority:'B' ~due_date:(make_date 2026 8 26)
        (Task.Status.Completed (make_date 2026 8 24))
        "read @ling413 paradigms in corpus ling";
      make_task (Task.Status.Archived (make_date 2026 8 24)) "old task";
    ]
  in
  List.iter
    (fun task ->
      check_parsed_task
        ("round-trips " ^ Task.to_string task)
        (Some task) (Task.to_string task))
    tasks

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
        Alcotest.test_case "to_char archived" `Quick
          test_status_to_char_archived;
        Alcotest.test_case "to_string active" `Quick
          test_status_to_string_active;
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
    ( "parse_date",
      [
        Alcotest.test_case "valid" `Quick test_parse_date_valid;
        Alcotest.test_case "padded" `Quick test_parse_date_padded;
        Alcotest.test_case "invalid text" `Quick test_parse_date_invalid_text;
        Alcotest.test_case "missing component" `Quick
          test_parse_date_missing_component;
        Alcotest.test_case "empty" `Quick test_parse_date_empty;
        Alcotest.test_case "trailing text" `Quick test_parse_date_trailing_text;
      ] );
    ( "parse_status",
      [
        Alcotest.test_case "active" `Quick test_parse_status_active;
        Alcotest.test_case "completed" `Quick test_parse_status_completed;
        Alcotest.test_case "archived" `Quick test_parse_status_archived;
        Alcotest.test_case "completed without description" `Quick
          test_parse_status_completed_without_description;
        Alcotest.test_case "leaves priority" `Quick
          test_parse_status_leaves_priority;
        Alcotest.test_case "invalid date" `Quick test_parse_status_invalid_date;
      ] );
    ( "parse_due_date",
      [
        Alcotest.test_case "present" `Quick test_parse_due_date_present;
        Alcotest.test_case "absent" `Quick test_parse_due_date_absent;
        Alcotest.test_case "without description" `Quick
          test_parse_due_date_without_description;
        Alcotest.test_case "invalid" `Quick test_parse_due_date_invalid;
        Alcotest.test_case "short prefix" `Quick
          test_parse_due_date_short_prefix;
      ] );
    ( "parse_priority",
      [
        Alcotest.test_case "accepts capital letters" `Quick
          test_parse_priority_capital;
        Alcotest.test_case "rejects lowercase" `Quick
          test_parse_priority_rejects_lowercase;
        Alcotest.test_case "rejects digit" `Quick
          test_parse_priority_rejects_digit;
        Alcotest.test_case "rejects symbol" `Quick
          test_parse_priority_rejects_symbol;
        Alcotest.test_case "ignores malformed prefix" `Quick
          test_parse_priority_ignores_malformed_prefix;
      ] );
    ( "parse_task",
      [
        Alcotest.test_case "active" `Quick test_parse_task_active;
        Alcotest.test_case "priority" `Quick test_parse_task_priority;
        Alcotest.test_case "due date" `Quick test_parse_task_due_date;
        Alcotest.test_case "completed with all fields" `Quick
          test_parse_task_completed_with_all_fields;
        Alcotest.test_case "archived" `Quick test_parse_task_archived;
        Alcotest.test_case "empty line" `Quick test_parse_task_empty;
        Alcotest.test_case "trims whitespace" `Quick test_parse_task_trims;
        Alcotest.test_case "invalid due date" `Quick
          test_parse_task_invalid_due_date;
        Alcotest.test_case "short due date" `Quick
          test_parse_task_short_due_date;
        Alcotest.test_case "invalid priority" `Quick
          test_parse_task_invalid_priority;
        Alcotest.test_case "round-trips to_string" `Quick
          test_parse_task_roundtrip;
      ] );
  ]

open Kiroku

let check_priority name expected line =
  Alcotest.(check (option char)) name expected (Utils.get_priority line)

let test_parses_A () = check_priority "parses (A)" (Some 'A') "(A) buy milk"
let test_no_prefix () = check_priority "no prefix" None "buy milk"

let test_lowercase_rejected () =
  check_priority "lowercase rejected" None "(a) task"

let test_short_line_rejected () =
  check_priority "short line rejected" None "(A)"

let test_no_space_rejected () =
  check_priority "no space rejected" None "(A)task"

let test_parses_B () = check_priority "parses (B)" (Some 'B') "(B) task"
let test_parses_Z () = check_priority "parses (Z)" (Some 'Z') "(Z) task"
let test_prefix_only () = check_priority "prefix only" (Some 'A') "(A) "

let test_double_space_accepted () =
  check_priority "double space accepted" (Some 'A') "(A)  task"

let test_leading_space_rejected () =
  check_priority "leading space rejected" None " (A) task"

let test_multi_letter_rejected () =
  check_priority "multi-letter prefix rejected" None "(AA) task"

let test_digit_rejected () =
  check_priority "digit prefix rejected" None "(1) task"

let test_tab_rejected () =
  check_priority "tab separator rejected" None "(A)\ttask"

let sign n = compare n 0

let check_comparison name expected a b =
  Alcotest.(check int) name expected (sign (Utils.compare_by_priority a b))

let test_both_priorities () =
  check_comparison "A sorts before B" (-1) "(A) task" "(B) task";
  check_comparison "B sorts after A" 1 "(B) task" "(A) task";
  check_comparison "equal priorities" 0 "(A) one" "(A) two"

let test_priority_before_no_priority () =
  check_comparison "priority before none" (-1) "(A) task" "no priority";
  check_comparison "none after priority" 1 "no priority" "(A) task";
  check_comparison "both none" 0 "no priority" "other no priority"

let check_bucket name expected key lines =
  Alcotest.(check (option (list string)))
    name expected
    (Utils.Priority_map.find_opt key (Utils.generate_priority_map lines))

let test_priority_map_empty () =
  Alcotest.(check bool)
    "empty map" true
    (Utils.Priority_map.is_empty (Utils.generate_priority_map []))

let test_priority_map_groups () =
  let lines = [ "(A) first"; "(B) second"; "(A) third"; "no priority" ] in
  check_bucket "A bucket" (Some [ "(A) third"; "(A) first" ]) (Some 'A') lines;
  check_bucket "B bucket" (Some [ "(B) second" ]) (Some 'B') lines;
  check_bucket "no priority bucket" (Some [ "no priority" ]) None lines

let test_priority_map_reverses_bucket () =
  check_bucket "newest insertion first"
    (Some [ "(A) two"; "(A) one" ])
    (Some 'A') [ "(A) one"; "(A) two" ]

let test_priority_map_bucket_count () =
  let lines = [ "(A) first"; "(B) second"; "no priority" ] in
  Alcotest.(check int)
    "one bucket per distinct priority" 3
    (Utils.Priority_map.cardinal (Utils.generate_priority_map lines))

let check_filtered name expected pattern lines =
  Alcotest.(check (list string))
    name expected
    (Filter.filter_lines pattern lines)

let test_empty_pattern_matches_all () =
  check_filtered "empty pattern"
    [ "first"; "second"; "third" ]
    ""
    [ "first"; "second"; "third" ]

let test_matches_substring () =
  check_filtered "substring matches" [ "buy milk"; "rebuy" ] "buy"
    [ "buy milk"; "rebuy"; "sell" ]

let test_case_sensitive () =
  check_filtered "case sensitive" [ "Buy milk" ] "Buy"
    [ "buy milk"; "Buy milk" ]

let test_preserves_order () =
  check_filtered "preserves order" [ "banana"; "apple" ] "a"
    [ "banana"; "apple"; "cherry" ]

let test_no_matches () =
  check_filtered "no matches" [] "zzz" [ "buy milk"; "sell" ]

let test_empty_input () = check_filtered "empty input" [] "buy" []

let test_pattern_longer_than_line () =
  check_filtered "pattern longer than line" [] "very long pattern" [ "short" ]

let test_treats_pattern_literally () =
  check_filtered "dot is literal" [ "a.b" ] "." [ "a.b"; "ab" ]

let test_delete_last_char () =
  Alcotest.(check string) "empty stays empty" "" (Views.delete_last_char "");
  Alcotest.(check string) "single char" "" (Views.delete_last_char "a");
  Alcotest.(check string) "drops last char" "ab" (Views.delete_last_char "abc")

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

let () =
  Alcotest.run "kiroku"
    [
      ( "get_priority",
        [
          Alcotest.test_case "parses (A)" `Quick test_parses_A;
          Alcotest.test_case "no prefix" `Quick test_no_prefix;
          Alcotest.test_case "lowercase rejected" `Quick test_lowercase_rejected;
          Alcotest.test_case "short line rejected" `Quick
            test_short_line_rejected;
          Alcotest.test_case "no space rejected" `Quick test_no_space_rejected;
          Alcotest.test_case "parses (B)" `Quick test_parses_B;
          Alcotest.test_case "parses (Z)" `Quick test_parses_Z;
          Alcotest.test_case "prefix only" `Quick test_prefix_only;
          Alcotest.test_case "double space accepted" `Quick
            test_double_space_accepted;
          Alcotest.test_case "leading space rejected" `Quick
            test_leading_space_rejected;
          Alcotest.test_case "multi-letter prefix rejected" `Quick
            test_multi_letter_rejected;
          Alcotest.test_case "digit prefix rejected" `Quick test_digit_rejected;
          Alcotest.test_case "tab separator rejected" `Quick test_tab_rejected;
        ] );
      ( "compare_by_priority",
        [
          Alcotest.test_case "both priorities" `Quick test_both_priorities;
          Alcotest.test_case "priority before no priority" `Quick
            test_priority_before_no_priority;
        ] );
      ( "generate_priority_map",
        [
          Alcotest.test_case "empty map" `Quick test_priority_map_empty;
          Alcotest.test_case "groups by priority" `Quick
            test_priority_map_groups;
          Alcotest.test_case "reverses bucket" `Quick
            test_priority_map_reverses_bucket;
          Alcotest.test_case "bucket count" `Quick
            test_priority_map_bucket_count;
        ] );
      ( "filter_lines",
        [
          Alcotest.test_case "empty pattern matches all" `Quick
            test_empty_pattern_matches_all;
          Alcotest.test_case "matches substring" `Quick test_matches_substring;
          Alcotest.test_case "case sensitive" `Quick test_case_sensitive;
          Alcotest.test_case "preserves order" `Quick test_preserves_order;
          Alcotest.test_case "no matches" `Quick test_no_matches;
          Alcotest.test_case "empty input" `Quick test_empty_input;
          Alcotest.test_case "pattern longer than line" `Quick
            test_pattern_longer_than_line;
          Alcotest.test_case "treats pattern literally" `Quick
            test_treats_pattern_literally;
        ] );
      ( "delete_last_char",
        [
          Alcotest.test_case "deletes last character" `Quick
            test_delete_last_char;
        ] );
      ( "highlight_matches",
        [
          Alcotest.test_case "preserves width" `Quick
            test_highlight_preserves_width;
        ] );
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
    ]

open Kiroku

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

let task_of_line line = Option.get (Task.parse line)

let check_filtered_tasks name expected pattern lines =
  Alcotest.(check (list string))
    name expected
    (Filter.filter_tasks pattern (List.map task_of_line lines)
    |> List.map Task.to_string)

let test_tasks_empty_pattern_matches_all () =
  check_filtered_tasks "empty pattern"
    [ "buy milk"; "(A) call mom"; "x 2026-08-24 done" ]
    ""
    [ "buy milk"; "(A) call mom"; "x 2026-08-24 done" ]

let test_tasks_matches_description () =
  check_filtered_tasks "description substring" [ "buy milk"; "rebuy" ] "buy"
    [ "buy milk"; "rebuy"; "sell" ]

let test_tasks_matches_priority () =
  check_filtered_tasks "priority marker" [ "(A) call mom" ] "(A)"
    [ "(A) call mom"; "(B) pay bills" ]

let test_tasks_matches_due_date () =
  check_filtered_tasks "due date"
    [ "due:2026-08-26 buy milk" ]
    "due:2026-08-26"
    [ "due:2026-08-26 buy milk"; "no due date" ]

let test_tasks_matches_status () =
  check_filtered_tasks "status marker" [ "x 2026-08-24 done" ] "x 2026-08-24"
    [ "x 2026-08-24 done"; "still active" ]

let test_tasks_case_sensitive () =
  check_filtered_tasks "case sensitive" [ "Buy milk" ] "Buy"
    [ "buy milk"; "Buy milk" ]

let test_tasks_preserves_order () =
  check_filtered_tasks "preserves order" [ "banana"; "apple" ] "a"
    [ "banana"; "apple"; "cherry" ]

let test_tasks_no_matches () =
  check_filtered_tasks "no matches" [] "zzz" [ "buy milk"; "sell" ]

let test_tasks_empty_input () = check_filtered_tasks "empty input" [] "buy" []

let suites =
  [
    ( "filter_tasks",
      [
        Alcotest.test_case "empty pattern matches all" `Quick
          test_tasks_empty_pattern_matches_all;
        Alcotest.test_case "matches description" `Quick
          test_tasks_matches_description;
        Alcotest.test_case "matches priority" `Quick test_tasks_matches_priority;
        Alcotest.test_case "matches due date" `Quick test_tasks_matches_due_date;
        Alcotest.test_case "matches status" `Quick test_tasks_matches_status;
        Alcotest.test_case "case sensitive" `Quick test_tasks_case_sensitive;
        Alcotest.test_case "preserves order" `Quick test_tasks_preserves_order;
        Alcotest.test_case "no matches" `Quick test_tasks_no_matches;
        Alcotest.test_case "empty input" `Quick test_tasks_empty_input;
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
  ]

open Kiroku

let get_priority line =
  match Task.parse_priority line with
  | Some (priority, _) -> priority
  | None -> None

let check_priority name expected line =
  Alcotest.(check (option char)) name expected (get_priority line)

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

let compare_by_priority a b =
  let task line = Option.get (Task.parse_task line) in
  Task.compare_by_priority (task a) (task b)

let check_comparison name expected a b =
  Alcotest.(check int) name expected (sign (compare_by_priority a b))

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

let suites =
  [
    ( "get_priority",
      [
        Alcotest.test_case "parses (A)" `Quick test_parses_A;
        Alcotest.test_case "no prefix" `Quick test_no_prefix;
        Alcotest.test_case "lowercase rejected" `Quick test_lowercase_rejected;
        Alcotest.test_case "short line rejected" `Quick test_short_line_rejected;
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
        Alcotest.test_case "groups by priority" `Quick test_priority_map_groups;
        Alcotest.test_case "reverses bucket" `Quick
          test_priority_map_reverses_bucket;
        Alcotest.test_case "bucket count" `Quick test_priority_map_bucket_count;
      ] );
  ]

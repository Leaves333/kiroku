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

let suites =
  [
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

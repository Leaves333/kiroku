open Kiroku

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
  ]

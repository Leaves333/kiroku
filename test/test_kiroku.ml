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
        ] );
    ]

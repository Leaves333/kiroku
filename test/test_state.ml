open Kiroku

let make_state ?(focus = State.Task) ?(task_index = 0) ?(filter_pattern = "") ()
    =
  State.{ focus; task_index; filter_pattern }

let check_int name expected actual = Alcotest.(check int) name expected actual

let check_string name expected actual =
  Alcotest.(check string) name expected actual

(* focus_to_string *)

let test_focus_to_string_task () =
  check_string "task focus" "Task" (State.focus_to_string State.Task)

let test_focus_to_string_filter () =
  check_string "filter focus" "Filter" (State.focus_to_string State.Filter)

(* filter_pattern_append *)

let test_append_to_empty_pattern () =
  let state = make_state () in
  let state = State.filter_pattern_append 'a' state in
  check_string "appends to empty" "a" state.filter_pattern

let test_append_to_existing_pattern () =
  let state = make_state ~filter_pattern:"ab" () in
  let state = State.filter_pattern_append 'c' state in
  check_string "appends to existing" "abc" state.filter_pattern

let test_append_space () =
  let state = make_state ~filter_pattern:"buy" () in
  let state = State.filter_pattern_append ' ' state in
  check_string "appends space" "buy " state.filter_pattern

let test_append_preserves_other_fields () =
  let state = make_state ~focus:State.Filter ~task_index:3 () in
  let state = State.filter_pattern_append 'x' state in
  check_string "focus preserved" "Filter" (State.focus_to_string state.focus);
  check_int "task_index preserved" 3 state.task_index

(* filter_pattern_backspace *)

let test_backspace_empty_pattern () =
  let state = make_state () in
  let state = State.filter_pattern_backspace state in
  check_string "empty stays empty" "" state.filter_pattern

let test_backspace_single_character () =
  let state = make_state ~filter_pattern:"a" () in
  let state = State.filter_pattern_backspace state in
  check_string "single character becomes empty" "" state.filter_pattern

let test_backspace_multiple_characters () =
  let state = make_state ~filter_pattern:"abc" () in
  let state = State.filter_pattern_backspace state in
  check_string "drops last character" "ab" state.filter_pattern

let test_backspace_preserves_other_fields () =
  let state =
    make_state ~focus:State.Filter ~task_index:2 ~filter_pattern:"abc" ()
  in
  let state = State.filter_pattern_backspace state in
  check_string "focus preserved" "Filter" (State.focus_to_string state.focus);
  check_int "task_index preserved" 2 state.task_index

(* reset_task_index *)

let test_reset_task_index () =
  let state = make_state ~task_index:5 () in
  check_int "resets to zero" 0 (State.reset_task_index state).task_index

let test_reset_task_index_preserves_other_fields () =
  let state =
    make_state ~focus:State.Filter ~task_index:5 ~filter_pattern:"abc" ()
  in
  let state = State.reset_task_index state in
  check_string "focus preserved" "Filter" (State.focus_to_string state.focus);
  check_string "pattern preserved" "abc" state.filter_pattern

(* set_task_index *)

let test_set_task_index_positive () =
  let state = make_state () in
  check_int "sets positive index" 3 (State.set_task_index 3 state).task_index

let test_set_task_index_zero () =
  let state = make_state ~task_index:4 () in
  check_int "sets zero" 0 (State.set_task_index 0 state).task_index

let test_set_task_index_negative () =
  let state = make_state () in
  check_int "sets negative index" (-1)
    (State.set_task_index (-1) state).task_index

let test_set_task_index_preserves_other_fields () =
  let state = make_state ~focus:State.Filter ~filter_pattern:"abc" () in
  let state = State.set_task_index 2 state in
  check_string "focus preserved" "Filter" (State.focus_to_string state.focus);
  check_string "pattern preserved" "abc" state.filter_pattern

(* move_task_index *)

let test_move_task_index_down () =
  let state = make_state ~task_index:1 () in
  check_int "moves down" 2 (State.move_task_index 3 1 state).task_index

let test_move_task_index_up () =
  let state = make_state ~task_index:1 () in
  check_int "moves up" 0 (State.move_task_index 3 (-1) state).task_index

let test_move_task_index_clamps_at_zero () =
  let state = make_state ~task_index:0 () in
  check_int "clamps at zero" 0 (State.move_task_index 3 (-1) state).task_index

let test_move_task_index_clamps_at_max () =
  let state = make_state ~task_index:3 () in
  check_int "clamps at max" 3 (State.move_task_index 3 1 state).task_index

let test_move_task_index_clamps_from_above_max () =
  let state = make_state ~task_index:5 () in
  check_int "clamps from above max" 3
    (State.move_task_index 3 1 state).task_index

let test_move_task_index_empty_list_sentinel () =
  let state = make_state ~task_index:0 () in
  check_int "empty list keeps sentinel -1" (-1)
    (State.move_task_index (-1) 1 state).task_index

let test_move_task_index_zero_amount () =
  let state = make_state ~task_index:2 () in
  check_int "leaves index alone" 2 (State.move_task_index 3 0 state).task_index

let test_move_task_index_preserves_other_fields () =
  let state =
    make_state ~focus:State.Filter ~task_index:1 ~filter_pattern:"abc" ()
  in
  let state = State.move_task_index 3 1 state in
  check_string "focus preserved" "Filter" (State.focus_to_string state.focus);
  check_string "pattern preserved" "abc" state.filter_pattern

let suites =
  [
    ( "focus_to_string",
      [
        Alcotest.test_case "task" `Quick test_focus_to_string_task;
        Alcotest.test_case "filter" `Quick test_focus_to_string_filter;
      ] );
    ( "filter_pattern_append",
      [
        Alcotest.test_case "appends to empty" `Quick
          test_append_to_empty_pattern;
        Alcotest.test_case "appends to existing" `Quick
          test_append_to_existing_pattern;
        Alcotest.test_case "appends a space" `Quick test_append_space;
        Alcotest.test_case "preserves other fields" `Quick
          test_append_preserves_other_fields;
      ] );
    ( "filter_pattern_backspace",
      [
        Alcotest.test_case "empty stays empty" `Quick
          test_backspace_empty_pattern;
        Alcotest.test_case "single character" `Quick
          test_backspace_single_character;
        Alcotest.test_case "multiple characters" `Quick
          test_backspace_multiple_characters;
        Alcotest.test_case "preserves other fields" `Quick
          test_backspace_preserves_other_fields;
      ] );
    ( "reset_task_index",
      [
        Alcotest.test_case "resets to zero" `Quick test_reset_task_index;
        Alcotest.test_case "preserves other fields" `Quick
          test_reset_task_index_preserves_other_fields;
      ] );
    ( "set_task_index",
      [
        Alcotest.test_case "positive index" `Quick test_set_task_index_positive;
        Alcotest.test_case "zero" `Quick test_set_task_index_zero;
        Alcotest.test_case "negative index" `Quick test_set_task_index_negative;
        Alcotest.test_case "preserves other fields" `Quick
          test_set_task_index_preserves_other_fields;
      ] );
    ( "move_task_index",
      [
        Alcotest.test_case "moves down" `Quick test_move_task_index_down;
        Alcotest.test_case "moves up" `Quick test_move_task_index_up;
        Alcotest.test_case "clamps at zero" `Quick
          test_move_task_index_clamps_at_zero;
        Alcotest.test_case "clamps at max" `Quick
          test_move_task_index_clamps_at_max;
        Alcotest.test_case "clamps from above max" `Quick
          test_move_task_index_clamps_from_above_max;
        Alcotest.test_case "empty list sentinel" `Quick
          test_move_task_index_empty_list_sentinel;
        Alcotest.test_case "zero amount" `Quick test_move_task_index_zero_amount;
        Alcotest.test_case "preserves other fields" `Quick
          test_move_task_index_preserves_other_fields;
      ] );
  ]

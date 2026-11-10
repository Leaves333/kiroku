open Kiroku

let write_file path contents =
  Out_channel.with_open_text path (fun oc ->
      Out_channel.output_string oc contents)

let read_file path = In_channel.with_open_text path In_channel.input_all

let with_temp_dir f =
  let dir = Filename.temp_file "kiroku-todo-file-" "" in
  Sys.remove dir;
  Unix.mkdir dir 0o700;
  Fun.protect
    ~finally:(fun () ->
      Array.iter
        (fun name -> Unix.unlink (Filename.concat dir name))
        (Sys.readdir dir);
      Unix.rmdir dir)
    (fun () -> f dir)

(* The todo.txt path is hardcoded, so tests for read/write run in a temp cwd. *)
let with_temp_cwd f =
  let original = Sys.getcwd () in
  with_temp_dir (fun dir ->
      Sys.chdir dir;
      Fun.protect ~finally:(fun () -> Sys.chdir original) (fun () -> f dir))

let check_raises_sys_error name f =
  match f () with
  | _ -> Alcotest.fail (name ^ ": expected Sys_error")
  | exception Sys_error _ -> ()

(* read_lines *)

let test_read_lines_reads_lines () =
  with_temp_dir (fun dir ->
      let path = Filename.concat dir "lines.txt" in
      write_file path "first\nsecond\n";
      Alcotest.(check (list string))
        "reads lines in order" [ "first"; "second" ]
        (Todo_file.read_lines path))

let test_read_lines_trims () =
  with_temp_dir (fun dir ->
      let path = Filename.concat dir "lines.txt" in
      write_file path "  first  \n\tsecond\t\nthird\r\n";
      Alcotest.(check (list string))
        "trims surrounding whitespace"
        [ "first"; "second"; "third" ]
        (Todo_file.read_lines path))

let test_read_lines_skips_blank_lines () =
  with_temp_dir (fun dir ->
      let path = Filename.concat dir "lines.txt" in
      write_file path "\nfirst\n\nsecond\n\n";
      Alcotest.(check (list string))
        "skips blank lines" [ "first"; "second" ]
        (Todo_file.read_lines path))

let test_read_lines_keeps_whitespace_only_lines () =
  with_temp_dir (fun dir ->
      let path = Filename.concat dir "lines.txt" in
      write_file path "   \nfirst\n";
      Alcotest.(check (list string))
        "whitespace-only lines become empty strings" [ ""; "first" ]
        (Todo_file.read_lines path))

let test_read_lines_empty_file () =
  with_temp_dir (fun dir ->
      let path = Filename.concat dir "lines.txt" in
      write_file path "";
      Alcotest.(check (list string)) "empty file" [] (Todo_file.read_lines path);
      write_file path "\n\n";
      Alcotest.(check (list string))
        "blank lines only" []
        (Todo_file.read_lines path))

let test_read_lines_missing_file () =
  with_temp_dir (fun dir ->
      let path = Filename.concat dir "missing.txt" in
      check_raises_sys_error "missing file" (fun () ->
          ignore (Todo_file.read_lines path)))

(* write_lines *)

let test_write_lines_appends_newline () =
  with_temp_dir (fun dir ->
      let path = Filename.concat dir "lines.txt" in
      Todo_file.write_lines path [ "first"; "second" ];
      Alcotest.(check string)
        "writes a trailing newline" "first\nsecond\n" (read_file path));
  with_temp_dir (fun dir ->
      let path = Filename.concat dir "lines.txt" in
      Todo_file.write_lines path [ "only" ];
      Alcotest.(check string) "single line" "only\n" (read_file path))

let test_write_lines_empty () =
  with_temp_dir (fun dir ->
      let path = Filename.concat dir "lines.txt" in
      Todo_file.write_lines path [];
      Alcotest.(check string) "writes an empty file" "" (read_file path))

let test_write_lines_overwrites () =
  with_temp_dir (fun dir ->
      let path = Filename.concat dir "lines.txt" in
      write_file path "old\ncontents\n";
      Todo_file.write_lines path [ "new" ];
      Alcotest.(check string) "replaces contents" "new\n" (read_file path))

let test_write_lines_roundtrip () =
  with_temp_dir (fun dir ->
      let path = Filename.concat dir "lines.txt" in
      let lines = [ "first"; "second"; "third" ] in
      Todo_file.write_lines path lines;
      Alcotest.(check (list string))
        "read_lines writes what it read" lines
        (Todo_file.read_lines path))

(* read *)

let test_read_parses_tasks () =
  with_temp_cwd (fun _ ->
      write_file Todo_file.todo_file_path
        "buy milk\n(A) call mom\nx 2026-08-24 done\n";
      let lines = List.map Task.to_string (Todo_file.read ()) in
      Alcotest.(check (list string))
        "parses each line as a task"
        [ "buy milk"; "(A) call mom"; "x 2026-08-24 done" ]
        lines)

let test_read_ignores_blank_and_invalid_lines () =
  with_temp_cwd (fun _ ->
      write_file Todo_file.todo_file_path
        "\nbuy milk\ndue:not-a-date\n(a) task\n   \nsecond\n";
      let lines = List.map Task.to_string (Todo_file.read ()) in
      Alcotest.(check (list string))
        "keeps only valid tasks" [ "buy milk"; "second" ] lines)

let test_read_empty_file () =
  with_temp_cwd (fun _ ->
      write_file Todo_file.todo_file_path "";
      Alcotest.(check bool) "empty todo file" true (Todo_file.read () = []))

let test_read_missing_file () =
  with_temp_cwd (fun _ ->
      check_raises_sys_error "missing todo file" (fun () ->
          ignore (Todo_file.read ())))

(* write *)

let task_of_line line = Option.get (Task.parse line)

let test_write_serializes_tasks () =
  with_temp_cwd (fun _ ->
      let tasks =
        List.map task_of_line
          [
            "buy milk";
            "(A) call mom";
            "x 2026-08-24 (B) due:2026-08-26 finished";
            "~ 2026-08-24 old task";
          ]
      in
      Todo_file.write tasks;
      Alcotest.(check string)
        "writes tasks with a trailing newline"
        "buy milk\n\
         (A) call mom\n\
         x 2026-08-24 (B) due:2026-08-26 finished\n\
         ~ 2026-08-24 old task\n"
        (read_file Todo_file.todo_file_path))

let test_write_empty_tasks () =
  with_temp_cwd (fun _ ->
      write_file Todo_file.todo_file_path "old\n";
      Todo_file.write [];
      Alcotest.(check string)
        "empty task list writes an empty file" ""
        (read_file Todo_file.todo_file_path))

let test_write_read_roundtrip () =
  with_temp_cwd (fun _ ->
      let lines =
        [
          "buy milk";
          "(A) call mom";
          "x 2026-08-24 (B) due:2026-08-26 finished";
          "~ 2026-08-24 old task";
        ]
      in
      Todo_file.write (List.map task_of_line lines);
      let result = List.map Task.to_string (Todo_file.read ()) in
      Alcotest.(check (list string)) "write then read" lines result)

let suites =
  [
    ( "read_lines",
      [
        Alcotest.test_case "reads lines" `Quick test_read_lines_reads_lines;
        Alcotest.test_case "trims whitespace" `Quick test_read_lines_trims;
        Alcotest.test_case "skips blank lines" `Quick
          test_read_lines_skips_blank_lines;
        Alcotest.test_case "keeps whitespace-only lines" `Quick
          test_read_lines_keeps_whitespace_only_lines;
        Alcotest.test_case "empty file" `Quick test_read_lines_empty_file;
        Alcotest.test_case "missing file" `Quick test_read_lines_missing_file;
      ] );
    ( "write_lines",
      [
        Alcotest.test_case "appends newline" `Quick
          test_write_lines_appends_newline;
        Alcotest.test_case "empty list" `Quick test_write_lines_empty;
        Alcotest.test_case "overwrites" `Quick test_write_lines_overwrites;
        Alcotest.test_case "round-trips read_lines" `Quick
          test_write_lines_roundtrip;
      ] );
    ( "read",
      [
        Alcotest.test_case "parses tasks" `Quick test_read_parses_tasks;
        Alcotest.test_case "ignores blank and invalid lines" `Quick
          test_read_ignores_blank_and_invalid_lines;
        Alcotest.test_case "empty file" `Quick test_read_empty_file;
        Alcotest.test_case "missing file" `Quick test_read_missing_file;
      ] );
    ( "write",
      [
        Alcotest.test_case "serializes tasks" `Quick test_write_serializes_tasks;
        Alcotest.test_case "empty task list" `Quick test_write_empty_tasks;
        Alcotest.test_case "round-trips read" `Quick test_write_read_roundtrip;
      ] );
  ]

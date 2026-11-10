let () =
  Alcotest.run "kiroku"
    (List.concat
       [
         Test_filter.suites;
         Test_views.suites;
         Test_todo_file.suites;
         Test_task.suites;
       ])

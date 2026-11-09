let () =
  Alcotest.run "kiroku"
    (List.concat
       [
         Test_utils.suites;
         Test_filter.suites;
         Test_views.suites;
         Test_task.suites;
       ])

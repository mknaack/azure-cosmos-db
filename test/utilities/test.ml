let () =
  Lwt_main.run
  @@ Alcotest_lwt.run "Main tests" [ ("utility test", Test_utilities.test) ]

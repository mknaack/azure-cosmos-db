let () =
  Lwt_main.run
  @@ Alcotest_lwt.run "Main tests"
       [
         ("app config test", Test_app_config.test);
         ("utility cosmos test", Test_cosmos_utility.utility_test);
         ("integration test", Integration_test.test);
         ("partition key test", Integration_test.test_partition_key);
         ("user test", Users_test.test);
         ("utility test", Test_utilities.test);
       ]

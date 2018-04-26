open OUnit2

let suite = "azure cosmos db test" >:::
  [
   "azure cosmos db test" >::: Test_utility.tests;
 ]

let _ =
  run_test_tt_main suite

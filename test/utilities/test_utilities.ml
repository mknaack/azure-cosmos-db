let x_ms_date () =
  let value = Utilities.Ms_time.create 123.0 in
  let result = Utilities.Ms_time.x_ms_date value in
  let expected_result = "Thu, 01 Jan 1970 00:02:03 GMT" in
  Alcotest.(check string) "Same string" expected_result result

let weekday_of_tm_wday () =
  Alcotest.(check string)
    "Weekday Sun" "Sun"
    (Utilities.Ms_time.weekday_of_tm_wday 0);
  Alcotest.(check string)
    "Weekday Mon" "Mon"
    (Utilities.Ms_time.weekday_of_tm_wday 1);
  Alcotest.(check string)
    "Weekday Tue" "Tue"
    (Utilities.Ms_time.weekday_of_tm_wday 2);
  Alcotest.(check string)
    "Weekday Wed" "Wed"
    (Utilities.Ms_time.weekday_of_tm_wday 3);
  Alcotest.(check string)
    "Weekday Thu" "Thu"
    (Utilities.Ms_time.weekday_of_tm_wday 4);
  Alcotest.(check string)
    "Weekday Fri" "Fri"
    (Utilities.Ms_time.weekday_of_tm_wday 5);
  Alcotest.(check string)
    "Weekday Sat" "Sat"
    (Utilities.Ms_time.weekday_of_tm_wday 6);
  Alcotest.(check_raises)
    "Weekday out of bound" (Failure "Day number unknown: 7") (fun () ->
      let _ = Utilities.Ms_time.weekday_of_tm_wday 7 in
      ())

let month_of_tm_mon () =
  Alcotest.(check string) "Month" "Jan" (Utilities.Ms_time.month_of_tm_mon 0);
  Alcotest.(check string) "Month" "Feb" (Utilities.Ms_time.month_of_tm_mon 1);
  Alcotest.(check string) "Month" "Mar" (Utilities.Ms_time.month_of_tm_mon 2);
  Alcotest.(check string) "Month" "Apr" (Utilities.Ms_time.month_of_tm_mon 3);
  Alcotest.(check string) "Month" "May" (Utilities.Ms_time.month_of_tm_mon 4);
  Alcotest.(check string) "Month" "Jun" (Utilities.Ms_time.month_of_tm_mon 5);
  Alcotest.(check string) "Month" "Jul" (Utilities.Ms_time.month_of_tm_mon 6);
  Alcotest.(check string) "Month" "Aug" (Utilities.Ms_time.month_of_tm_mon 7);
  Alcotest.(check string) "Month" "Sep" (Utilities.Ms_time.month_of_tm_mon 8);
  Alcotest.(check string) "Month" "Oct" (Utilities.Ms_time.month_of_tm_mon 9);
  Alcotest.(check string) "Month" "Nov" (Utilities.Ms_time.month_of_tm_mon 10);
  Alcotest.(check string) "Month" "Dec" (Utilities.Ms_time.month_of_tm_mon 11);
  Alcotest.(check_raises)
    "Month out of bound" (Failure "Month number unknown: 12") (fun () ->
      let _ = Utilities.Ms_time.month_of_tm_mon 12 in
      ())

let test =
  let open Alcotest_lwt in
  [
    test_case_sync "x_ms_date" `Quick x_ms_date;
    test_case_sync "weekday_of_tm_wday" `Quick weekday_of_tm_wday;
    test_case_sync "month_of_tm_mon" `Quick month_of_tm_mon;
  ]

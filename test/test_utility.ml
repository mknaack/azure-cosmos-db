open Cosmos

let x_ms_date () =
  let value = 123.0 in
  let result = Utility.x_ms_date value in
  let expected_result = "Thu, 01 Jan 1970 00:02:03 GMT" in
  Alcotest.(check string) "Same string" expected_result result

let adjust_host_no_change () =
  let test = "test.documents.azure.com" in
  let expected_result = test in
  let result = Utility.adjust_host test in
  Alcotest.(check string) "Same string" expected_result result

let adjust_host_change () =
  let test = "test" in
  let expected_result = "test.documents.azure.com" in
  let result = Utility.adjust_host test in
  Alcotest.(check string) "Same string" expected_result result

let auth_key_test () =
  let verb = "GET" in
  let resource_type = "dbs" in
  let resource_link = "dbs/ToDoList" in
  let date = "Thu, 27 Apr 2017 00:51:12 GMT" in
  let master_key =
    "dsZQi3KtZmCv1ljt3VNWNm7sQUF1y5rJfC6kv5JiwvW0EndXdDku/dkKBp8/ufDToSxLzR4y+O/0H/t4bQtVNw=="
  in
  let result =
    Utility.authorization_token_using_master_key verb resource_type
      resource_link date master_key
  in
  let expected_result =
    "type%3dmaster%26ver%3d1.0%26sig%3dc09PEVJrgp2uQRkr934kFbTqhByc7TVr3OHyqlu%2bc%2bc%3d"
  in
  Alcotest.(check string) "Same string" expected_result result

let auth_key_test_get_list () =
  let verb = "GET" in
  let resource_type = "dbs" in
  let resource_link = "" in
  let date = "Wed, 16 May 2018 19:47:46 GMT" in
  let master_key =
    "SB1mrDcsPfPnHN2lCLYLTXDJMEqXsjvWqS2BXbvBbro94dxVHem3gyXKLPruSeMVE7ZKf36EGC5ArCkJqJaoOg=="
  in
  let result =
    Utility.authorization_token_using_master_key verb resource_type
      resource_link date master_key
  in
  let expected_result =
    "type%3dmaster%26ver%3d1.0%26sig%3dcK0fCpDW9YvCbmmrIVxaGL%2fq9o%2flFlPc8GdCphvow3c%3d"
  in
  Alcotest.(check string) "Same string" expected_result result

let convert_list_databases_test () =
  let data =
    "{\"_rid\":\"\",\"Databases\":[{\"id\":\"test\",\"_rid\":\"1zxpAA==\",\"_self\":\"dbs\\/1zxpAA==\\/\",\"_etag\":\"\\\"00007e01-0000-0000-0000-5b0042840000\\\"\",\"_colls\":\"colls\\/\",\"_users\":\"users\\/\",\"_ts\":152674368}],\"_count\":1}"
  in
  let result = Json_converter_j.list_databases_of_string data in
  let expected_databases =
    ({
       id = "test";
       _rid = "1zxpAA==";
       _self = "dbs/1zxpAA==/";
       _etag = "\"00007e01-0000-0000-0000-5b0042840000\"";
       _colls = "colls/";
       _users = "users/";
       _ts = 152674368;
     }
      : Cosmos.Json_converter_j.database)
  in
  let expected_result =
    ({ _rid = ""; databases = [ expected_databases ]; _count = 1 }
      : Cosmos.Json_converter_j.list_databases)
  in
  let is_same = expected_result = result in
  Alcotest.(check bool) "Same record" true is_same

let string_of_bool_true () =
  let result = Utility.string_of_bool true in
  let expected_result = "true" in
  Alcotest.(check string) "Same string" expected_result result

let string_of_bool_false () =
  let result = Utility.string_of_bool false in
  let expected_result = "false" in
  Alcotest.(check string) "Same string" expected_result result

let weekday_of_tm_wday () =
  Alcotest.(check string) "Weekday Sun" "Sun" (Utility.weekday_of_tm_wday 0);
  Alcotest.(check string) "Weekday Mon" "Mon" (Utility.weekday_of_tm_wday 1);
  Alcotest.(check string) "Weekday Tue" "Tue" (Utility.weekday_of_tm_wday 2);
  Alcotest.(check string) "Weekday Wed" "Wed" (Utility.weekday_of_tm_wday 3);
  Alcotest.(check string) "Weekday Thu" "Thu" (Utility.weekday_of_tm_wday 4);
  Alcotest.(check string) "Weekday Fri" "Fri" (Utility.weekday_of_tm_wday 5);
  Alcotest.(check string) "Weekday Sat" "Sat" (Utility.weekday_of_tm_wday 6);
  Alcotest.(check_raises)
    "Weekday out of bound" (Failure "Day number unknown: 7") (fun () ->
      let _ = Utility.weekday_of_tm_wday 7 in
      ())

let month_of_tm_mon () =
  Alcotest.(check string) "Month" "Jan" (Utility.month_of_tm_mon 0);
  Alcotest.(check string) "Month" "Feb" (Utility.month_of_tm_mon 1);
  Alcotest.(check string) "Month" "Mar" (Utility.month_of_tm_mon 2);
  Alcotest.(check string) "Month" "Apr" (Utility.month_of_tm_mon 3);
  Alcotest.(check string) "Month" "May" (Utility.month_of_tm_mon 4);
  Alcotest.(check string) "Month" "Jun" (Utility.month_of_tm_mon 5);
  Alcotest.(check string) "Month" "Jul" (Utility.month_of_tm_mon 6);
  Alcotest.(check string) "Month" "Aug" (Utility.month_of_tm_mon 7);
  Alcotest.(check string) "Month" "Sep" (Utility.month_of_tm_mon 8);
  Alcotest.(check string) "Month" "Oct" (Utility.month_of_tm_mon 9);
  Alcotest.(check string) "Month" "Nov" (Utility.month_of_tm_mon 10);
  Alcotest.(check string) "Month" "Dec" (Utility.month_of_tm_mon 11);
  Alcotest.(check_raises)
    "Month out of bound" (Failure "Month number unknown: 12") (fun () ->
      let _ = Utility.month_of_tm_mon 12 in
      ())

let utility_test =
  let open Alcotest_lwt in
  [
    test_case_sync "x_ms_date" `Quick x_ms_date;
    test_case_sync "adjust_host_no_change" `Quick adjust_host_no_change;
    test_case_sync "adjust_host_change" `Quick adjust_host_change;
    test_case_sync "auth_key_test" `Quick auth_key_test;
    test_case_sync "auth_key_test_get_list" `Quick auth_key_test_get_list;
    test_case_sync "convert_list_databases_test" `Quick
      convert_list_databases_test;
    test_case_sync "string_of_bool_true" `Quick string_of_bool_true;
    test_case_sync "string_of_bool_false" `Quick string_of_bool_false;
    test_case_sync "weekday_of_tm_wday" `Quick weekday_of_tm_wday;
    test_case_sync "month_of_tm_mon" `Quick month_of_tm_mon;
  ]

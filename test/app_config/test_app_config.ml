let call_test _ () =
  (* let expected =
       "{\"items\":[{\"etag\":\"1qgmt87bb0Jr_2LJBBfSwgWXE2ZRu9E4fUiAfzmdS1Y\",\"key\":\"a \
        feature\",\"label\":null,\"content_type\":\"\",\"value\":\"true\",\"tags\":{},\"locked\":false,\"last_modified\":\"2023-11-21T20:19:10+00:00\"}]}"
     in *)
  let expected =
    "{\"items\":[{\"etag\":\"9i8j9h7IhJaUOSj9-uyc_XxtcK5MmUVJmNwhprZzQic\",\"key\":\".appconfig.featureflag/cmr\",\"label\":null,\"content_type\":\"application/vnd.microsoft.appconfig.ff+json;charset=utf-8\",\"value\":\"{\\\"id\\\":\\\"cmr\\\",\\\"description\\\":\\\"\\\",\\\"enabled\\\":true,\\\"conditions\\\":{\\\"client_filters\\\":[]}}\",\"tags\":{},\"locked\":false,\"last_modified\":\"2023-12-19T14:37:51+00:00\"},{\"etag\":\"OE7LkotIe6oelNHIgzSoLbl08nmysGkhcvsZk-myaPU\",\"key\":\".appconfig.featureflag/tbo\",\"label\":null,\"content_type\":\"application/vnd.microsoft.appconfig.ff+json;charset=utf-8\",\"value\":\"{\\\"id\\\":\\\"tbo\\\",\\\"description\\\":\\\"\\\",\\\"enabled\\\":false,\\\"conditions\\\":{\\\"client_filters\\\":[]}}\",\"tags\":{},\"locked\":false,\"last_modified\":\"2023-12-19T14:34:38+00:00\"},{\"etag\":\"1qgmt87bb0Jr_2LJBBfSwgWXE2ZRu9E4fUiAfzmdS1Y\",\"key\":\"a \
     feature\",\"label\":null,\"content_type\":\"\",\"value\":\"true\",\"tags\":{},\"locked\":false,\"last_modified\":\"2023-11-21T20:19:10+00:00\"}]}"
  in
  let%lwt code, body = App_config.call () in
  Alcotest.(check' int) ~msg:"Code is correct" ~expected:200 ~actual:code;
  Alcotest.(check' string) ~msg:"Body is correct" ~expected ~actual:body;
  Lwt.return_unit

let json_test _ () =
  (* let expected = () in *)
  let%lwt code, body = App_config.call_json () in
  let open App_config.Json in
  let items_length = List.length body.items in
  Alcotest.(check' int) ~msg:"Code is correct" ~expected:200 ~actual:code;
  Alcotest.(check' int) ~msg:"Items is correct" ~expected:3 ~actual:items_length;
  Lwt.return_unit

let test =
  let open Alcotest_lwt in
  [
    test_case "make call" `Quick call_test;
    test_case "make json" `Quick json_test;
  ]

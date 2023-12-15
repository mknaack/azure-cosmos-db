let make_call_test _ () =
  let expected =
    "{\"items\":[{\"etag\":\"1qgmt87bb0Jr_2LJBBfSwgWXE2ZRu9E4fUiAfzmdS1Y\",\"key\":\"a \
     feature\",\"label\":null,\"content_type\":\"\",\"value\":\"true\",\"tags\":{},\"locked\":false,\"last_modified\":\"2023-11-21T20:19:10+00:00\"}]}"
  in
  let%lwt code, body = App_config.make_call () in
  Alcotest.(check' int) ~msg:"Code is correct" ~expected:200 ~actual:code;
  Alcotest.(check' string) ~msg:"Body is correct" ~expected ~actual:body;
  Lwt.return_unit

let test =
  let open Alcotest_lwt in
  [ test_case "make call" `Quick make_call_test ]

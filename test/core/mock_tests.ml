open Mock_test_runner

let string_contains str substr =
  try
    let idx = Str.search_forward (Str.regexp_string substr) str 0 in
    idx >= 0
  with Not_found -> false

let test_mock_io_bind () =
  let module IO = Mock_io in
  let ( let* ) = IO.bind in
  let* x = IO.return 5 in
  let* y = IO.return 10 in
  Alcotest.(check int) "Bind chains" 15 (x + y)

let test_mock_io_catch () =
  let module IO = Mock_io in
  let result =
    IO.catch
      (fun () -> IO.return (raise (Failure "test error")))
      (fun exn -> IO.return (Printexc.to_string exn))
  in
  Alcotest.(check string) "Catch handler runs" "Failure(\"test error\")" result

let test_mock_io_timeout_disabled () =
  let module IO = Mock_io in
  let result = IO.with_timeout 0.0 "value" in
  Alcotest.(check (option string))
    "Timeout returns Some when disabled" (Some "value") result

let test_mock_io_timeout_enabled () =
  let module IO = Mock_io in
  Mock_io.with_timeouts_enabled (fun () ->
      let result = IO.with_timeout 0.0 "value" in
      Alcotest.(check (option string))
        "Timeout returns None when enabled and t <= 0" None result)

let test_mock_http_expectation () =
  let http = Mock_http.create () in
  Mock_http.with_mock http (fun () ->
      Mock_http.expect
        {
          method_ = `Get;
          uri = Uri.of_string "https://test.com/path";
          expected_headers = [];
          expected_body = None;
          response = Ok (Mock_response.make_response "OK");
        };
      let result =
        Mock_http.get ~headers:(Cohttp.Header.init ())
          (Uri.of_string "https://test.com/path")
      in
      (match result with
      | Ok (_resp, body) -> Alcotest.(check string) "Body is OK" "OK" body
      | Error _ -> Alcotest.fail "Expected OK result");
      Mock_http.verify ())

let test_mock_http_post () =
  let http = Mock_http.create () in
  Mock_http.with_mock http (fun () ->
      Mock_http.expect
        {
          method_ = `Post;
          uri = Uri.of_string "https://test.com/create";
          expected_headers = [];
          expected_body = Some "{}";
          response =
            Ok (Mock_response.make_response ~status:201 "{\"id\": \"123\"}");
        };
      let result =
        Mock_http.post ~headers:(Cohttp.Header.init ()) ~body:"{}"
          (Uri.of_string "https://test.com/create")
      in
      (match result with
      | Ok (resp, body) ->
          Alcotest.(check int)
            "Status is 201" 201
            (Cohttp.Response.status resp |> Cohttp.Code.code_of_status);
          Alcotest.(check string) "Body is correct" "{\"id\": \"123\"}" body
      | Error _ -> Alcotest.fail "Expected OK result");
      Mock_http.verify ())

let test_mock_http_verify_unconsumed () =
  let http = Mock_http.create () in
  try
    Mock_http.with_mock http (fun () ->
        Mock_http.expect
          {
            method_ = `Get;
            uri = Uri.of_string "https://test.com/1";
            expected_headers = [];
            expected_body = None;
            response = Ok (Mock_response.make_response "OK");
          };
        Mock_http.expect
          {
            method_ = `Get;
            uri = Uri.of_string "https://test.com/2";
            expected_headers = [];
            expected_body = None;
            response = Ok (Mock_response.make_response "OK");
          };
        (* Only consume first expectation *)
        let _ =
          Mock_http.get ~headers:(Cohttp.Header.init ())
            (Uri.of_string "https://test.com/1")
        in
        (* with_mock will call verify() and raise due to unconsumed expectation *)
        ());
    Alcotest.fail "with_mock should have raised due to unconsumed expectations"
  with _exn ->
    (* Expected: with_mock raised due to unconsumed expectations *)
    ()

let test_mock_database_response () =
  let json = Mock_response.database_response ~id:"testdb" ~_rid:"abc123" () in
  Alcotest.(check bool)
    "Contains id" true
    (string_contains json "\"id\": \"testdb\"");
  Alcotest.(check bool)
    "Contains _rid" true
    (string_contains json "\"_rid\": \"abc123\"")

let test_mock_collection_response () =
  let json =
    Mock_response.collection_response ~id:"testcoll" ~_rid:"coll123"
      ~partition_key:"pk" ()
  in
  Alcotest.(check bool)
    "Contains id" true
    (string_contains json "\"id\": \"testcoll\"");
  Alcotest.(check bool)
    "Contains partitionKey" true
    (string_contains json "partitionKey")

let test_mock_document_response () =
  let json =
    Mock_response.document_response ~id:"doc1" ~_rid:"doc123"
      ~json:"{\"name\": \"test\"}" ()
  in
  Alcotest.(check bool)
    "Contains id" true
    (string_contains json "\"id\": \"doc1\"");
  Alcotest.(check bool)
    "Contains name field from json" true
    (string_contains json "\"name\": \"test\"")

let test_mock_list_databases_response () =
  let json =
    Mock_response.list_databases_response [ ("db1", "rid1"); ("db2", "rid2") ]
  in
  Alcotest.(check bool)
    "Contains db1" true
    (string_contains json "\"id\": \"db1\"");
  Alcotest.(check bool)
    "Contains db2" true
    (string_contains json "\"id\": \"db2\"");
  Alcotest.(check bool)
    "Contains _count: 2" true
    (string_contains json "\"_count\": 2")

let test_mock_throttled_response () =
  let resp, body = Mock_response.throttled_response ~retry_after_ms:100 in
  let status = Cohttp.Response.status resp |> Cohttp.Code.code_of_status in
  Alcotest.(check int) "Status is 429" 429 status;
  Alcotest.(check string) "Body is empty" "" body

let test_mock_error_response () =
  let resp, body =
    Mock_response.error_response ~code:404 ~message:"Not found"
  in
  let status = Cohttp.Response.status resp |> Cohttp.Code.code_of_status in
  Alcotest.(check int) "Status is 404" 404 status;
  Alcotest.(check bool)
    "Body contains message" true
    (string_contains body "Not found")

let mock_create_database_test () =
  let http = Mock_http.create () in
  Mock_http.with_mock http (fun () ->
      Mock_http.expect
        {
          method_ = `Post;
          uri =
            Uri.make ~scheme:"https" ~host:"mock-account.documents.azure.com"
              ~port:443 ~path:"dbs" ();
          expected_headers = [];
          expected_body = None;
          response =
            Ok
              (Mock_response.make_response ~status:201
                 (Mock_response.database_response ~id:"testdb" ~_rid:"abc123" ()));
        };
      let result = Mock_db.create "testdb" in
      (match result with
      | Ok (code, _) -> Alcotest.(check int) "Status 201" 201 code
      | Error _ -> Alcotest.fail "Should not return error");
      Mock_http.verify ())

let mock_list_databases_test () =
  let http = Mock_http.create () in
  Mock_http.with_mock http (fun () ->
      Mock_http.expect
        {
          method_ = `Get;
          uri =
            Uri.make ~scheme:"https" ~host:"mock-account.documents.azure.com"
              ~port:443 ~path:"dbs" ();
          expected_headers = [];
          expected_body = None;
          response =
            Ok
              (Mock_response.make_response
                 (Mock_response.list_databases_response [ ("db1", "rid1") ]));
        };
      let result = Mock_db.list_databases () in
      (match result with
      | Ok (code, list_result) ->
          Alcotest.(check int) "Status 200" 200 code;
          Alcotest.(check int)
            "Count is 1" 1 list_result.Cosmos.Json_converter_t._count
      | Error _ -> Alcotest.fail "Should not return error");
      Mock_http.verify ())

let tests =
  [
    ("mock_io_bind", `Quick, test_mock_io_bind);
    ("mock_io_catch", `Quick, test_mock_io_catch);
    ("mock_io_timeout_disabled", `Quick, test_mock_io_timeout_disabled);
    ("mock_io_timeout_enabled", `Quick, test_mock_io_timeout_enabled);
    ("mock_http_expectation", `Quick, test_mock_http_expectation);
    ("mock_http_post", `Quick, test_mock_http_post);
    ("mock_http_verify_unconsumed", `Quick, test_mock_http_verify_unconsumed);
    ("mock_database_response", `Quick, test_mock_database_response);
    ("mock_collection_response", `Quick, test_mock_collection_response);
    ("mock_document_response", `Quick, test_mock_document_response);
    ("mock_list_databases_response", `Quick, test_mock_list_databases_response);
    ("mock_throttled_response", `Quick, test_mock_throttled_response);
    ("mock_error_response", `Quick, test_mock_error_response);
    ("mock_create_database", `Quick, mock_create_database_test);
    ("mock_list_databases", `Quick, mock_list_databases_test);
  ]

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

let compute_expected_auth verb resource_type resource_path date_header =
  Cosmos.Utility.authorization_token_using_master_key verb resource_type
    resource_path date_header Mock_auth.Auth.master_key

let get_recorded_auth_and_date () =
  let req, _ = Mock_http.get_recorded () |> List.hd in
  let get name =
    match Cohttp.Header.get req.Mock_http.headers name with
    | None -> Alcotest.fail ("No " ^ name ^ " header")
    | Some v -> v
  in
  (get "authorization", get "x-ms-date")

let mock_document_list_auth_resource_path_test () =
  let http = Mock_http.create () in
  Mock_http.with_mock http (fun () ->
      Mock_http.expect
        {
          method_ = `Get;
          uri =
            Uri.make ~scheme:"https" ~host:"mock-account.documents.azure.com"
              ~port:443 ~path:"/dbs/mydb/colls/mycoll/docs" ();
          expected_headers = [];
          expected_body = None;
          response =
            Ok
              (Mock_response.make_response
                 (Mock_response.list_documents_response
                    [ ("doc1", "rid1"); ("doc2", "rid2") ]));
        };
      let _ = Mock_db.Collection.Document.list "mydb" "mycoll" in
      let auth, date = get_recorded_auth_and_date () in
      let expected =
        compute_expected_auth "get" "docs" "dbs/mydb/colls/mycoll" date
      in
      Alcotest.(check string)
        "Document.list signs with collection path (not docs path)" expected auth)

let mock_document_query_auth_resource_path_test () =
  let query =
    Cosmos.Json_converter_t.{ query = "SELECT * FROM c"; parameters = [] }
  in
  let http = Mock_http.create () in
  Mock_http.with_mock http (fun () ->
      Mock_http.expect
        {
          method_ = `Post;
          uri =
            Uri.make ~scheme:"https" ~host:"mock-account.documents.azure.com"
              ~port:443 ~path:"/dbs/mydb/colls/mycoll/docs" ();
          expected_headers = [];
          expected_body = None;
          response =
            Ok
              (Mock_response.make_response
                 (Mock_response.list_documents_response [ ("doc1", "rid1") ]));
        };
      let _ =
        Mock_db.Collection.Document.query ~is_partition:true "mydb" "mycoll"
          query
      in
      let auth, date = get_recorded_auth_and_date () in
      let expected =
        compute_expected_auth "post" "docs" "dbs/mydb/colls/mycoll" date
      in
      Alcotest.(check string)
        "Document.query signs with collection path (not docs path)" expected
        auth)

let manual_offer_response =
  Mock_response.offer_response ~offer_throughput:400 ~id:"GpFA" ~_rid:"GpFA"
    ~resource:"dbs/db/colls/coll/" ~offer_resource_id:"CollRid" ()

let autoscale_offer_response =
  Mock_response.offer_response ~max_throughput:4000 ~id:"GpFA" ~_rid:"GpFA"
    ~resource:"dbs/db/colls/coll/" ~offer_resource_id:"CollRid" ()

let mock_offer_get_auth_resource_path_test () =
  let http = Mock_http.create () in
  Mock_http.with_mock http (fun () ->
      Mock_http.expect
        {
          method_ = `Get;
          uri =
            Uri.make ~scheme:"https" ~host:"mock-account.documents.azure.com"
              ~port:443 ~path:"offers/GpFA" ();
          expected_headers = [];
          expected_body = None;
          response = Ok (Mock_response.make_response manual_offer_response);
        };
      let _ = Mock_db.Offer.get "GpFA" in
      let auth, date = get_recorded_auth_and_date () in
      let expected = compute_expected_auth "get" "offers" "gpfa" date in
      Alcotest.(check string)
        "Offer.get signs with lowercased rid" expected auth)

let mock_offer_list_auth_resource_path_test () =
  let http = Mock_http.create () in
  Mock_http.with_mock http (fun () ->
      Mock_http.expect
        {
          method_ = `Get;
          uri =
            Uri.make ~scheme:"https" ~host:"mock-account.documents.azure.com"
              ~port:443 ~path:"offers" ();
          expected_headers = [];
          expected_body = None;
          response =
            Ok
              (Mock_response.make_response
                 (Mock_response.list_offers_response [ manual_offer_response ]));
        };
      let _ = Mock_db.Offer.list () in
      let auth, date = get_recorded_auth_and_date () in
      let expected = compute_expected_auth "get" "offers" "" date in
      Alcotest.(check string) "Offer.list signs with empty path" expected auth)

let mock_offer_query_headers_test () =
  let http = Mock_http.create () in
  Mock_http.with_mock http (fun () ->
      Mock_http.expect
        {
          method_ = `Post;
          uri =
            Uri.make ~scheme:"https" ~host:"mock-account.documents.azure.com"
              ~port:443 ~path:"offers" ();
          expected_headers =
            [
              ("x-ms-documentdb-isquery", "true");
              ("content-type", "application/query+json");
              ("x-ms-max-item-count", "10");
              ("x-ms-continuation", "next");
            ];
          expected_body = None;
          response =
            Ok
              (Mock_response.make_response
                 (Mock_response.list_offers_response [ manual_offer_response ]));
        };
      let query =
        Cosmos.Json_converter_t.
          {
            query = "SELECT * FROM c WHERE c.offerResourceId = @rid";
            parameters = [ { name = "@rid"; value = "CollRid" } ];
          }
      in
      let _ =
        Mock_db.Offer.query ~max_item_count:10 ~continuation:"next" query
      in
      Mock_http.verify ())

let recorded_body () =
  let req, _ = Mock_http.get_recorded () |> List.hd in
  match req.Mock_http.body with
  | Some body -> body
  | None -> Alcotest.fail "Offer request had no body"

let expect_offer_replace ?migrate response =
  Mock_http.expect
    {
      method_ = `Put;
      uri =
        Uri.make ~scheme:"https" ~host:"mock-account.documents.azure.com"
          ~port:443 ~path:"offers/GpFA" ();
      expected_headers =
        (match migrate with
        | None -> []
        | Some value ->
            [ ("x-ms-cosmos-migrate-offer-to-autopilot", value) ]);
      expected_body = None;
      response = Ok (Mock_response.make_response response);
    }

let manual_offer () =
  Cosmos.Json_converter_j.offer_of_string manual_offer_response

let mock_offer_replace_body_round_trip_test () =
  let http = Mock_http.create () in
  Mock_http.with_mock http (fun () ->
      expect_offer_replace manual_offer_response;
      let _ =
        Mock_db.Offer.replace (manual_offer ())
          (Mock_db.Offer.Throughput.Manual 500)
      in
      let json = recorded_body () |> Yojson.Safe.from_string in
      let req, _ = Mock_http.get_recorded () |> List.hd in
      let open Yojson.Safe.Util in
      Alcotest.(check string)
        "rid preserved" "GpFA" (json |> member "_rid" |> to_string);
      Alcotest.(check string)
        "self preserved" "offers/GpFA/" (json |> member "_self" |> to_string);
      Alcotest.(check string)
        "version preserved" "V2" (json |> member "offerVersion" |> to_string);
      Alcotest.(check int)
        "manual throughput" 500
        (json |> member "content" |> member "offerThroughput" |> to_int);
      Alcotest.(check (option string))
        "migration header omitted" None
        (Cohttp.Header.get req.Mock_http.headers
           "x-ms-cosmos-migrate-offer-to-autopilot"))

let mock_offer_replace_autoscale_body_test () =
  let http = Mock_http.create () in
  Mock_http.with_mock http (fun () ->
      expect_offer_replace autoscale_offer_response;
      let _ =
        Mock_db.Offer.replace (manual_offer ())
          (Mock_db.Offer.Throughput.Autoscale { max_throughput = 4000 })
      in
      let content =
        recorded_body () |> Yojson.Safe.from_string
        |> Yojson.Safe.Util.member "content"
      in
      let open Yojson.Safe.Util in
      Alcotest.(check int)
        "autoscale throughput" 4000
        (content |> member "offerAutopilotSettings"
        |> member "maxThroughput" |> to_int);
      Alcotest.(check bool)
        "manual throughput omitted" true
        (content |> member "offerThroughput" = `Null))

let mock_offer_migrate_header_test () =
  let http = Mock_http.create () in
  Mock_http.with_mock http (fun () ->
      expect_offer_replace ~migrate:"true" autoscale_offer_response;
      let _ =
        Mock_db.Offer.replace ~migrate:`To_autoscale (manual_offer ())
          (Mock_db.Offer.Throughput.Autoscale { max_throughput = 4000 })
      in
      let req, _ = Mock_http.get_recorded () |> List.hd in
      Alcotest.(check (option string))
        "migration header set" (Some "true")
        (Cohttp.Header.get req.Mock_http.headers
           "x-ms-cosmos-migrate-offer-to-autopilot");
      Alcotest.(check (option string))
        "manual migration header omitted" None
        (Cohttp.Header.get req.Mock_http.headers
           "x-ms-cosmos-migrate-offer-to-manual-throughput"))

let mock_offer_throughput_of_content_test () =
  let module T = Mock_db.Offer.Throughput in
  let manual =
    Cosmos.Json_converter_t.
      {
        offer_throughput = Some 400;
        offer_is_ru_per_minute_throughput_enabled = None;
        offer_autopilot_settings = None;
      }
  in
  let autoscale =
    Cosmos.Json_converter_t.
      {
        offer_throughput = None;
        offer_is_ru_per_minute_throughput_enabled = None;
        offer_autopilot_settings = Some { max_throughput = 4000 };
      }
  in
  let empty =
    Cosmos.Json_converter_t.
      {
        offer_throughput = None;
        offer_is_ru_per_minute_throughput_enabled = None;
        offer_autopilot_settings = None;
      }
  in
  Alcotest.(check (option string))
    "manual content" (Some "Manual 400")
    (Option.map T.string_of (T.of_content manual));
  Alcotest.(check (option string))
    "autoscale content" (Some "Autoscale 4000")
    (Option.map T.string_of (T.of_content autoscale));
  Alcotest.(check (option string))
    "empty content" None (Option.map T.string_of (T.of_content empty))

let mock_batch_patch_body_valid_json_test () =
  let http = Mock_http.create () in
  Mock_http.with_mock http (fun () ->
      Mock_http.expect
        {
          method_ = `Post;
          uri =
            Uri.make ~scheme:"https" ~host:"mock-account.documents.azure.com"
              ~port:443 ~path:"/dbs/mydb/colls/mycoll/docs" ();
          expected_headers = [];
          expected_body = None;
          response =
            Ok
              (Mock_response.make_response ~status:200
                 {|[{"statusCode":200,"requestCharge":1.0}]|});
        };
      let ops =
        [
          Mock_db.Collection.Batch.Patch
            {
              id = "doc1";
              if_match = None;
              patch_op =
                Mock_db.Collection.Batch.Increment { path = "/age"; value = 5 };
            };
        ]
      in
      let _ =
        Mock_db.Collection.Batch.execute ~partition_key:"pk" "mydb" "mycoll" ops
      in
      let req, _ = Mock_http.get_recorded () |> List.hd in
      let body =
        match req.Mock_http.body with
        | Some b -> b
        | None -> Alcotest.fail "Batch request had no body"
      in
      let json =
        try Yojson.Safe.from_string body
        with _ ->
          Alcotest.fail
            (Printf.sprintf "Batch request body is not valid JSON: %s" body)
      in
      let first =
        match json with
        | `List (x :: _) -> x
        | _ -> Alcotest.fail "Expected a non-empty JSON array of operations"
      in
      match Yojson.Safe.Util.member "value" first with
      | `Assoc _ -> ()
      | `String _ ->
          Alcotest.fail
            "Patch 'value' was encoded as a JSON string instead of a JSON \
             object"
      | _ -> Alcotest.fail "Patch 'value' has an unexpected shape")

let mock_batch_empty_returns_validation_error_test () =
  match
    Mock_db.Collection.Batch.execute ~partition_key:"pk" "mydb" "mycoll" []
  with
  | Error (Cosmos.Databases_core.Batch_validation_error Empty_batch) -> ()
  | Error _ -> Alcotest.fail "Expected Batch_validation_error Empty_batch"
  | Ok _ -> Alcotest.fail "Empty batch should not succeed"

let mock_batch_too_many_returns_validation_error_test () =
  let ops =
    List.init 101 (fun i ->
        Mock_db.Collection.Batch.Create
          {
            if_match = None;
            if_none_match = None;
            body = Printf.sprintf {|{"id": "%d"}|} i;
          })
  in
  match
    Mock_db.Collection.Batch.execute ~partition_key:"pk" "mydb" "mycoll" ops
  with
  | Error
      (Cosmos.Databases_core.Batch_validation_error (Too_many_operations 101))
    ->
      ()
  | Error _ ->
      Alcotest.fail "Expected Batch_validation_error (Too_many_operations 101)"
  | Ok _ -> Alcotest.fail "Oversized batch should not succeed"

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
    ( "mock_document_list_auth_resource_path",
      `Quick,
      mock_document_list_auth_resource_path_test );
    ( "mock_document_query_auth_resource_path",
      `Quick,
      mock_document_query_auth_resource_path_test );
    ( "mock_offer_get_auth_resource_path",
      `Quick,
      mock_offer_get_auth_resource_path_test );
    ( "mock_offer_list_auth_resource_path",
      `Quick,
      mock_offer_list_auth_resource_path_test );
    ("mock_offer_query_headers", `Quick, mock_offer_query_headers_test);
    ( "mock_offer_replace_body_round_trip",
      `Quick,
      mock_offer_replace_body_round_trip_test );
    ( "mock_offer_replace_autoscale_body",
      `Quick,
      mock_offer_replace_autoscale_body_test );
    ("mock_offer_migrate_header", `Quick, mock_offer_migrate_header_test);
    ( "mock_offer_throughput_of_content",
      `Quick,
      mock_offer_throughput_of_content_test );
    ( "mock_batch_patch_body_valid_json",
      `Quick,
      mock_batch_patch_body_valid_json_test );
    ( "mock_batch_empty_returns_validation_error",
      `Quick,
      mock_batch_empty_returns_validation_error_test );
    ( "mock_batch_too_many_returns_validation_error",
      `Quick,
      mock_batch_too_many_returns_validation_error_test );
  ]

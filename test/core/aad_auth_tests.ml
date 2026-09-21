open Mock_test_runner

(* Live integration recipe (env-gated elsewhere):
     az ad sp create-for-rbac --name cosmos-ocaml-sdk --skip-assignment
     az cosmosdb sql role assignment create \
       --account-name <acct> --resource-group <rg> \
       --scope "/" --principal-id <sp-object-id> \
       --role-definition-name "Cosmos DB Built-in Data Contributor" *)

let host = Mock_auth.endpoint

let aad_jwt =
  "eyJhbGciOiJSUzI1NiJ9.eyJhdWQiOiJodHRwczovL2Nvc21vcy5henVyZS5jb20ifQ.c2lnbmF0dXJl"

let rotated_jwt =
  "eyJhbGciOiJSUzI1NiJ9.eyJhdWQiOiJodHRwczovL2Nvc21vcy5henVyZS5jb20ifQ.cm90YXRlZA"

let string_contains str substr =
  match Str.search_forward (Str.regexp_string substr) str 0 with
  | _ -> true
  | exception Not_found -> false

let authorization_exn account verb resource date path =
  match account verb resource date path with
  | Ok header -> header
  | Error _ -> Alcotest.fail "authorization should not fail"

module Aad_token_auth : Cosmos.Databases_intf.Credentials = struct
  let credential = Cosmos.Databases_intf.Credential.Aad_token aad_jwt
  let endpoint = host
end

module Aad_account =
  Cosmos.Databases_core.Auth_credential (Mock_io) (Aad_token_auth)

module Mock_db_aad_token =
  Cosmos.Databases_core.Make_credential (Mock_io) (Mock_http_impl)
    (Aad_token_auth)

let provider_calls = ref 0
let provider_tokens = [| aad_jwt; rotated_jwt |]

module Aad_provider_auth : Cosmos.Databases_intf.Credentials = struct
  let credential =
    Cosmos.Databases_intf.Credential.Aad_token_provider
      (fun () ->
        let index = min !provider_calls (Array.length provider_tokens - 1) in
        incr provider_calls;
        provider_tokens.(index))

  let endpoint = host
end

module Mock_db_aad_provider =
  Cosmos.Databases_core.Make_credential (Mock_io) (Mock_http_impl)
    (Aad_provider_auth)

let now_ref = ref 1000.0

module Aad_config : Cosmos.Databases_intf.Aad_client = struct
  let endpoint = host
  let tenant_id = "test-tenant"
  let client_id = "test-client-id"
  let client_secret = "test-client-secret"
  let scope = "https://cosmos.azure.com/.default"
  let authority_host = "https://login.microsoftonline.com"
  let now () = !now_ref
end

let date = Utilities.Ms_time.create 0.

let document_uri =
  Uri.make ~scheme:"https" ~host ~port:443
    ~path:"/dbs/mydb/colls/mycoll/docs/doc1" ()

let expect_document_get response =
  Mock_http.expect
    {
      method_ = `Get;
      uri = document_uri;
      expected_headers = [];
      expected_body = None;
      response;
    }

let token_uri =
  Uri.make ~scheme:"https" ~host:"login.microsoftonline.com"
    ~path:"/test-tenant/oauth2/v2.0/token" ()

let token_response token expires_in =
  Ok
    (Mock_response.make_response
       (Printf.sprintf
          {|{"access_token": "%s", "token_type": "Bearer", "expires_in": %d}|}
          token expires_in))

let expect_token_request response =
  Mock_http.expect
    {
      method_ = `Post;
      uri = token_uri;
      expected_headers =
        [ ("Content-Type", "application/x-www-form-urlencoded") ];
      expected_body = None;
      response;
    }

let recorded_requests () = Mock_http.get_recorded () |> List.map fst

let header_exn name headers =
  match Cohttp.Header.get headers name with
  | None -> Alcotest.fail ("No " ^ name ^ " header")
  | Some value -> value

let aad_header_shape () =
  let encoded = Cosmos.Utility.authorization_token_using_aad_token aad_jwt in
  Alcotest.(check bool)
    "type, ver and sig are percent encoded" true
    (string_contains encoded "type%3daad%26ver%3d1.0%26sig%3d");
  Alcotest.(check string)
    "Decodes to the raw aad authorization value"
    ("type=aad&ver=1.0&sig=" ^ aad_jwt)
    (Uri.pct_decode encoded)

let aad_header_ignores_verb_and_path () =
  let authorization verb resource path =
    authorization_exn Aad_account.authorization verb resource date path
  in
  let reference =
    authorization Utilities.Verb.Get Aad_account.Docs "dbs/mydb/colls/mycoll"
  in
  Alcotest.(check string)
    "Verb does not change the header" reference
    (authorization Utilities.Verb.Post Aad_account.Docs "dbs/mydb/colls/mycoll");
  Alcotest.(check string)
    "Resource link does not change the header" reference
    (authorization Utilities.Verb.Get Aad_account.Dbs "dbs/otherdb")

let aad_request_keeps_ms_headers () =
  let http = Mock_http.create () in
  Mock_http.with_mock http (fun () ->
      expect_document_get (Ok (Mock_response.make_response "{}"));
      let _ =
        Mock_db_aad_token.Collection.Document.get ~partition_key:"pk" "mydb"
          "mycoll" "doc1"
      in
      let headers = (List.hd (recorded_requests ())).Mock_http.headers in
      Alcotest.(check string)
        "Api version is sent" "2018-12-31"
        (header_exn "x-ms-version" headers);
      Alcotest.(check string)
        "Authorization carries the aad token"
        ("type=aad&ver=1.0&sig=" ^ aad_jwt)
        (Uri.pct_decode (header_exn "authorization" headers));
      ignore (header_exn "x-ms-date" headers))

let aad_token_reaches_the_wire () =
  let http = Mock_http.create () in
  Mock_http.with_mock http (fun () ->
      expect_document_get (Ok (Mock_response.make_response {|{"id": "doc1"}|}));
      match
        Mock_db_aad_token.Collection.Document.get ~partition_key:"pk" "mydb"
          "mycoll" "doc1"
      with
      | Ok (code, body) ->
          Alcotest.(check int) "Status 200" 200 code;
          Alcotest.(check string) "Body is returned" {|{"id": "doc1"}|} body
      | Error _ -> Alcotest.fail "Should not return error")

let aad_token_provider_called_per_request () =
  let http = Mock_http.create () in
  provider_calls := 0;
  Mock_http.with_mock http (fun () ->
      expect_document_get (Ok (Mock_response.make_response "{}"));
      expect_document_get (Ok (Mock_response.make_response "{}"));
      let get_document () =
        Mock_db_aad_provider.Collection.Document.get ~partition_key:"pk" "mydb"
          "mycoll" "doc1"
      in
      let _ = get_document () in
      let _ = get_document () in
      Alcotest.(check int) "Provider called per request" 2 !provider_calls;
      match recorded_requests () with
      | [ first; second ] ->
          Alcotest.(check string)
            "First request uses the first token"
            ("type=aad&ver=1.0&sig=" ^ aad_jwt)
            (Uri.pct_decode
               (header_exn "authorization" first.Mock_http.headers));
          Alcotest.(check string)
            "Second request uses the rotated token"
            ("type=aad&ver=1.0&sig=" ^ rotated_jwt)
            (Uri.pct_decode
               (header_exn "authorization" second.Mock_http.headers))
      | _ -> Alcotest.fail "Expected two recorded requests")

let first_request_acquires_token () =
  now_ref := 1000.;
  let http = Mock_http.create () in
  Mock_http.with_mock http (fun () ->
      let module Db =
        Cosmos.Databases_core.Make_aad (Mock_io) (Mock_http_impl) (Aad_config)
      in
      expect_token_request (token_response aad_jwt 3600);
      expect_document_get (Ok (Mock_response.make_response {|{"id": "doc1"}|}));
      (match
         Db.Collection.Document.get ~partition_key:"pk" "mydb" "mycoll" "doc1"
       with
      | Ok (code, _) -> Alcotest.(check int) "Status 200" 200 code
      | Error _ -> Alcotest.fail "Request should not fail");
      match recorded_requests () with
      | [ token_req; doc_req ] -> (
          match token_req.Mock_http.body with
          | None -> Alcotest.fail "Token request has no body"
          | Some body ->
              Alcotest.(check bool)
                "Client credentials grant" true
                (string_contains body "grant_type=client_credentials");
              Alcotest.(check bool)
                "Client id is sent" true
                (string_contains body "client_id=test-client-id");
              Alcotest.(check string)
                "Data request carries the issued token"
                ("type=aad&ver=1.0&sig=" ^ aad_jwt)
                (Uri.pct_decode
                   (header_exn "authorization" doc_req.Mock_http.headers)))
      | _ -> Alcotest.fail "Expected a token request and a data request")

let token_cached_across_requests () =
  now_ref := 1000.;
  let http = Mock_http.create () in
  Mock_http.with_mock http (fun () ->
      let module Db =
        Cosmos.Databases_core.Make_aad (Mock_io) (Mock_http_impl) (Aad_config)
      in
      expect_token_request (token_response aad_jwt 3600);
      expect_document_get (Ok (Mock_response.make_response {|{"id": "doc1"}|}));
      expect_document_get (Ok (Mock_response.make_response {|{"id": "doc2"}|}));
      (match
         Db.Collection.Document.get ~partition_key:"pk" "mydb" "mycoll" "doc1"
       with
      | Ok (code, _) -> Alcotest.(check int) "Status 200" 200 code
      | Error _ -> Alcotest.fail "First request should not fail");
      match
        Db.Collection.Document.get ~partition_key:"pk" "mydb" "mycoll" "doc1"
      with
      | Ok (code, _) -> Alcotest.(check int) "Status 200" 200 code
      | Error _ -> Alcotest.fail "Second request should not fail")

let token_refreshes_after_expiry () =
  now_ref := 1000.;
  let http = Mock_http.create () in
  Mock_http.with_mock http (fun () ->
      let module Db =
        Cosmos.Databases_core.Make_aad (Mock_io) (Mock_http_impl) (Aad_config)
      in
      expect_token_request (token_response aad_jwt 3600);
      expect_document_get (Ok (Mock_response.make_response "{}"));
      let _ =
        Db.Collection.Document.get ~partition_key:"pk" "mydb" "mycoll" "doc1"
      in
      now_ref := 4301.;
      expect_token_request (token_response rotated_jwt 3600);
      expect_document_get (Ok (Mock_response.make_response "{}"));
      let _ =
        Db.Collection.Document.get ~partition_key:"pk" "mydb" "mycoll" "doc1"
      in
      match recorded_requests () with
      | [ _; first_doc; _; second_doc ] ->
          Alcotest.(check string)
            "First data request uses the first token"
            ("type=aad&ver=1.0&sig=" ^ aad_jwt)
            (Uri.pct_decode
               (header_exn "authorization" first_doc.Mock_http.headers));
          Alcotest.(check string)
            "Second data request uses the refreshed token"
            ("type=aad&ver=1.0&sig=" ^ rotated_jwt)
            (Uri.pct_decode
               (header_exn "authorization" second_doc.Mock_http.headers))
      | _ -> Alcotest.fail "Expected token and data request pairs")

let token_endpoint_http_error () =
  now_ref := 1000.;
  let http = Mock_http.create () in
  Mock_http.with_mock http (fun () ->
      let module Db =
        Cosmos.Databases_core.Make_aad (Mock_io) (Mock_http_impl) (Aad_config)
      in
      expect_token_request
        (Ok
           (Mock_response.make_response ~status:401
              {|{"error":"invalid_client","error_description":"bad secret"}|}));
      (match
         Db.Collection.Document.get ~partition_key:"pk" "mydb" "mycoll" "doc1"
       with
      | Error (Cosmos.Databases_core.Azure_error (code, _)) ->
          Alcotest.(check int) "Token endpoint status code" 401 code
      | Error _ -> Alcotest.fail "Expected token endpoint Azure_error"
      | Ok _ -> Alcotest.fail "Should not succeed");
      Alcotest.(check int)
        "Data request is never issued" 1
        (List.length (recorded_requests ())))

let token_endpoint_malformed_body () =
  now_ref := 1000.;
  let http = Mock_http.create () in
  Mock_http.with_mock http (fun () ->
      let module Db =
        Cosmos.Databases_core.Make_aad (Mock_io) (Mock_http_impl) (Aad_config)
      in
      expect_token_request (Ok (Mock_response.make_response "{}"));
      match
        Db.Collection.Document.get ~partition_key:"pk" "mydb" "mycoll" "doc1"
      with
      | Error (Cosmos.Databases_core.Azure_error (code, _)) ->
          Alcotest.(check int) "Token endpoint status code" 200 code
      | Error _ -> Alcotest.fail "Expected malformed token Azure_error"
      | Ok _ -> Alcotest.fail "Should not succeed")

let token_endpoint_connection_refused () =
  now_ref := 1000.;
  let http = Mock_http.create () in
  Mock_http.with_mock http (fun () ->
      let module Db =
        Cosmos.Databases_core.Make_aad (Mock_io) (Mock_http_impl) (Aad_config)
      in
      expect_token_request (Error Mock_http.Connection_refused);
      match
        Db.Collection.Document.get ~partition_key:"pk" "mydb" "mycoll" "doc1"
      with
      | Error Cosmos.Databases_core.Connection_error -> ()
      | Error _ -> Alcotest.fail "Expected Connection_error"
      | Ok _ -> Alcotest.fail "Should not succeed")

let tests =
  [
    ("aad_header_shape", `Quick, aad_header_shape);
    ( "aad_header_ignores_verb_and_path",
      `Quick,
      aad_header_ignores_verb_and_path );
    ("aad_request_keeps_ms_headers", `Quick, aad_request_keeps_ms_headers);
    ("aad_token_reaches_the_wire", `Quick, aad_token_reaches_the_wire);
    ( "aad_token_provider_called_per_request",
      `Quick,
      aad_token_provider_called_per_request );
    ("first_request_acquires_token", `Quick, first_request_acquires_token);
    ("token_cached_across_requests", `Quick, token_cached_across_requests);
    ("token_refreshes_after_expiry", `Quick, token_refreshes_after_expiry);
    ("token_endpoint_http_error", `Quick, token_endpoint_http_error);
    ("token_endpoint_malformed_body", `Quick, token_endpoint_malformed_body);
    ( "token_endpoint_connection_refused",
      `Quick,
      token_endpoint_connection_refused );
  ]

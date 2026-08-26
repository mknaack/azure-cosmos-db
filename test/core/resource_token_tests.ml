open Mock_test_runner

let token = Mock_auth.sample_resource_token
let host = Mock_auth.endpoint

module Master_account = Cosmos.Databases_core.Auth (Mock_auth.Auth)

module Resource_token_account =
  Cosmos.Databases_core.Auth_credential (Mock_auth.Resource_token_auth)

module Mock_db_as =
  Cosmos.Databases_core.Make_credential (Mock_io) (Mock_http_impl)
    (Mock_auth.Resource_token_auth)

let rotating_tokens =
  [| "type=resource&ver=1&sig=first=="; "type=resource&ver=1&sig=second==" |]

let provider_calls = ref 0

module Rotating_token_auth : Cosmos.Databases_intf.Credentials = struct
  let credential =
    Cosmos.Databases_intf.Credential.Resource_token_provider
      (fun () ->
        let index = min !provider_calls (Array.length rotating_tokens - 1) in
        incr provider_calls;
        rotating_tokens.(index))

  let endpoint = Mock_auth.endpoint
end

module Mock_db_rotating =
  Cosmos.Databases_core.Make_credential (Mock_io) (Mock_http_impl)
    (Rotating_token_auth)

let string_contains str substr =
  match Str.search_forward (Str.regexp_string substr) str 0 with
  | _ -> true
  | exception Not_found -> false

let document_uri =
  Uri.make ~scheme:"https" ~host ~port:443
    ~path:"/dbs/mydb/colls/mycoll/docs/doc1" ()

let get_document_as ?timeout () =
  Mock_db_as.Collection.Document.get ~partition_key:"pk" ?timeout "mydb"
    "mycoll" "doc1"

let documents_uri =
  Uri.make ~scheme:"https" ~host ~port:443 ~path:"/dbs/mydb/colls/mycoll/docs"
    ()

let list_documents_as () = Mock_db_as.Collection.Document.list "mydb" "mycoll"

let expect_document_list response =
  Mock_http.expect
    {
      method_ = `Get;
      uri = documents_uri;
      expected_headers = [];
      expected_body = None;
      response;
    }

let get_document_rotating () =
  Mock_db_rotating.Collection.Document.get ~partition_key:"pk" "mydb" "mycoll"
    "doc1"

let expect_document_get response =
  Mock_http.expect
    {
      method_ = `Get;
      uri = document_uri;
      expected_headers = [];
      expected_body = None;
      response;
    }

let recorded_headers () =
  Mock_http.get_recorded () |> List.map (fun (req, _) -> req.Mock_http.headers)

let header_exn name headers =
  match Cohttp.Header.get headers name with
  | None -> Alcotest.fail ("No " ^ name ^ " header")
  | Some value -> value

let date = Utilities.Ms_time.create 0.

let resource_token_header_is_pct_encoded_token () =
  let expected =
    Uri.pct_encode ~component:`Userinfo token
    |> Str.global_replace (Str.regexp_string "%3D") "%3d"
    |> Str.global_replace (Str.regexp_string "%2B") "%2b"
    |> Str.global_replace (Str.regexp_string "%2F") "%2f"
  in
  Alcotest.(check string)
    "Resource token is sent percent encoded" expected
    (Resource_token_account.authorization Utilities.Verb.Get
       Resource_token_account.Docs date "dbs/mydb/colls/mycoll")

let resource_token_header_has_no_master_type () =
  let actual =
    Resource_token_account.authorization Utilities.Verb.Get
      Resource_token_account.Docs date "dbs/mydb/colls/mycoll"
  in
  Alcotest.(check bool)
    "Token type is resource" true
    (string_contains actual "type%3dresource");
  Alcotest.(check bool)
    "Token type is not master" false
    (string_contains actual "type%3dmaster")

let resource_token_header_ignores_verb_and_path () =
  let authorization verb resource path =
    Resource_token_account.authorization verb resource date path
  in
  let reference =
    authorization Utilities.Verb.Get Resource_token_account.Docs
      "dbs/mydb/colls/mycoll"
  in
  Alcotest.(check string)
    "Verb does not change the header" reference
    (authorization Utilities.Verb.Post Resource_token_account.Docs
       "dbs/mydb/colls/mycoll");
  Alcotest.(check string)
    "Resource link does not change the header" reference
    (authorization Utilities.Verb.Get Resource_token_account.Dbs "dbs/otherdb")

let master_key_header_unchanged () =
  let expected =
    Cosmos.Utility.authorization_token_using_master_key "get" "docs"
      "dbs/mydb/colls/mycoll"
      (Utilities.Ms_time.x_ms_date date)
      Mock_auth.Auth.master_key
  in
  Alcotest.(check string)
    "Master key signing is unchanged" expected
    (Master_account.authorization Utilities.Verb.Get Master_account.Docs date
       "dbs/mydb/colls/mycoll")

let resource_token_request_keeps_ms_headers () =
  let http = Mock_http.create () in
  Mock_http.with_mock http (fun () ->
      expect_document_get (Ok (Mock_response.make_response "{}"));
      let _ = get_document_as () in
      let headers = List.hd (recorded_headers ()) in
      Alcotest.(check string)
        "Api version is sent" "2018-12-31"
        (header_exn "x-ms-version" headers);
      Alcotest.(check string)
        "Authorization carries the resource token" token
        (Uri.pct_decode (header_exn "authorization" headers));
      ignore (header_exn "x-ms-date" headers))

let document_get_with_resource_token () =
  let http = Mock_http.create () in
  Mock_http.with_mock http (fun () ->
      expect_document_get (Ok (Mock_response.make_response {|{"id": "doc1"}|}));
      match get_document_as () with
      | Ok (code, body) ->
          Alcotest.(check int) "Status 200" 200 code;
          Alcotest.(check string) "Body is returned" {|{"id": "doc1"}|} body
      | Error _ -> Alcotest.fail "Should not return error")

let document_list_error code =
  let http = Mock_http.create () in
  Mock_http.with_mock http (fun () ->
      expect_document_list
        (Ok (Mock_response.error_response ~code ~message:"denied"));
      match list_documents_as () with
      | Error (Cosmos.Databases_core.Azure_error (actual, _)) ->
          Alcotest.(check int) "Azure error code" code actual
      | Error _ -> Alcotest.fail "Expected an Azure error"
      | Ok _ -> Alcotest.fail "Should not succeed")

let document_list_forbidden () = document_list_error 403
let document_list_unauthorized () = document_list_error 401

let document_get_timeout () =
  let http = Mock_http.create () in
  Mock_http.with_mock http (fun () ->
      expect_document_get (Ok (Mock_response.make_response "{}"));
      Mock_io.with_timeouts_enabled (fun () ->
          match get_document_as ~timeout:0.0 () with
          | Error Cosmos.Databases_core.Timeout_error -> ()
          | Error _ -> Alcotest.fail "Expected Timeout_error"
          | Ok _ -> Alcotest.fail "Should not succeed"))

let document_get_connection_refused () =
  let http = Mock_http.create () in
  Mock_http.with_mock http (fun () ->
      expect_document_get (Error Mock_http.Connection_refused);
      match get_document_as () with
      | Error Cosmos.Databases_core.Connection_error -> ()
      | Error _ -> Alcotest.fail "Expected Connection_error"
      | Ok _ -> Alcotest.fail "Should not succeed")

let token_provider_called_per_request () =
  let http = Mock_http.create () in
  provider_calls := 0;
  Mock_http.with_mock http (fun () ->
      expect_document_get (Ok (Mock_response.make_response "{}"));
      expect_document_get (Ok (Mock_response.make_response "{}"));
      let _ = get_document_rotating () in
      let _ = get_document_rotating () in
      Alcotest.(check int) "Provider called per request" 2 !provider_calls;
      match recorded_headers () with
      | [ first; second ] ->
          Alcotest.(check string)
            "First request uses the first token" rotating_tokens.(0)
            (Uri.pct_decode (header_exn "authorization" first));
          Alcotest.(check string)
            "Second request uses the rotated token" rotating_tokens.(1)
            (Uri.pct_decode (header_exn "authorization" second))
      | _ -> Alcotest.fail "Expected two recorded requests")

let permission_response =
  {|{"id": "a_permission", "permissionMode": "Read", "resource": "dbs/mydb/colls/mycoll", "_rid": "rid", "_ts": 1, "_self": "self", "_etag": "etag", "_token": "type=resource&ver=1&sig=abc=="}|}

let create_permission ?expiry_seconds () =
  Mock_db.Permission.create ?expiry_seconds ~dbname:"mydb" ~user_name:"a_user"
    ~coll_name:"mycoll" Mock_db.Permission.Read ~permission_name:"a_permission"

let expect_permission_create () =
  Mock_http.expect
    {
      method_ = `Post;
      uri =
        Uri.make ~scheme:"https" ~host ~port:443
          ~path:"/dbs/mydb/users/a_user/permissions" ();
      expected_headers = [];
      expected_body = None;
      response =
        Ok (Mock_response.make_response ~status:201 permission_response);
    }

let permission_create_sends_expiry_header () =
  let http = Mock_http.create () in
  Mock_http.with_mock http (fun () ->
      expect_permission_create ();
      let _ = create_permission ~expiry_seconds:3600 () in
      let headers = List.hd (recorded_headers ()) in
      Alcotest.(check string)
        "Expiry header is sent" "3600"
        (header_exn "x-ms-documentdb-expiry-seconds" headers))

let permission_create_omits_expiry_header () =
  let http = Mock_http.create () in
  Mock_http.with_mock http (fun () ->
      expect_permission_create ();
      let _ = create_permission () in
      let headers = List.hd (recorded_headers ()) in
      Alcotest.(check (option string))
        "Expiry header is absent" None
        (Cohttp.Header.get headers "x-ms-documentdb-expiry-seconds"))

let tests =
  [
    ( "resource_token_header_is_pct_encoded_token",
      `Quick,
      resource_token_header_is_pct_encoded_token );
    ( "resource_token_header_has_no_master_type",
      `Quick,
      resource_token_header_has_no_master_type );
    ( "resource_token_header_ignores_verb_and_path",
      `Quick,
      resource_token_header_ignores_verb_and_path );
    ("master_key_header_unchanged", `Quick, master_key_header_unchanged);
    ( "resource_token_request_keeps_ms_headers",
      `Quick,
      resource_token_request_keeps_ms_headers );
    ( "document_get_with_resource_token",
      `Quick,
      document_get_with_resource_token );
    ("document_list_forbidden", `Quick, document_list_forbidden);
    ("document_list_unauthorized", `Quick, document_list_unauthorized);
    ("document_get_timeout", `Quick, document_get_timeout);
    ("document_get_connection_refused", `Quick, document_get_connection_refused);
    ( "token_provider_called_per_request",
      `Quick,
      token_provider_called_per_request );
    ( "permission_create_sends_expiry_header",
      `Quick,
      permission_create_sends_expiry_header );
    ( "permission_create_omits_expiry_header",
      `Quick,
      permission_create_omits_expiry_header );
  ]

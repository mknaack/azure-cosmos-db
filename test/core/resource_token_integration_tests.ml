open Cosmos.Databases_core

(* The token only exists once the permission has been created, so the
   resource-token backend is built from a provider reading this reference. *)
let resource_token = ref ""
let current_resource_token () = !resource_token

module Make
    (Cfg : Test_io_intf.Config)
    (IO : Test_io_intf.IO)
    (D : Test_io_intf.DB with type 'a io := 'a IO.t)
    (Dt : Test_io_intf.DB with type 'a io := 'a IO.t) =
struct
  let ( let* ) = IO.bind
  let dbname = Cfg.prefix ^ "resource_token_database"
  let coll_name = "a_collection_name"
  let user_name = "a_user_name"
  let permission_name = "a_permission_name"
  let document_id = "a_document"
  let partition_key = "Andersen"

  let document =
    Printf.sprintf {|{"id": "%s", "lastName": "%s"}|} document_id partition_key

  let another_document =
    Printf.sprintf {|{"id": "another_document", "lastName": "%s"}|}
      partition_key

  let fail_error prefix = function
    | Azure_error (code, _) ->
        Alcotest.fail (Printf.sprintf "%s: %d" prefix code)
    | Timeout_error -> Alcotest.fail (prefix ^ ": timeout")
    | Connection_error -> Alcotest.fail (prefix ^ ": connection error")
    | Batch_validation_error _ -> Alcotest.fail (prefix ^ ": batch validation")

  let create_database_test () =
    let* res = D.create dbname in
    match res with
    | Result.Error e -> fail_error "Should create the database" e
    | Result.Ok (code, _) ->
        Alcotest.(check int) "Status same int" 201 code;
        IO.return ()

  let create_collection_test () =
    let partition_key =
      Cosmos.Json_converter_t.
        { paths = [ "/lastName" ]; kind = "Hash"; version = None }
    in
    let* res = D.Collection.create ~partition_key dbname coll_name in
    match res with
    | Result.Error e -> fail_error "Should create the collection" e
    | Result.Ok (code, _) ->
        Alcotest.(check int) "Status same int" 201 code;
        IO.return ()

  let create_document_test () =
    let* res =
      D.Collection.Document.create ~partition_key dbname coll_name document
    in
    match res with
    | Result.Error e -> fail_error "Should create the document" e
    | Result.Ok (code, _) ->
        Alcotest.(check int) "Status same int" 201 code;
        IO.return ()

  let create_user_test () =
    let* res = D.User.create dbname user_name in
    match res with
    | Result.Error e -> fail_error "Should create the user" e
    | Result.Ok (code, _) ->
        Alcotest.(check int) "Status same int" 201 code;
        IO.return ()

  let create_read_permission_test () =
    let* res =
      D.Permission.create ~expiry_seconds:3600 ~dbname ~user_name ~coll_name
        D.Permission.Read ~permission_name
    in
    match res with
    | Result.Error e -> fail_error "Should create the permission" e
    | Result.Ok (code, { token; _ }) ->
        Alcotest.(check int) "Status same int" 201 code;
        Alcotest.(check bool) "Token is returned" true (String.length token > 0);
        resource_token := token;
        IO.return ()

  let get_document_with_resource_token_test () =
    let* res =
      Dt.Collection.Document.get ~partition_key dbname coll_name document_id
    in
    match res with
    | Result.Error e -> fail_error "Should read with the resource token" e
    | Result.Ok (code, body) ->
        Alcotest.(check int) "Status same int" 200 code;
        Alcotest.(check bool)
          "Document is returned" true
          (Str.string_match (Str.regexp (".*" ^ Str.quote document_id)) body 0);
        IO.return ()

  let create_document_with_read_token_test () =
    let* res =
      Dt.Collection.Document.create ~partition_key dbname coll_name
        another_document
    in
    match res with
    | Result.Error (Azure_error (code, _)) ->
        Alcotest.(check int) "Read permission forbids writes" 403 code;
        IO.return ()
    | Result.Error e -> fail_error "Should return an azure error" e
    | Result.Ok _ -> Alcotest.fail "A read permission must not allow writes"

  let list_databases_with_resource_token_test () =
    let* res = Dt.list_databases () in
    match res with
    | Result.Error (Azure_error (code, _)) ->
        Alcotest.(check bool)
          "Account level operations are rejected" true
          (code = 401 || code = 403);
        IO.return ()
    | Result.Error e -> fail_error "Should return an azure error" e
    | Result.Ok _ ->
        Alcotest.fail "A resource token must not list the account databases"

  let replace_permission_test () =
    let* res =
      D.Permission.replace ~expiry_seconds:3600 ~dbname ~user_name ~coll_name
        D.Permission.All ~permission_name
    in
    match res with
    | Result.Error e -> fail_error "Should replace the permission" e
    | Result.Ok (code, { token; _ }) ->
        Alcotest.(check int) "Status same int" 200 code;
        Alcotest.(check bool)
          "A new token is returned" true (token <> !resource_token);
        resource_token := token;
        IO.return ()

  let create_document_with_rotated_token_test () =
    let* res =
      Dt.Collection.Document.create ~partition_key dbname coll_name
        another_document
    in
    match res with
    | Result.Error e -> fail_error "Should write with the rotated token" e
    | Result.Ok (code, _) ->
        Alcotest.(check int) "Status same int" 201 code;
        IO.return ()

  let delete_database_test () =
    let* res = D.delete dbname in
    match res with
    | Result.Error e -> fail_error "Should delete the database" e
    | Result.Ok code ->
        Alcotest.(check int) "Status same int" 204 code;
        IO.return ()

  let tests =
    [
      ("create database", create_database_test);
      ("create collection", create_collection_test);
      ("create document", create_document_test);
      ("create user", create_user_test);
      ("create read permission", create_read_permission_test);
      ("get document with resource token", get_document_with_resource_token_test);
      ("create document with read token", create_document_with_read_token_test);
      ( "list databases with resource token",
        list_databases_with_resource_token_test );
      ("replace permission with all", replace_permission_test);
      ( "create document with rotated token",
        create_document_with_rotated_token_test );
      ("delete database", delete_database_test);
    ]
end

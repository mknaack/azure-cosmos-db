open Cosmos.Databases_core

module Make
    (Cfg : Test_io_intf.Config)
    (IO : Test_io_intf.IO)
    (D : Test_io_intf.DB with type 'a io := 'a IO.t) =
struct
  let ( let* ) = IO.bind
  let dbname = Cfg.prefix ^ "user_database"
  let user_name = "a_user_name"
  let coll_name = "a_collection_name"
  let permission_name = "a_permission_name"

  let create_database_test () =
    let* res = D.create dbname in
    match res with
    | Result.Error _ -> Alcotest.fail "Should not return error"
    | Result.Ok (code, body) ->
        let _ = Alcotest.(check int) "Status same int" 201 code in
        let _ =
          match body with
          | Some { id; _ } ->
              Alcotest.(check string) "Create name is correct" dbname id
          | None -> ()
        in
        IO.return ()

  let create_collection_test () =
    let partition_key =
      Some
        Cosmos.Json_converter_t.
          { paths = [ "/lastName" ]; kind = "Hash"; version = None }
    in
    let* res = D.Collection.create ~partition_key dbname coll_name in
    match res with
    | Result.Error _ -> Alcotest.fail "Should not return error"
    | Result.Ok (code, body) ->
        let _ = Alcotest.(check int) "Status same int" 201 code in
        let _ =
          match body with
          | Some { id; _ } ->
              Alcotest.(check string) "Create name is correct" coll_name id
          | None -> ()
        in
        IO.return ()

  let create_user_test () =
    let* res = D.User.create dbname user_name in
    match res with
    | Result.Error _ -> Alcotest.fail "Should not return error"
    | Result.Ok (code, { id; _ }) ->
        let _ = Alcotest.(check int) "Status same int" 201 code in
        let _ = Alcotest.(check string) "Create name is correct" user_name id in
        IO.return ()

  let create_permission_test () =
    let* res =
      D.Permission.create ~dbname ~coll_name ~user_name D.Permission.Read
        ~permission_name
    in
    match res with
    | Result.Error (Azure_error (code, _)) ->
        Alcotest.fail (Printf.sprintf "Should not return error %d" code)
    | Result.Error _ -> Alcotest.fail "Should not return error"
    | Result.Ok (code, { id; _ }) ->
        let _ = Alcotest.(check int) "Status same int" 201 code in
        let _ =
          Alcotest.(check string) "Create name is correct" permission_name id
        in
        IO.return ()

  let get_permission_test () =
    let* res = D.Permission.get ~dbname ~user_name ~permission_name () in
    match res with
    | Result.Error _ -> Alcotest.fail "Should not return error"
    | Result.Ok (code, { id; token = _; _ }) ->
        let _ = Alcotest.(check int) "Status same int" 200 code in
        let _ =
          Alcotest.(check string) "Create name is correct" permission_name id
        in
        IO.return ()

  let get_permission_timeout_test () =
    let* res =
      D.Permission.get ~timeout:0.0 ~dbname ~user_name ~permission_name ()
    in
    match res with
    | Result.Error Timeout_error ->
        Alcotest.(check unit) "Timeout error" () ();
        IO.return ()
    | Result.Error _ -> Alcotest.fail "Should fail with timeout error"
    | Result.Ok _ -> Alcotest.fail "Should fail with timeout error"

  let list_permission_test () =
    let* res = D.Permission.list ~dbname ~user_name () in
    match res with
    | Result.Error (Azure_error (code, _)) ->
        Alcotest.fail (Printf.sprintf "Should not return error %d" code)
    | Result.Error _ -> Alcotest.fail "Should not return error"
    | Result.Ok (code, { rid = _; permissions; count }) ->
        let _ = Alcotest.(check int) "Status same int" 200 code in
        let _ = Alcotest.(check bool) "Count" true (count > 0) in
        let permissions_length = List.length permissions in
        let _ =
          Alcotest.(check int) "Permissions length" 1 permissions_length
        in
        IO.return ()

  let list_permission_timeout_test () =
    let* res = D.Permission.list ~timeout:0.0 ~dbname ~user_name () in
    match res with
    | Result.Error Timeout_error ->
        Alcotest.(check unit) "Timeout error" () ();
        IO.return ()
    | Result.Error _ -> Alcotest.fail "Should fail with timeout error"
    | Result.Ok _ -> Alcotest.fail "Should fail with timeout error"

  let replace_permission_test () =
    let* res =
      D.Permission.replace ~dbname ~user_name ~coll_name D.Permission.All
        ~permission_name
    in
    match res with
    | Result.Error (Azure_error (code, _)) ->
        Alcotest.fail (Printf.sprintf "Should not return error %d" code)
    | Result.Error _ -> Alcotest.fail "Should not return error"
    | Result.Ok (code, { id; token = _; _ }) ->
        let _ = Alcotest.(check int) "Status same int" 200 code in
        let _ =
          Alcotest.(check string) "Create name is correct" permission_name id
        in
        IO.return ()

  let replace_permission_timeout_test () =
    let* res =
      D.Permission.replace ~timeout:0.0 ~dbname ~user_name ~coll_name
        D.Permission.All ~permission_name
    in
    match res with
    | Result.Error Timeout_error ->
        Alcotest.(check unit) "Timeout error" () ();
        IO.return ()
    | Result.Error _ -> Alcotest.fail "Should fail with timeout error"
    | Result.Ok _ -> Alcotest.fail "Should fail with timeout error"

  let delete_permission_test () =
    let* res = D.Permission.delete ~dbname ~user_name ~permission_name () in
    match res with
    | Result.Error (Azure_error (code, _)) ->
        Alcotest.fail (Printf.sprintf "Should not return error %d" code)
    | Result.Error _ -> Alcotest.fail "Should not return error"
    | Result.Ok code ->
        let _ = Alcotest.(check int) "Status same int" 204 code in
        IO.return ()

  let delete_permission_timeout_test () =
    let* res =
      D.Permission.delete ~timeout:0.0 ~dbname ~user_name ~permission_name ()
    in
    match res with
    | Result.Error Timeout_error ->
        Alcotest.(check unit) "Timeout error" () ();
        IO.return ()
    | Result.Error _ -> Alcotest.fail "Should fail with timeout error"
    | Result.Ok _ -> Alcotest.fail "Should fail with timeout error"

  let delete_database_test () =
    let* res = D.delete dbname in
    match res with
    | Result.Error _ -> Alcotest.fail "Should not return error"
    | Result.Ok code ->
        let _ = Alcotest.(check int) "Status same int" 204 code in
        IO.return ()

  let tests =
    [
      ("create database", create_database_test);
      ("create collection", create_collection_test);
      ("create user", create_user_test);
      ("create permissions", create_permission_test);
      ("list permissions", list_permission_test);
      ("get user", get_permission_test);
      ("get user timeout", get_permission_timeout_test);
      ("list permissions timeout", list_permission_timeout_test);
      ("replace user", replace_permission_test);
      ("replace user timeout", replace_permission_timeout_test);
      ("delete user", delete_permission_test);
      ("delete user timeout", delete_permission_timeout_test);
      ("delete database", delete_database_test);
    ]
end

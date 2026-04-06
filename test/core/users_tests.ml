open Cosmos.Databases_core

module Make
    (IO : Test_io_intf.IO)
    (D : Test_io_intf.DB with type 'a io := 'a IO.t) =
struct
  let ( let* ) = IO.bind
  let dbname = "user_database"
  let user_name = "a_user_name"
  let replace_user_name = "replace_user_name"

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

  let create_user_test () =
    let* res = D.User.create dbname user_name in
    match res with
    | Result.Error _ -> Alcotest.fail "Should not return error"
    | Result.Ok (code, { id; _ }) ->
        let _ = Alcotest.(check int) "Status same int" 201 code in
        let _ = Alcotest.(check string) "Create name is correct" user_name id in
        IO.return ()

  let create_user_timeout_test () =
    let* res = D.User.create ~timeout:0.0 dbname user_name in
    match res with
    | Result.Error Timeout_error ->
        Alcotest.(check unit) "Timeout error" () ();
        IO.return ()
    | Result.Error _ -> Alcotest.fail "Should fail with timeout error"
    | Result.Ok _ -> Alcotest.fail "Should fail with timeout error"

  let list_user_test () =
    let* res = D.User.list dbname in
    match res with
    | Result.Error _ -> Alcotest.fail "Should not return error"
    | Result.Ok (code, { rid = _; users; count }) ->
        let db =
          List.filter
            (fun (x : Cosmos.Json_converter_t.user) -> x.id = user_name)
            users
        in
        let _ = Alcotest.(check int) "Status same int" 200 code in
        let _ = Alcotest.(check bool) "Count" true (count > 0) in
        let _ =
          Alcotest.(check string) "Name of user" user_name (List.hd db).id
        in
        IO.return ()

  let list_user_timeout_test () =
    let* res = D.User.list ~timeout:0.0 dbname in
    match res with
    | Result.Error Timeout_error ->
        Alcotest.(check unit) "Timeout error" () ();
        IO.return ()
    | Result.Error _ -> Alcotest.fail "Should fail with timeout error"
    | Result.Ok _ -> Alcotest.fail "Should fail with timeout error"

  let get_user_test () =
    let* res = D.User.get dbname user_name in
    match res with
    | Result.Error _ -> Alcotest.fail "Should not return error"
    | Result.Ok (code, { id; _ }) ->
        let _ = Alcotest.(check int) "Status same int" 200 code in
        let _ = Alcotest.(check string) "Create name is correct" user_name id in
        IO.return ()

  let get_user_timeout_test () =
    let* res = D.User.get ~timeout:0.0 dbname user_name in
    match res with
    | Result.Error Timeout_error ->
        Alcotest.(check unit) "Timeout error" () ();
        IO.return ()
    | Result.Error _ -> Alcotest.fail "Should fail with timeout error"
    | Result.Ok _ -> Alcotest.fail "Should fail with timeout error"

  let replace_user_test () =
    let* res = D.User.replace dbname user_name replace_user_name in
    match res with
    | Result.Error _ -> Alcotest.fail "Should not return error"
    | Result.Ok (code, { id; _ }) ->
        let _ = Alcotest.(check int) "Status same int" 200 code in
        let _ =
          Alcotest.(check string) "Replace name is correct" replace_user_name id
        in
        IO.return ()

  let replace_user_timeout_test () =
    let* res = D.User.replace ~timeout:0.0 dbname user_name replace_user_name in
    match res with
    | Result.Error Timeout_error ->
        Alcotest.(check unit) "Timeout error" () ();
        IO.return ()
    | Result.Error _ -> Alcotest.fail "Should fail with timeout error"
    | Result.Ok _ -> Alcotest.fail "Should fail with timeout error"

  let delete_user_test () =
    let* res = D.User.delete dbname replace_user_name in
    match res with
    | Result.Error _ -> Alcotest.fail "Should not return error"
    | Result.Ok code ->
        let _ = Alcotest.(check int) "Status same int" 204 code in
        IO.return ()

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
      ("create user", create_user_test);
      ("create user timeout", create_user_timeout_test);
      ("list user", list_user_test);
      ("list user timeout", list_user_timeout_test);
      ("get user", get_user_test);
      ("get user timeout", get_user_timeout_test);
      ("replace user", replace_user_test);
      ("replace user timeout", replace_user_timeout_test);
      ("delete user", delete_user_test);
      ("delete database", delete_database_test);
    ]
end

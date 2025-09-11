open Test_common

let dbname = "user_database"
let user_name = "a_user_name"
let replace_user_name = "replace_user_name"
let coll_name = "a_collection_name"
let permission_name = "a_permission_name"

let create_database_test _ () =
  let%lwt res = D.create dbname in
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
      Lwt.return_unit

let create_collection_test _ () =
  let partition_key =
    Some
      Cosmos.Json_converter_t.
        { paths = [ "/lastName" ]; kind = "Hash"; version = None }
  in
  let%lwt res = D.Collection.create ~partition_key dbname coll_name in
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
      Lwt.return_unit

let create_user_test _ () =
  let%lwt res = D.User.create dbname user_name in
  match res with
  | Result.Error _ -> Alcotest.fail "Should not return error"
  | Result.Ok (code, { id; _ }) ->
      let _ = Alcotest.(check int) "Status same int" 201 code in
      let _ = Alcotest.(check string) "Create name is correct" user_name id in
      Lwt.return_unit

let create_permission_test _ () =
  let%lwt res =
    D.Permission.create ~dbname ~coll_name ~user_name D.Permission.Read
      ~permission_name
  in
  match res with
  | Result.Error (Azure_error (code, _)) ->
      Alcotest.fail @@ Printf.sprintf "Should not return error %d" code
  | Result.Error _ -> Alcotest.fail "Should not return error"
  | Result.Ok (code, { id; _ }) ->
      let _ = Alcotest.(check int) "Status same int" 201 code in
      let _ =
        Alcotest.(check string) "Create name is correct" permission_name id
      in
      Lwt.return_unit

let list_permission_test _ () =
  let%lwt res = D.Permission.list ~dbname ~user_name () in
  match res with
  | Result.Error (Azure_error (code, _)) ->
      Alcotest.fail @@ Printf.sprintf "Should not return error %d" code
  | Result.Error _ -> Alcotest.fail "Should not return error"
  | Result.Ok (code, { rid = _; permissions; count }) ->
      let _ = Alcotest.(check int) "Status same int" 200 code in
      let _ = Alcotest.(check bool) "Count" true (count > 0) in
      let permissions_length = List.length permissions in
      let _ = Alcotest.(check int) "Permissions length" 1 permissions_length in
      Lwt.return_unit

let delete_database_test _ () =
  let%lwt res = D.delete dbname in
  match res with
  | Result.Error _ -> Alcotest.fail "Should not return error"
  | Result.Ok code ->
      let _ = Alcotest.(check int) "Status same int" 204 code in
      Lwt.return_unit

let user_tests =
  [
    Alcotest_lwt.test_case "create database" `Slow create_database_test;
    Alcotest_lwt.test_case "create collection" `Slow create_collection_test;
    Alcotest_lwt.test_case "create user" `Slow create_user_test;
    Alcotest_lwt.test_case "create permissions" `Slow create_permission_test;
    Alcotest_lwt.test_case "list permissions" `Slow list_permission_test;
    (*      Alcotest_lwt.test_case "get user" `Slow get_user_test;
          Alcotest_lwt.test_case "replace user" `Slow replace_user_test;
          Alcotest_lwt.test_case "delete user" `Slow delete_user_test;*)
    Alcotest_lwt.test_case "delete database" `Slow delete_database_test;
  ]

let test = if should_run () then user_tests else []

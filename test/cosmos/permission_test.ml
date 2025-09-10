open Test_common
(* open Cosmos *)

let dbname = "user_database"
let user_name = "a_user_name"
let replace_user_name = "replace_user_name"

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
      Lwt.return ()

let create_user_test _ () =
  let%lwt res = D.User.create dbname user_name in
  match res with
  | Result.Error _ -> Alcotest.fail "Should not return error"
  | Result.Ok (code, { id; _ }) ->
      let _ = Alcotest.(check int) "Status same int" 201 code in
      let _ = Alcotest.(check string) "Create name is correct" user_name id in
      Lwt.return ()

let delete_database_test _ () =
  let%lwt res = D.delete dbname in
  match res with
  | Result.Error _ -> Alcotest.fail "Should not return error"
  | Result.Ok code ->
      let _ = Alcotest.(check int) "Status same int" 204 code in
      Lwt.return ()

let user_tests =
  [
    Alcotest_lwt.test_case "create database" `Slow create_database_test;
    Alcotest_lwt.test_case "create user" `Slow create_user_test;
    (* Alcotest_lwt.test_case "list user" `Slow list_user_test;
          Alcotest_lwt.test_case "get user" `Slow get_user_test;
          Alcotest_lwt.test_case "replace user" `Slow replace_user_test;
          Alcotest_lwt.test_case "delete user" `Slow delete_user_test;*)
    Alcotest_lwt.test_case "delete database" `Slow delete_database_test;
  ]

let test = if should_run () then user_tests else []

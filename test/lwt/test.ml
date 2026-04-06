open Cosmos_lwt
open Databases

module MyAuthKeys : Auth_key = struct
  let getenv s = match Sys.getenv_opt s with None -> "" | Some x -> x
  let master_key = getenv Test_core.Test_common_core.master_key_env
  let endpoint = getenv Test_core.Test_common_core.endpoint_env
end

module D = Database (MyAuthKeys)

(* Lwt IO implementation for the test functor *)
module Lwt_test_io : Test_core.Test_io_intf.IO with type 'a t = 'a Lwt.t =
struct
  type +'a t = 'a Lwt.t

  let return = Lwt.return
  let bind = Lwt.bind
  let catch = Lwt.catch
  let sleep = Lwt_unix.sleep

  let with_timeout t cmd =
    let timeout = Lwt_unix.sleep t |> Lwt.map (fun () -> None) in
    Lwt.pick [ timeout; cmd |> Lwt.map (fun x -> Some x) ]

  let parallel_map f xs = Lwt_list.map_p f xs
  let run = Lwt_main.run
end

(* Instantiate the test functors *)
module Integration = Test_core.Integration_tests.Make (Lwt_test_io) (D)
module Users = Test_core.Users_tests.Make (Lwt_test_io) (D)
module Permissions = Test_core.Permission_tests.Make (Lwt_test_io) (D)

(* Wrap async test functions for Alcotest_lwt *)
let wrap_async_tests speed tests =
  List.map
    (fun (name, test_fn) ->
      Alcotest_lwt.test_case name speed (fun _switch () -> test_fn ()))
    tests

(* Wrap sync test functions for Alcotest_lwt *)
let wrap_sync_tests speed tests =
  List.map
    (fun (name, test_fn) -> Alcotest_lwt.test_case_sync name speed test_fn)
    tests

(* App config tests (Lwt-specific, kept here) *)
let host = "tp-app-conf.azconfig.io"
let credential = "/mQv"
let secret = "lKsGlGSd9aiY3iz1v9Q3o9BRhTchHCVtAmdOhapJtpA="

let call_test _ () =
  let expected =
    "{\"items\":[{\"etag\":\"9i8j9h7IhJaUOSj9-uyc_XxtcK5MmUVJmNwhprZzQic\",\"key\":\".appconfig.featureflag/cmr\",\"label\":null,\"content_type\":\"application/vnd.microsoft.appconfig.ff+json;charset=utf-8\",\"value\":\"{\\\"id\\\":\\\"cmr\\\",\\\"description\\\":\\\"\\\",\\\"enabled\\\":true,\\\"conditions\\\":{\\\"client_filters\\\":[]}}\",\"tags\":{},\"locked\":false,\"last_modified\":\"2023-12-19T14:37:51+00:00\"},{\"etag\":\"OE7LkotIe6oelNHIgzSoLbl08nmysGkhcvsZk-myaPU\",\"key\":\".appconfig.featureflag/tbo\",\"label\":null,\"content_type\":\"application/vnd.microsoft.appconfig.ff+json;charset=utf-8\",\"value\":\"{\\\"id\\\":\\\"tbo\\\",\\\"description\\\":\\\"\\\",\\\"enabled\\\":false,\\\"conditions\\\":{\\\"client_filters\\\":[]}}\",\"tags\":{},\"locked\":false,\"last_modified\":\"2023-12-19T14:34:38+00:00\"},{\"etag\":\"1qgmt87bb0Jr_2LJBBfSwgWXE2ZRu9E4fUiAfzmdS1Y\",\"key\":\"a \
     feature\",\"label\":null,\"content_type\":\"\",\"value\":\"true\",\"tags\":{},\"locked\":false,\"last_modified\":\"2023-11-21T20:19:10+00:00\"}]}"
  in
  let%lwt code, body = App_config.call host credential secret () in
  Alcotest.(check' int) ~msg:"Code is correct" ~expected:200 ~actual:code;
  Alcotest.(check' string) ~msg:"Body is correct" ~expected ~actual:body;
  Lwt.return_unit

let json_test _ () =
  let%lwt code, body = App_config.call_json host credential secret () in
  let open App_config.Json in
  let items_length = List.length body.items in
  Alcotest.(check' int) ~msg:"Code is correct" ~expected:200 ~actual:code;
  Alcotest.(check' int) ~msg:"Items is correct" ~expected:3 ~actual:items_length;
  Lwt.return_unit

let app_config_tests =
  let open Alcotest_lwt in
  if Test_core.Test_common_core.should_run () then
    [
      test_case "make call" `Quick call_test;
      test_case "make json" `Quick json_test;
    ]
  else []

let integration_tests =
  if Test_core.Test_common_core.should_run () then
    wrap_async_tests `Slow Integration.tests
  else []

let user_tests =
  if Test_core.Test_common_core.should_run () then
    wrap_async_tests `Slow Users.tests
  else []

let permission_tests =
  if Test_core.Test_common_core.should_run () then
    wrap_async_tests `Slow Permissions.tests
  else []

let () =
  Lwt_main.run
  @@ Alcotest_lwt.run "Main tests"
       [
         ("app config test", app_config_tests);
         ( "utility cosmos test",
           wrap_sync_tests `Quick Test_core.Test_cosmos_utility.tests );
         ("partition key test", integration_tests);
         ("user test", user_tests);
         ("permission test", permission_tests);
         ("utility test", wrap_sync_tests `Quick Test_core.Test_utilities.tests);
       ]

open Cosmos_eio
open Databases

module MyAuthKeys : Auth_key = struct
  let getenv s = match Sys.getenv_opt s with None -> "" | Some x -> x
  let master_key = getenv Test_core.Test_common_core.master_key_env
  let endpoint = getenv Test_core.Test_common_core.endpoint_env
end

module D = Database (MyAuthKeys)

module D_token =
  Database_as
    ((val credentials_of_token_provider ~endpoint:MyAuthKeys.endpoint
            Test_core.Resource_token_integration_tests.current_resource_token))

module Eio_config : Test_core.Test_io_intf.Config = struct
  let prefix = "eio"
end

let clock_ref = ref None

let get_clock () =
  match !clock_ref with
  | Some clock -> clock
  | None -> failwith "Eio test clock is not set"

(* Eio IO implementation for the test functor *)
module Eio_test_io : Test_core.Test_io_intf.IO with type 'a t = unit -> 'a =
struct
  type +'a t = unit -> 'a

  let return x () = x
  let bind x f () = f (x ()) ()
  let catch f handler () = try (f ()) () with exn -> (handler exn) ()
  let sleep secs () = Eio.Time.sleep (get_clock ()) secs

  let with_timeout t cmd () =
    match Eio.Time.with_timeout (get_clock ()) t (fun () -> Ok (cmd ())) with
    | Ok x -> Some x
    | Error `Timeout -> None

  let parallel_map f xs () =
    Eio.Fiber.List.map ~max_fibers:10 (fun x -> (f x) ()) xs

  let run thunk = thunk ()
end

(* Instantiate the test functors *)
module Integration =
  Test_core.Integration_tests.Make (Eio_config) (Eio_test_io) (D)

module Users = Test_core.Users_tests.Make (Eio_config) (Eio_test_io) (D)

module Permissions =
  Test_core.Permission_tests.Make (Eio_config) (Eio_test_io) (D)

module Resource_tokens =
  Test_core.Resource_token_integration_tests.Make (Eio_config) (Eio_test_io) (D)
    (D_token)

module Batch = Test_core.Batch_tests.Make (Eio_config) (Eio_test_io) (D)
module Offers = Test_core.Offer_tests.Make (Eio_config) (Eio_test_io) (D)

module Change_feed =
  Test_core.Change_feed_tests.Make (Eio_config) (Eio_test_io) (D)

(* Wrap async test functions for plain Alcotest *)
let wrap_async_tests speed tests =
  List.map
    (fun (name, test_fn) ->
      Alcotest.test_case name speed (fun () -> (test_fn ()) ()))
    tests

(* Wrap sync test functions for plain Alcotest *)
let wrap_sync_tests speed tests =
  List.map (fun (name, test_fn) -> Alcotest.test_case name speed test_fn) tests

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

let batch_tests =
  if Test_core.Test_common_core.should_run () then
    wrap_async_tests `Slow Batch.tests
  else []

let offer_tests =
  if Test_core.Test_common_core.should_run () then
    wrap_async_tests `Slow Offers.tests
  else []

let change_feed_tests =
  if Test_core.Test_common_core.should_run () then
    wrap_async_tests `Slow Change_feed.tests
  else []

let live_wiring_tests =
  wrap_sync_tests `Quick
    [
      ( "change feed live tests registered",
        Test_core.Test_common_core.live_wiring_test ~suite:"change feed"
          ~registered:(List.length change_feed_tests) );
    ]

let resource_token_integration_tests =
  if Test_core.Test_common_core.should_run () then
    wrap_async_tests `Slow Resource_tokens.tests
  else []

let mock_tests =
  List.map
    (fun (name, _speed, test_fn) -> (name, test_fn))
    Test_core.Mock_tests.tests

let resource_token_tests =
  List.map
    (fun (name, _speed, test_fn) -> (name, test_fn))
    Test_core.Resource_token_tests.tests

let () =
  Eio_main.run @@ fun env ->
  clock_ref := Some (Eio.Stdenv.clock env);
  Eio.Switch.run @@ fun sw ->
  with_env ~sw env (fun () ->
      Alcotest.run "Main tests (Eio)"
        [
          ("mock tests", wrap_sync_tests `Quick mock_tests);
          ("resource token tests", wrap_sync_tests `Quick resource_token_tests);
          ( "utility cosmos test",
            wrap_sync_tests `Quick Test_core.Test_cosmos_utility.tests );
          ("partition key test", integration_tests);
          ("user test", user_tests);
          ("permission test", permission_tests);
          ("resource token test", resource_token_integration_tests);
          ("batch test", batch_tests);
          ("offer test", offer_tests);
          ("change feed test (live)", change_feed_tests);
          ("live wiring", live_wiring_tests);
          ("utility test", wrap_sync_tests `Quick Test_core.Test_utilities.tests);
        ])

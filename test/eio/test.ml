open Cosmos_eio
open Databases

module MyAuthKeys : Auth_key = struct
  let getenv s = match Sys.getenv_opt s with None -> "" | Some x -> x
  let master_key = getenv Test_core.Test_common_core.master_key_env
  let endpoint = getenv Test_core.Test_common_core.endpoint_env
end

module D = Database (MyAuthKeys)

module Eio_config : Test_core.Test_io_intf.Config = struct
  let prefix = "eio"
end

(* Eio IO implementation for the test functor *)
module Eio_test_io : Test_core.Test_io_intf.IO with type 'a t = unit -> 'a =
struct
  type +'a t = unit -> 'a

  let return x () = x
  let bind x f () = f (x ()) ()
  let catch f handler () = try (f ()) () with exn -> (handler exn) ()

  let sleep secs () =
    (* Delegates to the stored clock closure set by with_env *)
    ignore secs

  let with_timeout t cmd () =
    ignore t;
    Some (cmd ())

  let parallel_map f xs () = List.map (fun x -> (f x) ()) xs
  let run thunk = thunk ()
end

(* Instantiate the test functors *)
module Integration =
  Test_core.Integration_tests.Make (Eio_config) (Eio_test_io) (D)

module Users = Test_core.Users_tests.Make (Eio_config) (Eio_test_io) (D)

module Permissions =
  Test_core.Permission_tests.Make (Eio_config) (Eio_test_io) (D)

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

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  with_env ~sw env (fun () ->
      Alcotest.run "Main tests (Eio)"
        [
          ( "utility cosmos test",
            wrap_sync_tests `Quick Test_core.Test_cosmos_utility.tests );
          ("partition key test", integration_tests);
          ("user test", user_tests);
          ("permission test", permission_tests);
          ("utility test", wrap_sync_tests `Quick Test_core.Test_utilities.tests);
        ])

(* Eio backend for azure-cosmos-db.

   Design: We use [type 'a t = unit -> 'a] (lazy thunks) rather than
   the more obvious [type 'a t = 'a] (direct values). This is necessary
   because the core library's [wrap_timeout] does:

     Http.get ... |> wrap_timeout timeout

   With [type 'a t = 'a], the HTTP call would block and complete before
   [with_timeout] ever sees it. With thunks, the HTTP call is deferred
   and [with_timeout] can run it inside a fiber with Eio's cancellation. *)

(* -- Environment storage ------------------------------------------------- *)

(* The IO and Http_client module types have no provision for passing an Eio
   environment (clock, network). We store closures that capture the clock
   in refs, along with the HTTP client. These are set by [with_env] and
   accessed lazily inside thunks. *)

let _sleep_fn : (float -> unit) option ref = ref None

(* Record with universally quantified field to store a polymorphic
   timeout function in a ref without hitting the value restriction. *)
type with_timeout_fn = { f : 'a. float -> (unit -> 'a) -> 'a option }

let _with_timeout_fn : with_timeout_fn option ref = ref None
let _client : Cohttp_eio.Client.t option ref = ref None

let env_error () =
  failwith
    "Cosmos_eio: call Databases.with_env before using Database operations"

let get_sleep () = match !_sleep_fn with Some f -> f | None -> env_error ()

let get_with_timeout () =
  match !_with_timeout_fn with Some wt -> wt.f | None -> env_error ()

let get_client () = match !_client with Some c -> c | None -> env_error ()

let with_env ~sw:_ env f =
  Mirage_crypto_rng_unix.use_default ();
  let net = Eio.Stdenv.net env in
  let clock = Eio.Stdenv.clock env in
  let authenticator = Result.get_ok (Ca_certs.authenticator ()) in
  let tls_config = Result.get_ok (Tls.Config.client ~authenticator ()) in
  let https _uri socket =
    (Tls_eio.client_of_flow tls_config socket :> _ Eio.Flow.two_way)
  in
  let client = Cohttp_eio.Client.make ~https:(Some https) net in
  let old_sleep = !_sleep_fn in
  let old_wt = !_with_timeout_fn in
  let old_client = !_client in
  _sleep_fn := Some (fun secs -> Eio.Time.sleep clock secs);
  _with_timeout_fn :=
    Some
      {
        f =
          (fun t thunk ->
            match Eio.Time.with_timeout clock t (fun () -> Ok (thunk ())) with
            | Ok x -> Some x
            | Error `Timeout -> None);
      };
  _client := Some client;
  Fun.protect
    ~finally:(fun () ->
      _sleep_fn := old_sleep;
      _with_timeout_fn := old_wt;
      _client := old_client)
    f

(* -- IO module ----------------------------------------------------------- *)

module Eio_io : Cosmos.Databases_intf.IO with type 'a t = unit -> 'a = struct
  type +'a t = unit -> 'a

  let return x () = x
  let bind x f () = f (x ()) ()
  let catch f handler () = try (f ()) () with exn -> (handler exn) ()
  let sleep secs () = (get_sleep ()) secs
  let with_timeout t cmd () = (get_with_timeout ()) t (fun () -> cmd ())

  let parallel_map f xs () =
    Eio.Fiber.List.map ~max_fibers:10 (fun x -> (f x) ()) xs
end

(* -- HTTP client module -------------------------------------------------- *)

module Eio_http :
  Cosmos.Databases_intf.Http_client with type 'a io := 'a Eio_io.t = struct
  type http_error = Connection_refused | Other_error of exn

  let perform_request f () =
    Eio.Switch.run @@ fun sw ->
    try Ok (f sw) with
    | Unix.Unix_error (Unix.ECONNREFUSED, _, _) -> Error Connection_refused
    | exn -> Error (Other_error exn)

  let get ~headers uri =
    perform_request (fun sw ->
        let resp, body =
          Cohttp_eio.Client.get (get_client ()) ~sw ~headers uri
        in
        let body_string = Eio.Flow.read_all body in
        (resp, body_string))

  let post ~headers ~body uri =
    perform_request (fun sw ->
        let body = Cohttp_eio.Body.of_string body in
        let resp, resp_body =
          Cohttp_eio.Client.post (get_client ()) ~sw ~body ~headers uri
        in
        let body_string = Eio.Flow.read_all resp_body in
        (resp, body_string))

  let put ~headers ~body uri =
    perform_request (fun sw ->
        let body = Cohttp_eio.Body.of_string body in
        let resp, resp_body =
          Cohttp_eio.Client.put (get_client ()) ~sw ~body ~headers uri
        in
        let body_string = Eio.Flow.read_all resp_body in
        (resp, body_string))

  let delete ~headers uri =
    perform_request (fun sw ->
        let resp, body =
          Cohttp_eio.Client.delete (get_client ()) ~sw ~headers uri
        in
        let body_string = Eio.Flow.read_all body in
        (resp, body_string))
end

(* -- Public interface ---------------------------------------------------- *)

type 'a io = 'a Eio_io.t

module type Auth_key = Cosmos.Databases_intf.Auth_key

module Response_headers = Cosmos.Databases_core.Response_headers

type cosmos_error = Cosmos.Databases_core.cosmos_error =
  | Timeout_error
  | Connection_error
  | Azure_error of int * Response_headers.t

module Database (Auth : Auth_key) =
  Cosmos.Databases_core.Make (Eio_io) (Eio_http) (Auth)

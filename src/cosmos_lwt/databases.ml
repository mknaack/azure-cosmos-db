open Lwt

module Lwt_io : Cosmos.Databases_intf.IO with type 'a t = 'a Lwt.t = struct
  type +'a t = 'a Lwt.t

  let return = Lwt.return
  let bind = Lwt.bind
  let catch = Lwt.catch
  let sleep = Lwt_unix.sleep

  let with_timeout t cmd =
    let timeout = Lwt_unix.sleep t >|= fun () -> None in
    Lwt.pick [ timeout; (cmd >|= fun x -> Some x) ]

  let max_parallel_requests = 10

  let parallel_map f xs =
    let pool =
      Lwt_pool.create max_parallel_requests (fun () -> Lwt.return_unit)
    in
    Lwt_list.map_p (fun x -> Lwt_pool.use pool (fun () -> f x)) xs
end

module Lwt_http :
  Cosmos.Databases_intf.Http_client with type 'a io := 'a Lwt.t = struct
  type http_error = Connection_refused | Other_error of exn

  module Connection = Cohttp_lwt.Connection.Make (Cohttp_lwt_unix.Net)

  module Sleep = struct
    let sleep_ns ns = Lwt_unix.sleep (Int64.to_float ns /. 1_000_000_000.)
  end

  module Connection_cache =
    Cohttp_lwt.Connection_cache.Make (Connection) (Sleep)

  let cache =
    lazy
      (Connection_cache.create ~keep:60_000_000_000L ~retry:2 ~parallel:16
         ~depth:100 ())

  let call ?headers ?body meth uri =
    Connection_cache.call (Lazy.force cache) ?headers ?body meth uri

  let perform_request f =
    Lwt.catch
      (fun () ->
        let%lwt resp, body = f () in
        let%lwt body_string = Cohttp_lwt.Body.to_string body in
        let%lwt () = Cohttp_lwt.Body.drain_body body in
        Lwt.return (Ok (resp, body_string)))
      (function
        | Unix.Unix_error (Unix.ECONNREFUSED, _, _) ->
            Lwt.return (Error Connection_refused)
        | exn -> Lwt.return (Error (Other_error exn)))

  let get ~headers uri = perform_request (fun () -> call ~headers `GET uri)

  let post ~headers ~body uri =
    let body = Cohttp_lwt.Body.of_string body in
    perform_request (fun () -> call ~headers ~body `POST uri)

  let put ~headers ~body uri =
    let body = Cohttp_lwt.Body.of_string body in
    perform_request (fun () -> call ~headers ~body `PUT uri)

  let delete ~headers uri =
    perform_request (fun () -> call ~headers `DELETE uri)
end

module type Auth_key = Cosmos.Databases_intf.Auth_key

module Response_headers = Cosmos.Databases_core.Response_headers

type batch_validation_error = Cosmos.Databases_core.batch_validation_error =
  | Too_many_operations of int
  | Mixed_patch_operations
  | Empty_batch

type cosmos_error = Cosmos.Databases_core.cosmos_error =
  | Timeout_error
  | Connection_error
  | Azure_error of int * Response_headers.t
  | Batch_validation_error of batch_validation_error

let body_to_string body = Cohttp_lwt.Body.to_string body

module Database (Auth : Auth_key) =
  Cosmos.Databases_core.Make (Lwt_io) (Lwt_http) (Auth)

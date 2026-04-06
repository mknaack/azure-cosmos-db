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

  let parallel_map f xs = Lwt_list.map_p f xs
end

module Lwt_http :
  Cosmos.Databases_intf.Http_client with type 'a io := 'a Lwt.t = struct
  type http_error = Connection_refused | Other_error of exn

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

  let get ~headers uri =
    perform_request (fun () -> Cohttp_lwt_unix.Client.get ~headers uri)

  let post ~headers ~body uri =
    let body = Cohttp_lwt.Body.of_string body in
    perform_request (fun () -> Cohttp_lwt_unix.Client.post ~headers ~body uri)

  let put ~headers ~body uri =
    let body = Cohttp_lwt.Body.of_string body in
    perform_request (fun () -> Cohttp_lwt_unix.Client.put ~headers ~body uri)

  let delete ~headers uri =
    perform_request (fun () -> Cohttp_lwt_unix.Client.delete ~headers uri)
end

module type Auth_key = Cosmos.Databases_intf.Auth_key

module Response_headers = Cosmos.Databases_core.Response_headers

type cosmos_error = Cosmos.Databases_core.cosmos_error =
  | Timeout_error
  | Connection_error
  | Azure_error of int * Response_headers.t

let body_to_string body = Cohttp_lwt.Body.to_string body

module Database (Auth : Auth_key) =
  Cosmos.Databases_core.Make (Lwt_io) (Lwt_http) (Auth)

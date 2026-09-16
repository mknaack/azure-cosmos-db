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
module type Credentials = Cosmos.Databases_intf.Credentials
module type S = Database_intf.S

module Credential = Cosmos.Databases_intf.Credential
module Response_headers = Cosmos.Databases_core.Response_headers

type batch_validation_error = Cosmos.Databases_core.batch_validation_error =
  | Too_many_operations of int
  | Mixed_patch_operations
  | Empty_batch

type cosmos_error = Cosmos.Databases_core.cosmos_error =
  | Timeout_error
  | Connection_error
  | Azure_error of int * Response_headers.t

let body_to_string body = Cohttp_lwt.Body.to_string body

module Database (Auth : Auth_key) =
  Cosmos.Databases_core.Make (Lwt_io) (Lwt_http) (Auth)

module Database_as (C : Credentials) =
  Cosmos.Databases_core.Make_credential (Lwt_io) (Lwt_http) (C)

let credentials_of_token ~endpoint token =
  (module struct
    let credential = Credential.Resource_token token
    let endpoint = endpoint
  end : Credentials)

let credentials_of_token_provider ~endpoint provider =
  (module struct
    let credential = Credential.Resource_token_provider provider
    let endpoint = endpoint
  end : Credentials)

let credentials_of_aad_token ~endpoint token =
  (module struct
    let credential = Credential.Aad_token token
    let endpoint = endpoint
  end : Credentials)

let credentials_of_aad_token_provider ~endpoint provider =
  (module struct
    let credential = Credential.Aad_token_provider provider
    let endpoint = endpoint
  end : Credentials)

module type Aad = sig
  val endpoint : string
  val tenant_id : string
  val client_id : string
  val client_secret : string
end

module type Aad_client = Cosmos.Databases_intf.Aad_client

module Database_aad (A : Aad) = struct
  module Aad_config = struct
    let endpoint = A.endpoint
    let tenant_id = A.tenant_id
    let client_id = A.client_id
    let client_secret = A.client_secret
    let scope = "https://cosmos.azure.com/.default"
    let authority_host = "https://login.microsoftonline.com"
    let now = Unix.gettimeofday
  end

  include Cosmos.Databases_core.Make_aad (Lwt_io) (Lwt_http) (Aad_config)
end

let aad_client ?(scope = "https://cosmos.azure.com/.default")
    ?(authority_host = "https://login.microsoftonline.com")
    ?(now = Unix.gettimeofday) ~endpoint ~tenant_id ~client_id ~client_secret ()
    =
  (module struct
    let endpoint = endpoint
    let tenant_id = tenant_id
    let client_id = client_id
    let client_secret = client_secret
    let scope = scope
    let authority_host = authority_host
    let now = now
  end : Aad_client)

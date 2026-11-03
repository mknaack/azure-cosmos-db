module Mock_config : Test_io_intf.Config = struct
  let prefix = "mock"
end

module Mock_test_io : Test_io_intf.IO with type 'a t = 'a = struct
  include Mock_io

  let run x = x
end

module Mock_http_impl :
  Cosmos.Databases_intf.Http_client with type 'a io := 'a Mock_io.t = struct
  type http_error = Mock_http.http_error =
    | Connection_refused
    | Other_error of exn

  let get = Mock_http.get
  let post = Mock_http.post
  let put = Mock_http.put
  let delete = Mock_http.delete
end

module Mock_db =
  Cosmos.Databases_core.Make (Mock_io) (Mock_http_impl) (Mock_auth.Auth)

(* Thunk-based IO that lets tests observe where [with_timeout] is applied.
   Unlike [Mock_io] (identity monad, where the command is already evaluated
   before [with_timeout] sees it), deferred thunks let [with_timeout] mark
   the dynamic extent in which the request runs. *)
module Tracking_io = struct
  type 'a t = unit -> 'a

  let return x () = x
  let bind m f () = f (m ()) ()
  let catch f handler () = try f () () with exn -> handler exn ()
  let sleep _secs () = ()
  let timeout_depth = ref 0

  let with_timeout _t cmd () =
    incr timeout_depth;
    Fun.protect
      ~finally:(fun () -> decr timeout_depth)
      (fun () -> Some (cmd ()))

  let parallel_map f xs () = List.map (fun x -> f x ()) xs
end

module Tracking_http = struct
  type http_error = Mock_http.http_error =
    | Connection_refused
    | Other_error of exn

  (* (method name, inside_timeout) pairs, most recent first *)
  let requests : (string * bool) list ref = ref []

  let record method_ =
    requests := (method_, !Tracking_io.timeout_depth > 0) :: !requests

  let get ~headers uri () =
    record "GET";
    Mock_http.get ~headers uri

  let post ~headers ~body uri () =
    record "POST";
    Mock_http.post ~headers ~body uri

  let put ~headers ~body uri () =
    record "PUT";
    Mock_http.put ~headers ~body uri

  let delete ~headers uri () =
    record "DELETE";
    Mock_http.delete ~headers uri
end

module Tracking_db =
  Cosmos.Databases_core.Make (Tracking_io) (Tracking_http) (Mock_auth.Auth)

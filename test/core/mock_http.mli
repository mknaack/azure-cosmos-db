type 'a io = 'a Mock_io.t
type http_error = Connection_refused | Other_error of exn

type request = {
  method_ : [ `Get | `Post | `Put | `Delete ];
  uri : Uri.t;
  headers : Cohttp.Header.t;
  body : string option;
}

type expectation = {
  method_ : [ `Get | `Post | `Put | `Delete ];
  uri : Uri.t;
  expected_headers : (string * string) list;
  expected_body : string option;
  response : (Cohttp.Response.t * string, http_error) result;
}

type state

val create : unit -> state
val install : state -> unit
val uninstall : unit -> unit
val expect : expectation -> unit
val verify : unit -> unit

val get_recorded :
  unit -> (request * (Cohttp.Response.t * string, http_error) result) list

val get :
  headers:Cohttp.Header.t ->
  Uri.t ->
  (Cohttp.Response.t * string, http_error) result io

val post :
  headers:Cohttp.Header.t ->
  body:string ->
  Uri.t ->
  (Cohttp.Response.t * string, http_error) result io

val put :
  headers:Cohttp.Header.t ->
  body:string ->
  Uri.t ->
  (Cohttp.Response.t * string, http_error) result io

val delete :
  headers:Cohttp.Header.t ->
  Uri.t ->
  (Cohttp.Response.t * string, http_error) result io

val with_mock : state -> (unit -> 'a) -> 'a

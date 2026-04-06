module type Auth_key = sig
  val master_key : string
  val endpoint : string
end

module type IO = sig
  type +'a t

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val catch : (unit -> 'a t) -> (exn -> 'a t) -> 'a t
  val sleep : float -> unit t
  val with_timeout : float -> 'a t -> 'a option t
  val parallel_map : ('a -> 'b t) -> 'a list -> 'b list t
end

module type Http_client = sig
  type +'a io
  type http_error = Connection_refused | Other_error of exn

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
end

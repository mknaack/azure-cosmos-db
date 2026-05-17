val make_response :
  ?status:int ->
  ?headers:Cohttp.Header.t ->
  string ->
  Cohttp.Response.t * string

val database_response : id:string -> _rid:string -> unit -> string

val collection_response :
  id:string -> _rid:string -> partition_key:string -> unit -> string

val document_response :
  id:string -> _rid:string -> json:string -> unit -> string

val list_databases_response : (string * string) list -> string
val list_collections_response : (string * string * string) list -> string
val list_documents_response : (string * string) list -> string
val error_response : code:int -> message:string -> Cohttp.Response.t * string
val throttled_response : retry_after_ms:int -> Cohttp.Response.t * string
val empty_body_response : status:int -> Cohttp.Response.t * string

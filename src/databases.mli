module type Auth_key = sig
  val master_key : string
  val endpoint : string
end

module Auth (Keys : Auth_key) : sig
  type verb = Get | Post | Put | Delete
  type resource = Dbs | Colls | Docs
end

val convert_list_databases : string ->
  Json_converter_t.list_databases

module Database (Auth_key : Auth_key) : sig

  val get_code : Cohttp.Response.t -> int
  val list_databases : unit -> (int * Json_converter_t.list_databases) Lwt.t
  val create :
    string -> (int * Json_converter_t.create_database_result option) Lwt.t
  val get : string -> (int * Json_converter_t.database option) Lwt.t
  val delete : string -> int Lwt.t
  module Collection :
  sig
    val list : string -> (int * Json_converter_t.list_collections) Lwt.t
    val create :
      string ->
      string ->
      (int * Json_converter_t.create_collection_result option) Lwt.t
    val get :
      string -> string -> (int * Json_converter_t.collection option) Lwt.t
    val delete : string -> string -> int Lwt.t
    module Document :
    sig
      type indexing_directive = Include | Exclude
      val create :
        ?is_upsert:bool ->
        ?indexing_directive:indexing_directive ->
        string ->
        string ->
        string ->
        (int * Json_converter_t.create_collection_result option) Lwt.t
      type list_result_meta_data = {
        rid: string;
        self: string;
        etag: string;
        ts: int;
        attachments: string;
      }
      type list_result = {
          rid: string;
          documents: (string * list_result_meta_data) list;
          count: int;
        }
      val list :
        ?max_item_count:int ->
        ?continuation:string ->
        ?consistency_level:string ->
        ?session_token:string ->
        ?a_im:bool ->
        ?if_none_match:string ->
        ?partition_key_range_id:string ->
        string -> string -> (int * list_result option) Lwt.t
      type consistency_level = Strong | Bounded | Session | Eventual
      val string_of_consistency_level : consistency_level -> string
      val get :
        ?if_none_match:string ->
        ?partition_key:string ->
        ?consistency_level:consistency_level ->
        ?session_token:string ->
        string -> string -> string -> (int * string) Lwt.t
      val replace :
        ?indexing_directive:indexing_directive ->
        ?partition_key:string ->
        ?if_match:string ->
        string ->
        string -> string -> string -> (int * Cohttp_lwt.Body.t) Lwt.t
      val delete : string -> string -> string -> int Lwt.t
      val query :
        ?max_item_count:int ->
        ?continuation:string ->
        ?consistency_level:string ->
        ?session_token:string ->
        ?is_partition:bool ->
        string ->
        string -> Json_converter_t.query -> (int * list_result option) Lwt.t
    end
  end
end

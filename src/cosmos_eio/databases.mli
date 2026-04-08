module type Auth_key = Cosmos.Databases_intf.Auth_key

type 'a io = unit -> 'a
(** A deferred IO computation. Call with [()] to execute. This is the Eio
    equivalent of ['a Lwt.t]; computations are deferred as thunks so that
    [with_timeout] can cancel them via Eio's fiber cancellation. *)

val with_env :
  sw:Eio.Switch.t ->
  < clock : _ Eio.Time.clock ; net : _ Eio.Net.t ; .. > ->
  (unit -> 'a) ->
  'a
(** [with_env ~sw env f] sets up the Eio environment (clock, network, TLS
    client) required by {!Database} operations. All database calls must happen
    inside [f]. Call this once inside [Eio_main.run] and [Eio.Switch.run]. *)

module Response_headers : sig
  type t = Cosmos.Databases_core.Response_headers.t

  val content_type : t -> string option
  val date : t -> string option
  val etag : t -> string option
  val x_ms_activity_id : t -> string option
  val x_ms_alt_content_path : t -> string option
  val x_ms_continuation : t -> string option
  val x_ms_item_count : t -> string option
  val x_ms_request_charge : t -> string option
  val x_ms_resource_quota : t -> string option
  val x_ms_resource_usage : t -> string option
  val x_ms_retry_after_ms : t -> string option
  val x_ms_schemaversion : t -> string option
  val x_ms_serviceversion : t -> string option
  val x_ms_session_token : t -> string option
end

type cosmos_error = Cosmos.Databases_core.cosmos_error =
  | Timeout_error
  | Connection_error
  | Azure_error of int * Response_headers.t

module Database (Auth_key : Auth_key) : sig
  val get_code : Cohttp.Response.t -> int

  val list_databases :
    ?timeout:float ->
    unit ->
    (int * Cosmos.Json_converter_t.list_databases, cosmos_error) result io

  val create :
    ?timeout:float ->
    string ->
    (int * Cosmos.Json_converter_t.database option, cosmos_error) result io

  val create_if_not_exists :
    ?timeout:float ->
    string ->
    (int * Cosmos.Json_converter_t.database option, cosmos_error) result io

  val get :
    ?timeout:float ->
    string ->
    (int * Cosmos.Json_converter_t.database option, cosmos_error) result io

  val delete : ?timeout:float -> string -> (int, cosmos_error) result io

  module Collection : sig
    val list :
      ?timeout:float ->
      string ->
      (int * Cosmos.Json_converter_t.list_collections, cosmos_error) result io

    val create :
      ?indexing_policy:Cosmos.Json_converter_t.indexing_policy option ->
      partition_key:Cosmos.Json_converter_t.create_partition_key ->
      ?timeout:float ->
      string ->
      string ->
      (int * Cosmos.Json_converter_t.collection option, cosmos_error) result io

    val create_if_not_exists :
      ?indexing_policy:Cosmos.Json_converter_t.indexing_policy option ->
      partition_key:Cosmos.Json_converter_t.create_partition_key ->
      ?timeout:float ->
      string ->
      string ->
      (int * Cosmos.Json_converter_t.collection option, cosmos_error) result io

    val get :
      ?timeout:float ->
      string ->
      string ->
      (int * Cosmos.Json_converter_t.collection option, cosmos_error) result io

    val delete :
      ?timeout:float -> string -> string -> (int, cosmos_error) result io

    module Document : sig
      type indexing_directive = Include | Exclude

      val create :
        ?is_upsert:bool ->
        ?indexing_directive:indexing_directive ->
        partition_key:string ->
        ?timeout:float ->
        string ->
        string ->
        string ->
        (int * Cosmos.Json_converter_t.collection option, cosmos_error) result
        io

      val create_multiple :
        ?is_upsert:bool ->
        ?indexing_directive:indexing_directive ->
        ?timeout:float ->
        ?chunk_size:int ->
        string ->
        string ->
        (string * string) list ->
        (int * Cosmos.Json_converter_t.collection option, cosmos_error) result
        list
        io

      type list_result_meta_data = {
        rid : string;
        self : string;
        etag : string;
        ts : int;
        attachments : string;
      }

      type list_result = {
        rid : string;
        documents : (string * list_result_meta_data option) list;
        count : int;
      }

      val list :
        ?max_item_count:int ->
        ?continuation:string ->
        ?consistency_level:string ->
        ?session_token:string ->
        ?a_im:bool ->
        ?if_none_match:string ->
        ?partition_key_range_id:string ->
        ?timeout:float ->
        string ->
        string ->
        (int * Response_headers.t * list_result, cosmos_error) result io

      type consistency_level = Strong | Bounded | Session | Eventual

      val string_of_consistency_level : consistency_level -> string

      val get :
        ?if_none_match:string ->
        partition_key:string ->
        ?consistency_level:consistency_level ->
        ?session_token:string ->
        ?timeout:float ->
        string ->
        string ->
        string ->
        (int * string, cosmos_error) result io

      val replace :
        ?indexing_directive:indexing_directive ->
        partition_key:string ->
        ?if_match:string ->
        ?timeout:float ->
        string ->
        string ->
        string ->
        string ->
        (int * string, cosmos_error) result io

      val delete :
        partition_key:string ->
        ?timeout:float ->
        string ->
        string ->
        string ->
        (int, cosmos_error) result io

      val delete_multiple :
        partition_key:string ->
        ?timeout:float ->
        ?chunk_size:int ->
        string ->
        string ->
        string list ->
        (int, cosmos_error) result list io

      val query :
        ?max_item_count:int ->
        ?continuation:string ->
        ?consistency_level:string ->
        ?session_token:string ->
        ?is_partition:bool ->
        ?partition_key:string ->
        ?timeout:float ->
        string ->
        string ->
        Cosmos.Json_converter_t.query ->
        (int * Response_headers.t * list_result, cosmos_error) result io
    end
  end

  module User : sig
    val create :
      ?timeout:float ->
      string ->
      string ->
      (int * Cosmos.Json_converter_t.user, cosmos_error) result io

    val list :
      ?timeout:float ->
      string ->
      (int * Cosmos.Json_converter_t.list_users, cosmos_error) result io

    val get :
      ?timeout:float ->
      string ->
      string ->
      (int * Cosmos.Json_converter_t.user, cosmos_error) result io

    val replace :
      ?timeout:float ->
      string ->
      string ->
      string ->
      (int * Cosmos.Json_converter_t.user, cosmos_error) result io

    val delete :
      ?timeout:float -> string -> string -> (int, cosmos_error) result io
  end

  module Permission : sig
    type permission_mode = Read | All

    val create :
      ?timeout:float ->
      dbname:string ->
      user_name:string ->
      coll_name:string ->
      permission_mode ->
      permission_name:string ->
      (int * Cosmos.Json_converter_t.permission, cosmos_error) result io

    val list :
      ?timeout:float ->
      dbname:string ->
      user_name:string ->
      unit ->
      (int * Cosmos.Json_converter_t.list_permissions, cosmos_error) result io

    val get :
      ?timeout:float ->
      dbname:string ->
      user_name:string ->
      permission_name:string ->
      unit ->
      (int * Cosmos.Json_converter_t.permission, cosmos_error) result io

    val replace :
      ?timeout:float ->
      dbname:string ->
      user_name:string ->
      coll_name:string ->
      permission_mode ->
      permission_name:string ->
      (int * Cosmos.Json_converter_t.permission, cosmos_error) result io

    val delete :
      ?timeout:float ->
      dbname:string ->
      user_name:string ->
      permission_name:string ->
      unit ->
      (int, cosmos_error) result io
  end
end

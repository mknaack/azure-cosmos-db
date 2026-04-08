module type Config = sig
  val prefix : string
end

module type IO = sig
  include Cosmos.Databases_intf.IO

  val run : 'a t -> 'a
end

module type DB = sig
  type 'a io

  val get_code : Cohttp.Response.t -> int

  val list_databases :
    ?timeout:float ->
    unit ->
    ( int * Cosmos.Json_converter_t.list_databases,
      Cosmos.Databases_core.cosmos_error )
    result
    io

  val create :
    ?timeout:float ->
    string ->
    ( int * Cosmos.Json_converter_t.database option,
      Cosmos.Databases_core.cosmos_error )
    result
    io

  val create_if_not_exists :
    ?timeout:float ->
    string ->
    ( int * Cosmos.Json_converter_t.database option,
      Cosmos.Databases_core.cosmos_error )
    result
    io

  val get :
    ?timeout:float ->
    string ->
    ( int * Cosmos.Json_converter_t.database option,
      Cosmos.Databases_core.cosmos_error )
    result
    io

  val delete :
    ?timeout:float ->
    string ->
    (int, Cosmos.Databases_core.cosmos_error) result io

  module Collection : sig
    val list :
      ?timeout:float ->
      string ->
      ( int * Cosmos.Json_converter_t.list_collections,
        Cosmos.Databases_core.cosmos_error )
      result
      io

    val create :
      ?indexing_policy:Cosmos.Json_converter_t.indexing_policy option ->
      partition_key:Cosmos.Json_converter_t.create_partition_key ->
      ?timeout:float ->
      string ->
      string ->
      ( int * Cosmos.Json_converter_t.collection option,
        Cosmos.Databases_core.cosmos_error )
      result
      io

    val create_if_not_exists :
      ?indexing_policy:Cosmos.Json_converter_t.indexing_policy option ->
      partition_key:Cosmos.Json_converter_t.create_partition_key ->
      ?timeout:float ->
      string ->
      string ->
      ( int * Cosmos.Json_converter_t.collection option,
        Cosmos.Databases_core.cosmos_error )
      result
      io

    val get :
      ?timeout:float ->
      string ->
      string ->
      ( int * Cosmos.Json_converter_t.collection option,
        Cosmos.Databases_core.cosmos_error )
      result
      io

    val delete :
      ?timeout:float ->
      string ->
      string ->
      (int, Cosmos.Databases_core.cosmos_error) result io

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
        ( int * Cosmos.Json_converter_t.collection option,
          Cosmos.Databases_core.cosmos_error )
        result
        io

      val create_multiple :
        ?is_upsert:bool ->
        ?indexing_directive:indexing_directive ->
        ?timeout:float ->
        ?chunk_size:int ->
        string ->
        string ->
        (string * string) list ->
        ( int * Cosmos.Json_converter_t.collection option,
          Cosmos.Databases_core.cosmos_error )
        result
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
        ( int * Cosmos.Databases_core.Response_headers.t * list_result,
          Cosmos.Databases_core.cosmos_error )
        result
        io

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
        (int * string, Cosmos.Databases_core.cosmos_error) result io

      val replace :
        ?indexing_directive:indexing_directive ->
        partition_key:string ->
        ?if_match:string ->
        ?timeout:float ->
        string ->
        string ->
        string ->
        string ->
        (int * string, Cosmos.Databases_core.cosmos_error) result io

      val delete :
        partition_key:string ->
        ?timeout:float ->
        string ->
        string ->
        string ->
        (int, Cosmos.Databases_core.cosmos_error) result io

      val delete_multiple :
        partition_key:string ->
        ?timeout:float ->
        ?chunk_size:int ->
        string ->
        string ->
        string list ->
        (int, Cosmos.Databases_core.cosmos_error) result list io

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
        ( int * Cosmos.Databases_core.Response_headers.t * list_result,
          Cosmos.Databases_core.cosmos_error )
        result
        io
    end
  end

  module User : sig
    val create :
      ?timeout:float ->
      string ->
      string ->
      ( int * Cosmos.Json_converter_t.user,
        Cosmos.Databases_core.cosmos_error )
      result
      io

    val list :
      ?timeout:float ->
      string ->
      ( int * Cosmos.Json_converter_t.list_users,
        Cosmos.Databases_core.cosmos_error )
      result
      io

    val get :
      ?timeout:float ->
      string ->
      string ->
      ( int * Cosmos.Json_converter_t.user,
        Cosmos.Databases_core.cosmos_error )
      result
      io

    val replace :
      ?timeout:float ->
      string ->
      string ->
      string ->
      ( int * Cosmos.Json_converter_t.user,
        Cosmos.Databases_core.cosmos_error )
      result
      io

    val delete :
      ?timeout:float ->
      string ->
      string ->
      (int, Cosmos.Databases_core.cosmos_error) result io
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
      ( int * Cosmos.Json_converter_t.permission,
        Cosmos.Databases_core.cosmos_error )
      result
      io

    val list :
      ?timeout:float ->
      dbname:string ->
      user_name:string ->
      unit ->
      ( int * Cosmos.Json_converter_t.list_permissions,
        Cosmos.Databases_core.cosmos_error )
      result
      io

    val get :
      ?timeout:float ->
      dbname:string ->
      user_name:string ->
      permission_name:string ->
      unit ->
      ( int * Cosmos.Json_converter_t.permission,
        Cosmos.Databases_core.cosmos_error )
      result
      io

    val replace :
      ?timeout:float ->
      dbname:string ->
      user_name:string ->
      coll_name:string ->
      permission_mode ->
      permission_name:string ->
      ( int * Cosmos.Json_converter_t.permission,
        Cosmos.Databases_core.cosmos_error )
      result
      io

    val delete :
      ?timeout:float ->
      dbname:string ->
      user_name:string ->
      permission_name:string ->
      unit ->
      (int, Cosmos.Databases_core.cosmos_error) result io
  end
end

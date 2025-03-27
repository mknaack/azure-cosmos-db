module type Auth_key = sig
  val master_key : string
  (** Key value of cosmos container *)

  val endpoint : string
  (** Name of the endpoint of cosmos container *)
end

module Response_headers : sig
  type t

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

type cosmos_error =
  | Timeout_error
  | Connection_error
  | Azure_error of int * Response_headers.t

module Database (Auth_key : Auth_key) : sig
  val get_code : Cohttp.Response.t -> int

  val list_databases :
    ?timeout:float ->
    unit ->
    (int * Json_converter_t.list_databases, cosmos_error) result Lwt.t
  (** [list_databases] returns a list of databases *)

  val create :
    ?timeout:float ->
    string ->
    (int * Json_converter_t.database option, cosmos_error) result Lwt.t
  (** [create database_name] creates a database in Cosmos with name
      database_name. *)

  val create_if_not_exists :
    ?timeout:float ->
    string ->
    (int * Json_converter_t.database option, cosmos_error) result Lwt.t
  (** [create_if_not_exists database_name] creates a database in Cosmos with
      name database_name if it not already exists. *)

  val get :
    ?timeout:float ->
    string ->
    (int * Json_converter_t.database option, cosmos_error) result Lwt.t
  (** [get database_name] returns info about the database *)

  val delete : ?timeout:float -> string -> (int, cosmos_error) result Lwt.t
  (** [delete database_name] deletes the database [database_name] from Cosmos *)

  module Collection : sig
    val list :
      ?timeout:float ->
      string ->
      (int * Json_converter_t.list_collections, cosmos_error) result Lwt.t

    val create :
      ?indexing_policy:Json_converter_t.indexing_policy option ->
      ?partition_key:Json_converter_t.create_partition_key option ->
      ?timeout:float ->
      string ->
      string ->
      (int * Json_converter_t.collection option, cosmos_error) result Lwt.t

    val create_if_not_exists :
      ?indexing_policy:Json_converter_t.indexing_policy option ->
      ?partition_key:Json_converter_t.create_partition_key option ->
      ?timeout:float ->
      string ->
      string ->
      (int * Json_converter_t.collection option, cosmos_error) result Lwt.t

    val get :
      ?timeout:float ->
      string ->
      string ->
      (int * Json_converter_t.collection option, cosmos_error) result Lwt.t

    val delete :
      ?timeout:float -> string -> string -> (int, cosmos_error) result Lwt.t

    module Document : sig
      type indexing_directive = Include | Exclude

      val create :
        ?is_upsert:bool ->
        ?indexing_directive:indexing_directive ->
        ?partition_key:string ->
        ?timeout:float ->
        string ->
        string ->
        string ->
        (int * Json_converter_t.collection option, cosmos_error) result Lwt.t

      val create_multiple :
        ?is_upsert:bool ->
        ?indexing_directive:indexing_directive ->
        ?timeout:float ->
        ?chunk_size:int ->
        string ->
        string ->
        (string option * string) list ->
        (int * Json_converter_t.collection option, cosmos_error) result list
        Lwt.t

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
        (int * Response_headers.t * list_result, cosmos_error) result Lwt.t

      type consistency_level = Strong | Bounded | Session | Eventual

      val string_of_consistency_level : consistency_level -> string

      val get :
        ?if_none_match:string ->
        ?partition_key:string ->
        ?consistency_level:consistency_level ->
        ?session_token:string ->
        ?timeout:float ->
        string ->
        string ->
        string ->
        (int * string, cosmos_error) result Lwt.t

      val replace :
        ?indexing_directive:indexing_directive ->
        ?partition_key:string ->
        ?if_match:string ->
        ?timeout:float ->
        string ->
        string ->
        string ->
        string ->
        (int * Cohttp_lwt.Body.t, cosmos_error) result Lwt.t

      val delete :
        ?partition_key:string ->
        ?timeout:float ->
        string ->
        string ->
        string ->
        (int, cosmos_error) result Lwt.t

      val delete_multiple :
        ?partition_key:string ->
        ?timeout:float ->
        ?chunk_size:int ->
        string ->
        string ->
        string list ->
        (int, cosmos_error) result list Lwt.t

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
        Json_converter_t.query ->
        (int * Response_headers.t * list_result, cosmos_error) result Lwt.t
    end
  end

  module CollectionLabels : sig
    val list :
      ?timeout:float ->
      dbname:string ->
      unit ->
      (int * Json_converter_t.list_collections, cosmos_error) result Lwt.t
    (** [list database_name] returns a list of collections in the database *)

    val create :
      ?indexing_policy:Json_converter_t.indexing_policy option ->
      ?partition_key:Json_converter_t.create_partition_key option ->
      ?timeout:float ->
      dbname:string ->
      coll_name:string ->
      unit ->
      (int * Json_converter_t.collection option, cosmos_error) result Lwt.t
    (** [create database_name collection_name] creates a collection in the
        database [database_name] with name [collection_name] *)

    val create_if_not_exists :
      ?indexing_policy:Json_converter_t.indexing_policy option ->
      ?partition_key:Json_converter_t.create_partition_key option ->
      ?timeout:float ->
      dbname:string ->
      coll_name:string ->
      unit ->
      (int * Json_converter_t.collection option, cosmos_error) result Lwt.t
    (** [create_if_not_exists database_name collection_name] creates a
        collection in the database [database_name] with name [collection_name]
        if it not already exists. *)

    val get :
      ?timeout:float ->
      dbname:string ->
      coll_name:string ->
      unit ->
      (int * Json_converter_t.collection option, cosmos_error) result Lwt.t
    (** [get database_name collection_name] returns info about the collection *)

    val delete :
      ?timeout:float ->
      dbname:string ->
      coll_name:string ->
      unit ->
      (int, cosmos_error) result Lwt.t
    (** [delete database_name collection_name] deletes the collection
        [collection_name] from the database [database_name] *)

    module DocumentLabels : sig
      type indexing_directive = Include | Exclude

      val create :
        ?is_upsert:bool ->
        ?indexing_directive:indexing_directive ->
        ?partition_key:string ->
        ?timeout:float ->
        dbname:string ->
        coll_name:string ->
        string ->
        (int * Json_converter_t.collection option, cosmos_error) result Lwt.t
      (** [create database_name collection_name document] creates a document in
          the collection [collection_name] in the database [database_name] with
          content [document] *)

      val create_multiple :
        ?is_upsert:bool ->
        ?indexing_directive:indexing_directive ->
        ?partition_key:string ->
        ?timeout:float ->
        ?chunk_size:int ->
        dbname:string ->
        coll_name:string ->
        string list ->
        (int * Json_converter_t.collection option, cosmos_error) result list
        Lwt.t
      (** [create_multiple database_name collection_name documents] creates a
          list of documents in the collection [collection_name] in the database
          [database_name] with content [documents] *)

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
        dbname:string ->
        coll_name:string ->
        unit ->
        (int * Response_headers.t * list_result, cosmos_error) result Lwt.t
      (** [list database_name collection_name] returns a list of documents in
          the collection [collection_name] in the database [database_name] *)

      type consistency_level = Strong | Bounded | Session | Eventual

      val string_of_consistency_level : consistency_level -> string

      val get :
        ?if_none_match:string ->
        ?partition_key:string ->
        ?consistency_level:consistency_level ->
        ?session_token:string ->
        ?timeout:float ->
        dbname:string ->
        coll_name:string ->
        string ->
        (int * string, cosmos_error) result Lwt.t
      (** [get database_name collection_name document] returns the document
          [document] in the collection [collection_name] in the database
          [database_name] *)

      val replace :
        ?indexing_directive:indexing_directive ->
        ?partition_key:string ->
        ?if_match:string ->
        ?timeout:float ->
        dbname:string ->
        coll_name:string ->
        string ->
        string ->
        (int * Cohttp_lwt.Body.t, cosmos_error) result Lwt.t
      (** [replace database_name collection_name document] replaces the document
          [document] in the collection [collection_name] in the database
          [database_name] with content [document] *)

      val delete :
        ?partition_key:string ->
        ?timeout:float ->
        dbname:string ->
        coll_name:string ->
        string ->
        (int, cosmos_error) result Lwt.t
      (** [delete database_name collection_name document] deletes the document
          [document] in the collection [collection_name] in the database
          [database_name] *)

      val delete_multiple :
        ?partition_key:string ->
        ?timeout:float ->
        ?chunk_size:int ->
        dbname:string ->
        coll_name:string ->
        string list ->
        (int, cosmos_error) result list Lwt.t
      (** [delete_multiple database_name collection_name documents] deletes the
          list of documents [documents] in the collection [collection_name] in
          the database [database_name] *)

      val query :
        ?max_item_count:int ->
        ?continuation:string ->
        ?consistency_level:string ->
        ?session_token:string ->
        ?is_partition:bool ->
        ?partition_key:string ->
        ?timeout:float ->
        dbname:string ->
        coll_name:string ->
        Json_converter_t.query ->
        (int * Response_headers.t * list_result, cosmos_error) result Lwt.t
      (** [query database_name collection_name query] queries the collection
          [collection_name] in the database [database_name] with query [query]
      *)
    end
  end

  module User : sig
    val create :
      ?timeout:float ->
      dbname:string ->
      user_name:string ->
      unit ->
      (int * Json_converter_t.user, cosmos_error) result Lwt.t
    (** [create database_name user_name] creates a user in the database
        [database_name] with name [user_name] *)

    val list :
      ?timeout:float ->
      dbname:string ->
      unit ->
      (int * Json_converter_t.list_users, cosmos_error) result Lwt.t
    (** [list database_name] returns a list of users in the database
        [database_name] *)

    val get :
      ?timeout:float ->
      dbname:string ->
      user_name:string ->
      unit ->
      (int * Json_converter_t.user, cosmos_error) result Lwt.t
    (** [get database_name user_name] returns the user [user_name] in the
        database [database_name] *)

    val replace :
      ?timeout:float ->
      dbname:string ->
      user_name:string ->
      new_user_name:string ->
      unit ->
      (int * Json_converter_t.user, cosmos_error) result Lwt.t
    (** [replace database_name user_name new_user_name] will replace the user
        name from [user_name] to [new_user_name] *)

    val delete :
      ?timeout:float ->
      dbname:string ->
      user_name:string ->
      unit ->
      (int, cosmos_error) result Lwt.t
    (** [delete database_name user_name] deletes the user [user_name] in the
        database [database_name] *)
  end
end

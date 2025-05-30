module type Auth_key = sig
  val master_key : string
  (** Key value of Cosmos DB container *)

  val endpoint : string
  (** Name of the endpoint of Cosmos DB container *)
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

module Database (_ : Auth_key) : sig
  val get_code : Cohttp.Response.t -> int
  (** [get_code response] extracts the status code from a Cohttp response *)

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
    (** [list ?timeout database_name] returns a list of collections in the specified database *)

    val create :
      ?indexing_policy:Json_converter_t.indexing_policy option ->
      ?partition_key:Json_converter_t.create_partition_key option ->
      ?timeout:float ->
      string ->
      string ->
      (int * Json_converter_t.collection option, cosmos_error) result Lwt.t
    (** [create ?indexing_policy ?partition_key ?timeout database_name collection_name] creates a collection in the specified database *)

    val create_if_not_exists :
      ?indexing_policy:Json_converter_t.indexing_policy option ->
      ?partition_key:Json_converter_t.create_partition_key option ->
      ?timeout:float ->
      string ->
      string ->
      (int * Json_converter_t.collection option, cosmos_error) result Lwt.t
    (** [create_if_not_exists ?indexing_policy ?partition_key ?timeout database_name collection_name] creates a collection in the specified database if it doesn't already exist *)

    val get :
      ?timeout:float ->
      string ->
      string ->
      (int * Json_converter_t.collection option, cosmos_error) result Lwt.t
    (** [get ?timeout database_name collection_name] returns information about the specified collection *)

    val delete :
      ?timeout:float -> string -> string -> (int, cosmos_error) result Lwt.t

    module Document : sig
      type indexing_directive = Include | Exclude
      (** Directive to include or exclude a document from indexing *)

      val create :
        ?is_upsert:bool ->
        ?indexing_directive:indexing_directive ->
        ?partition_key:string ->
        ?timeout:float ->
        string ->
        string ->
        string ->
        (int * Json_converter_t.collection option, cosmos_error) result Lwt.t
      (** [create ?is_upsert ?indexing_directive ?partition_key ?timeout database_name collection_name document_json] creates a document in the specified collection *)

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
      (** [create_multiple ?is_upsert ?indexing_directive ?timeout ?chunk_size database_name collection_name documents] creates multiple documents in the specified collection *)

      type list_result_meta_data = {
        rid : string;
        (** Resource ID *)
        self : string;
        (** Self link *)
        etag : string;
        (** Entity tag for optimistic concurrency *)
        ts : int;
        (** Timestamp *)
        attachments : string;
        (** Attachments link *)
      }

      type list_result = {
        rid : string;
        (** Resource ID *)
        documents : (string * list_result_meta_data option) list;
        (** List of documents with their metadata *)
        count : int;
        (** Number of documents *)
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
      (** [list ?max_item_count ?continuation ?consistency_level ?session_token ?a_im ?if_none_match ?partition_key_range_id ?timeout database_name collection_name] lists documents in the specified collection *)

      type consistency_level = Strong | Bounded | Session | Eventual
      (** Consistency levels for Cosmos DB operations *)

      val string_of_consistency_level : consistency_level -> string
      (** [string_of_consistency_level level] converts a consistency level to its string representation *)

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
      (** [get ?if_none_match ?partition_key ?consistency_level ?session_token ?timeout database_name collection_name document_id] retrieves a document from the specified collection *)

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
      (** [replace ?indexing_directive ?partition_key ?if_match ?timeout database_name collection_name document_id document_json] replaces a document in the specified collection *)

      val delete :
        ?partition_key:string ->
        ?timeout:float ->
        string ->
        string ->
        string ->
        (int, cosmos_error) result Lwt.t
      (** [delete ?partition_key ?timeout database_name collection_name document_id] deletes a document from the specified collection *)

      val delete_multiple :
        ?partition_key:string ->
        ?timeout:float ->
        ?chunk_size:int ->
        string ->
        string ->
        string list ->
        (int, cosmos_error) result list Lwt.t
      (** [delete_multiple ?partition_key ?timeout ?chunk_size database_name collection_name document_ids] deletes multiple documents from the specified collection *)

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
      (** [query ?max_item_count ?continuation ?consistency_level ?session_token ?is_partition ?partition_key ?timeout database_name collection_name query] executes a query against the specified collection *)
    end
  end

  module User : sig
    val create :
      ?timeout:float ->
      string ->
      string ->
      (int * Json_converter_t.user, cosmos_error) result Lwt.t
    (** [create ?timeout database_name user_name] creates a user in the specified database *)

    val list :
      ?timeout:float ->
      string ->
      (int * Json_converter_t.list_users, cosmos_error) result Lwt.t
    (** [list ?timeout database_name] lists all users in the specified database *)

    val get :
      ?timeout:float ->
      string ->
      string ->
      (int * Json_converter_t.user, cosmos_error) result Lwt.t
    (** [get ?timeout database_name user_name] retrieves information about the specified user *)

    val replace :
      ?timeout:float ->
      string ->
      string ->
      string ->
      (int * Json_converter_t.user, cosmos_error) result Lwt.t
    (** [replace ?timeout database_name old_name new_name] replaces the user name from old_name to new_name *)

    val delete :
      ?timeout:float -> string -> string -> (int, cosmos_error) result Lwt.t
    (** [delete ?timeout database_name user_name] deletes the specified user from the database *)
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
      (int * Json_converter_t.permission, cosmos_error) result Lwt.t
    (** [create dbname user_name ()] will create a permission for the user with
        name user_name in the database with name dbname *)

    val list :
      ?timeout:float ->
      dbname:string ->
      user_name:string ->
      unit ->
      (int * Json_converter_t.list_permissions, cosmos_error) result Lwt.t
    (** [list dbname user_name ()] returns a list of all permissions for the
        user with name user_name in the database with name dbname *)

    val get :
      ?timeout:float ->
      dbname:string ->
      user_name:string ->
      permission_name:string ->
      unit ->
      (int * Json_converter_t.permission, cosmos_error) result Lwt.t
    (** [get dbname user_name permission_name ()] returns the permission with
        name permission_name for the user with name user_name in the database
        with name dbname *)

    val replace :
      ?timeout:float ->
      dbname:string ->
      user_name:string ->
      coll_name:string ->
      permission_mode ->
      permission_name:string ->
      (int * Json_converter_t.permission, cosmos_error) result Lwt.t
    (** [replace dbname user_name ()] returns the updated permission *)

    val delete :
      ?timeout:float ->
      dbname:string ->
      user_name:string ->
      permission_name:string ->
      unit ->
      (int, cosmos_error) result Lwt.t
    (** [delete dbname user_name permission_name ()] deletes the permission with
        name permission_name for the user with name user_name in the database
        with name dbname *)
  end
end

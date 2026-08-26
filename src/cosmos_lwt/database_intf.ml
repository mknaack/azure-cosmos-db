(* Shared signature of the database entry points; see [Databases.S]. *)

type batch_validation_error = Cosmos.Databases_core.batch_validation_error =
  | Too_many_operations of int
  | Mixed_patch_operations
  | Empty_batch

type cosmos_error = Cosmos.Databases_core.cosmos_error

module Response_headers = Cosmos.Databases_core.Response_headers

module type S = sig
  val get_code : Cohttp.Response.t -> int

  val list_databases :
    ?timeout:float ->
    unit ->
    (int * Cosmos.Json_converter_t.list_databases, cosmos_error) result Lwt.t
  (** [list_databases] returns a list of databases *)

  val create :
    ?timeout:float ->
    string ->
    (int * Cosmos.Json_converter_t.database option, cosmos_error) result Lwt.t
  (** [create database_name] creates a database in Cosmos with name
      database_name. *)

  val create_if_not_exists :
    ?timeout:float ->
    string ->
    (int * Cosmos.Json_converter_t.database option, cosmos_error) result Lwt.t
  (** [create_if_not_exists database_name] creates a database in Cosmos with
      name database_name if it not already exists. *)

  val get :
    ?timeout:float ->
    string ->
    (int * Cosmos.Json_converter_t.database option, cosmos_error) result Lwt.t
  (** [get database_name] returns info about the database *)

  val delete : ?timeout:float -> string -> (int, cosmos_error) result Lwt.t
  (** [delete database_name] deletes the database [database_name] from Cosmos *)

  module Collection : sig
    val list :
      ?timeout:float ->
      string ->
      (int * Cosmos.Json_converter_t.list_collections, cosmos_error) result
      Lwt.t

    val create :
      ?indexing_policy:Cosmos.Json_converter_t.indexing_policy option ->
      ?offer_throughput:int ->
      partition_key:Cosmos.Json_converter_t.create_partition_key ->
      ?timeout:float ->
      string ->
      string ->
      (int * Cosmos.Json_converter_t.collection option, cosmos_error) result
      Lwt.t

    val create_if_not_exists :
      ?indexing_policy:Cosmos.Json_converter_t.indexing_policy option ->
      ?offer_throughput:int ->
      partition_key:Cosmos.Json_converter_t.create_partition_key ->
      ?timeout:float ->
      string ->
      string ->
      (int * Cosmos.Json_converter_t.collection option, cosmos_error) result
      Lwt.t

    val get :
      ?timeout:float ->
      string ->
      string ->
      (int * Cosmos.Json_converter_t.collection option, cosmos_error) result
      Lwt.t

    val delete :
      ?timeout:float -> string -> string -> (int, cosmos_error) result Lwt.t

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
        Lwt.t

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
        partition_key:string ->
        ?consistency_level:consistency_level ->
        ?session_token:string ->
        ?timeout:float ->
        string ->
        string ->
        string ->
        (int * string, cosmos_error) result Lwt.t

      val replace :
        ?indexing_directive:indexing_directive ->
        partition_key:string ->
        ?if_match:string ->
        ?timeout:float ->
        string ->
        string ->
        string ->
        string ->
        (int * string, cosmos_error) result Lwt.t

      val delete :
        partition_key:string ->
        ?timeout:float ->
        string ->
        string ->
        string ->
        (int, cosmos_error) result Lwt.t

      val delete_multiple :
        partition_key:string ->
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
        Cosmos.Json_converter_t.query ->
        (int * Response_headers.t * list_result, cosmos_error) result Lwt.t
    end

    module Batch : sig
      type operation =
        | Create of {
            if_match : string option;
            if_none_match : string option;
            body : string;
          }
        | Upsert of {
            if_match : string option;
            if_none_match : string option;
            body : string;
          }
        | Read of {
            id : string;
            if_match : string option;
            if_none_match : string option;
          }
        | Delete of {
            id : string;
            if_match : string option;
            if_none_match : string option;
          }
        | Replace of {
            id : string;
            if_match : string option;
            if_none_match : string option;
            body : string;
          }
        | Patch of {
            id : string;
            if_match : string option;
            patch_op : patch_operation;
          }

      and patch_operation =
        | Add of { path : string; value : string }
        | Set of { path : string; value : string }
        | ReplacePath of { path : string; value : string }
        | Remove of { path : string }
        | Increment of { path : string; value : int }

      type operation_result = {
        status_code : int;
        request_charge : float;
        etag : string option;
        resource_body : string option;
      }

      type batch_result = {
        outcomes : operation_result list;
        total_request_charge : float;
      }

      type validation_error = batch_validation_error =
        | Too_many_operations of int
        | Mixed_patch_operations
        | Empty_batch

      val validate : operation list -> (unit, validation_error) result

      val execute :
        ?timeout:float ->
        ?atomic:bool ->
        ?should_validate:bool ->
        partition_key:string ->
        string ->
        string ->
        operation list ->
        (batch_result, cosmos_error) result Lwt.t
    end

    module Batch_builder : sig
      type t

      val empty : t

      val add_create :
        ?if_match:string -> ?if_none_match:string -> body:string -> t -> t

      val add_upsert :
        ?if_match:string -> ?if_none_match:string -> body:string -> t -> t

      val add_read :
        ?if_match:string -> ?if_none_match:string -> id:string -> t -> t

      val add_delete :
        ?if_match:string -> ?if_none_match:string -> id:string -> t -> t

      val add_replace :
        ?if_match:string ->
        ?if_none_match:string ->
        id:string ->
        body:string ->
        t ->
        t

      val add_patch :
        ?if_match:string ->
        id:string ->
        patch_op:Batch.patch_operation ->
        t ->
        t

      val to_operations : t -> Batch.operation list
      val length : t -> int
    end
  end

  module User : sig
    val create :
      ?timeout:float ->
      string ->
      string ->
      (int * Cosmos.Json_converter_t.user, cosmos_error) result Lwt.t

    val list :
      ?timeout:float ->
      string ->
      (int * Cosmos.Json_converter_t.list_users, cosmos_error) result Lwt.t

    val get :
      ?timeout:float ->
      string ->
      string ->
      (int * Cosmos.Json_converter_t.user, cosmos_error) result Lwt.t

    val replace :
      ?timeout:float ->
      string ->
      string ->
      string ->
      (int * Cosmos.Json_converter_t.user, cosmos_error) result Lwt.t

    val delete :
      ?timeout:float -> string -> string -> (int, cosmos_error) result Lwt.t
  end

  module Permission : sig
    type permission_mode = Read | All

    val create :
      ?timeout:float ->
      ?expiry_seconds:int ->
      dbname:string ->
      user_name:string ->
      coll_name:string ->
      permission_mode ->
      permission_name:string ->
      (int * Cosmos.Json_converter_t.permission, cosmos_error) result Lwt.t
    (** [expiry_seconds] sets [x-ms-documentdb-expiry-seconds], the validity of
        the returned resource token (1..18000, default 3600). *)

    val list :
      ?timeout:float ->
      dbname:string ->
      user_name:string ->
      unit ->
      (int * Cosmos.Json_converter_t.list_permissions, cosmos_error) result
      Lwt.t

    val get :
      ?timeout:float ->
      ?expiry_seconds:int ->
      dbname:string ->
      user_name:string ->
      permission_name:string ->
      unit ->
      (int * Cosmos.Json_converter_t.permission, cosmos_error) result Lwt.t

    val replace :
      ?timeout:float ->
      ?expiry_seconds:int ->
      dbname:string ->
      user_name:string ->
      coll_name:string ->
      permission_mode ->
      permission_name:string ->
      (int * Cosmos.Json_converter_t.permission, cosmos_error) result Lwt.t

    val delete :
      ?timeout:float ->
      dbname:string ->
      user_name:string ->
      permission_name:string ->
      unit ->
      (int, cosmos_error) result Lwt.t
  end

  module Offer : sig
    module Throughput : sig
      type t = Manual of int | Autoscale of { max_throughput : int }

      val to_content : t -> Cosmos.Json_converter_t.offer_content
      val of_content : Cosmos.Json_converter_t.offer_content -> t option
      val string_of : t -> string
    end

    val list :
      ?timeout:float ->
      unit ->
      (int * Cosmos.Json_converter_t.list_offers, cosmos_error) result Lwt.t
    (** [list ()] returns all throughput offers in the account. Requires
        master-key authentication. *)

    val get :
      ?timeout:float ->
      string ->
      (int * Cosmos.Json_converter_t.offer, cosmos_error) result Lwt.t
    (** [get offer_rid] returns the offer identified by [offer_rid]. Requires
        master-key authentication. *)

    val query :
      ?max_item_count:int ->
      ?continuation:string ->
      ?timeout:float ->
      Cosmos.Json_converter_t.query ->
      ( int * Response_headers.t * Cosmos.Json_converter_t.list_offers,
        cosmos_error )
      result
      Lwt.t

    val replace :
      ?migrate:[ `To_autoscale | `To_manual ] ->
      ?timeout:float ->
      Cosmos.Json_converter_t.offer ->
      Throughput.t ->
      (int * Cosmos.Json_converter_t.offer, cosmos_error) result Lwt.t

    val get_for_collection :
      ?timeout:float ->
      string ->
      string ->
      (int * Cosmos.Json_converter_t.offer option, cosmos_error) result Lwt.t

    val get_for_database :
      ?timeout:float ->
      string ->
      (int * Cosmos.Json_converter_t.offer option, cosmos_error) result Lwt.t

    val get_throughput :
      ?timeout:float ->
      string ->
      string ->
      (int * Throughput.t option, cosmos_error) result Lwt.t
    (** [get_throughput dbname coll_name] returns the collection's provisioned
        throughput, or [None] for serverless accounts. Requires master-key
        authentication. *)

    val set_throughput :
      ?migrate:[ `To_autoscale | `To_manual ] ->
      ?timeout:float ->
      string ->
      string ->
      Throughput.t ->
      (int * Cosmos.Json_converter_t.offer, cosmos_error) result Lwt.t
    (** [set_throughput dbname coll_name throughput] changes the collection's
        provisioned throughput. Requires master-key authentication and is
        unavailable for serverless accounts. *)
  end
end

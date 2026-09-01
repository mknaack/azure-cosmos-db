module type Auth_key = Cosmos.Databases_intf.Auth_key

module Credential : sig
  type t = Cosmos.Databases_intf.Credential.t =
    | Master_key of string
    | Resource_token of string
    | Resource_token_provider of (unit -> string)
end

module type Credentials = Cosmos.Databases_intf.Credentials

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
  val x_ms_substatus : t -> string option
end

type batch_validation_error = Cosmos.Databases_core.batch_validation_error =
  | Too_many_operations of int
  | Mixed_patch_operations
  | Empty_batch

type cosmos_error = Cosmos.Databases_core.cosmos_error =
  | Timeout_error
  | Connection_error
  | Azure_error of int * Response_headers.t
  | Batch_validation_error of batch_validation_error

module type S = Database_intf.S

(** [Database] connects with master-key authentication. *)
module Database (Auth_key : Auth_key) : S

(** [Database_as] connects with an arbitrary credential, e.g. a resource token
    minted through [Permission]. Master-key-only operations ([list_databases],
    [User], [Permission], [Offer]) fail with [Azure_error (401 | 403, _)] under
    a resource token. *)
module Database_as (C : Credentials) : S

val credentials_of_token : endpoint:string -> string -> (module Credentials)
(** [credentials_of_token ~endpoint token] wraps a permission's [_token] as
    credentials suitable for [Database_as]. *)

val credentials_of_token_provider :
  endpoint:string -> (unit -> string) -> (module Credentials)
(** [credentials_of_token_provider ~endpoint provider] calls [provider] for
    every request, so callers can refresh expiring resource tokens. *)

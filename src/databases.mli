module type Auth_key = sig
  val master_key : string
  val endpoint : string
end

(* module type Account = sig
 *   type verb = Get | Post | Put | Delete
 *   type resource = Dbs | Colls | Docs
 *   val authorization : verb -> resource -> string -> string -> string
 *   val endpoint : string
 * end *)

module Auth (Keys : Auth_key) : sig
  type verb = Get | Post | Put | Delete
  type resource = Dbs | Colls | Docs

  (* let string_of_verb = function
   *   | Get -> "GET"
   *   | Post -> "POST"
   *   | Put -> "PUT"
   *   | Delete -> "DELETE"
   * 
   * let string_of_resource = function
   *   | Dbs -> "dbs"
   *   | Colls -> "colls"
   *   | Docs -> "docs"
   * 
   * let authorization verb resource date db_name = (\* "type=master&ver=1.0&sig=" ^ key *\)
   *   let verb = string_of_verb verb in (\* get, post, put *\)
   *   let resource_type = string_of_resource resource in (\* "dbs", "colls", "docs". *\)
   *   let resource_id = db_name in
   *   let result = Utility.authorization_token_using_master_key verb resource_type resource_id date Keys.master_key in
   *   result
   * 
   * let endpoint = Keys.endpoint *)
end


(* module Account (Auth_key : Auth_key) : sig
 *   type verb = Auth(Auth_key).verb = Get | Post | Put | Delete
 *   type resource = Auth(Auth_key).resource = Dbs | Colls | Docs
 *   val authorization : verb -> resource -> string -> string -> string
 *   val endpoint : string
 * end *)
    (* val old_headers :
     *   Auth(Auth_key).resource -> Account.verb -> string -> Http_headers.t
     * val host : string
     * val headers :
     *   Account.resource -> Account.verb -> string -> Cohttp.Header.t
     * val json_headers :
     *   Account.resource -> Account.verb -> string -> Cohttp.Header.t *)
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
            (* val string_of_indexing_directive : indexing_directive -> string
             * val old_apply_to_header_if_some :
             *   Http_headers.name ->
             *   ('a -> string) -> 'a option -> Http_headers.t -> Http_headers.t
             * val apply_to_header_if_some :
             *   string ->
             *   ('a -> string) ->
             *   'a option -> Cohttp.Header.t -> Cohttp.Header.t
             * val add_header :
             *   string -> string -> Cohttp.Header.t -> Cohttp.Header.t *)
            val create :
              ?is_upsert:bool ->
              ?indexing_directive:indexing_directive ->
              string ->
              string ->
              string ->
              (int * Json_converter_t.create_collection_result option) Lwt.t
            val list :
              ?max_item_count:int ->
              ?continuation:string ->
              ?consistency_level:string ->
              ?session_token:string ->
              ?a_im:bool ->
              ?if_none_match:string ->
              ?partition_key_range_id:string ->
              string -> string -> (int * string) Lwt.t
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
            (* val old_replace :
             *   ?indexing_directive:indexing_directive ->
             *   ?partition_key:string ->
             *   ?if_match:string ->
             *   string -> string -> string -> string -> Ocsigen_http_frame.t *)
            val delete : string -> string -> string -> int Lwt.t
            val query :
              ?max_item_count:int ->
              ?continuation:string ->
              ?consistency_level:string ->
              ?session_token:string ->
              ?is_partition:bool ->
              string ->
              string -> Json_converter_t.query -> (int * Cohttp_lwt.Body.t) Lwt.t
          end
      end
  end

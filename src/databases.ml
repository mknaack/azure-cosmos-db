open Lwt

(*
 Azure cosmos database documentation: https://docs.microsoft.com/en-us/rest/api/cosmos-db/
dune build @check
*)

module type Auth_key = sig
  val master_key : string
  val endpoint : string
end

module type Account = sig
  type verb = Get | Post | Put | Delete
  type resource = Dbs | Colls | Docs
  val authorization : verb -> resource -> string -> string -> string
  val endpoint : string
end

module Auth (Keys : Auth_key) : Account = struct
  type verb = Get | Post | Put | Delete
  type resource = Dbs | Colls | Docs

  let string_of_verb = function
    | Get -> "GET"
    | Post -> "POST"
    | Put -> "PUT"
    | Delete -> "DELETE"

  let string_of_resource = function
    | Dbs -> "dbs"
    | Colls -> "colls"
    | Docs -> "docs"

  let authorization verb resource date db_name = (* "type=master&ver=1.0&sig=" ^ key *)
    let verb = string_of_verb verb in (* get, post, put *)
    let resource_type = string_of_resource resource in (* "dbs", "colls", "docs". *)
    let resource_id = db_name in
    let result = Utility.authorization_token_using_master_key verb resource_type resource_id date Keys.master_key in
    result

  let endpoint = Keys.endpoint
end

(* list databases: *)
let convert_list_databases s =
  Json_converter_j.list_databases_of_string s

module Database (Auth_key : Auth_key) = struct
  module Account = Auth(Auth_key)

  let host = Account.endpoint ^ ".documents.azure.com"

  let headers resource verb db_name =
    let ms_date =
      let now = Unix.time () in
      Utility.x_ms_date now
    in
    let header = Cohttp.Header.init () in
    let header = Cohttp.Header.add header "authorization" (Account.authorization verb resource ms_date db_name) in
    let header = Cohttp.Header.add header "x-ms-version" "2017-02-22" in
    let header = Cohttp.Header.add header "x-ms-date" ms_date in
    header

  let json_headers resource verb db_name =
    let header = headers resource verb db_name in
    let header = Cohttp.Header.add header "content_type" "application/json" in
    header

  let get_code resp = resp |> Cohttp_lwt_unix.Response.status |> Cohttp.Code.code_of_status

  let list_databases () =
    let uri = Uri.make ~scheme:"https" ~host ~port:443 ~path:"dbs" () in
    Cohttp_lwt_unix.Client.get ~headers:(headers Account.Dbs Account.Get "") uri >>= fun (resp, body) ->
    let code = get_code resp in
    body |> Cohttp_lwt.Body.to_string >|= fun body ->
    let value = Json_converter_j.list_databases_of_string body in
    (code, value)

  (* create database: *)

  let create name =
    let body =
      ({id = name}: Json_converter_j.create_database)
      |> Json_converter_j.string_of_create_database
      |> Cohttp_lwt.Body.of_string
    in
    let uri = Uri.make ~scheme:"https" ~host ~port:443 ~path:"dbs" () in
    let headers = (json_headers Account.Dbs Account.Post "") in
    Cohttp_lwt_unix.Client.post ~headers ~body uri >>= fun (resp, body) ->
    let code = get_code resp in
    body |> Cohttp_lwt.Body.to_string >|= fun body ->
    let value = match code with
      | 200 -> Some (Json_converter_j.create_database_result_of_string body)
      | _ -> None
    in
    (code, value)

  let get name =
    let uri = Uri.make ~scheme:"https" ~host ~port:443 ~path:("dbs/" ^ name) () in
    Cohttp_lwt_unix.Client.get ~headers:(headers Account.Dbs Account.Get ("dbs/" ^ name)) uri >>= fun (resp, body) ->
    let code = get_code resp in
    body |> Cohttp_lwt.Body.to_string >|= fun body ->
    let value = match code with
      | 200 -> Some (Json_converter_j.database_of_string body)
      | _ -> None
    in
    (code, value)

  let delete name =
    let uri = Uri.make ~scheme:"https" ~host ~port:443 ~path:("dbs/" ^ name) () in
    Cohttp_lwt_unix.Client.delete ~headers:(headers Account.Dbs Account.Delete ("dbs/" ^ name)) uri >>= fun (resp, body) ->
    let code = get_code resp in
    body |> Cohttp_lwt.Body.to_string >|= fun _ ->
    code

  module Collection = struct
    let list dbname =
      let uri = Uri.make ~scheme:"https" ~host ~port:443 ~path:("dbs/" ^ dbname ^ "/colls")  () in
      Cohttp_lwt_unix.Client.get ~headers:(headers Account.Colls Account.Get ("dbs/" ^ dbname)) uri >>= fun (resp, body) ->
      let code = get_code resp in
      body |> Cohttp_lwt.Body.to_string >|= fun body ->
      let value = Json_converter_j.list_collections_of_string body in
      (code, value)

  let create dbname coll_name =
    let body =
      ({id = coll_name; indexingPolicy = None; partitionKey = None}: Json_converter_j.create_collection) |>
      Json_converter_j.string_of_create_collection |>
      Cohttp_lwt.Body.of_string
    in
    let uri = Uri.make ~scheme:"https" ~host ~port:443 ~path:("/dbs/" ^ dbname ^ "/colls") () in
    let headers = (json_headers Account.Colls Account.Post ("dbs/" ^ dbname)) in
    Cohttp_lwt_unix.Client.post ~headers ~body uri >>= fun (resp, body) ->
    let code = get_code resp in
    body |> Cohttp_lwt.Body.to_string >|= fun body ->
    let value = match code with
      | 200 -> Some (Json_converter_j.create_collection_result_of_string body)
      | _ -> None
    in
    (code, value)

  let get name coll_name =
    let path = "/dbs/" ^ name ^ "/colls/" ^ coll_name in
    let uri = Uri.make ~scheme:"https" ~host ~port:443 ~path () in
    Cohttp_lwt_unix.Client.get ~headers:(headers Account.Colls Account.Get ("dbs/" ^ name^ "/colls/" ^ coll_name)) uri >>= fun (resp, body) ->
    let code = get_code resp in
    body |> Cohttp_lwt.Body.to_string >|= fun body ->
    let value = match code with
      | 200 -> Some (Json_converter_j.collection_of_string body)
      | _ -> None
    in
    (code, value)

  let delete name coll_name =
    let path = "/dbs/" ^ name ^ "/colls/" ^ coll_name in
    let header_path = "dbs/" ^ name ^ "/colls/" ^ coll_name in
    let uri = Uri.make ~scheme:"https" ~host ~port:443 ~path () in
    Cohttp_lwt_unix.Client.delete ~headers:(headers Account.Colls Account.Delete header_path) uri >>= fun (resp, body) ->
    let code = get_code resp in
    body |> Cohttp_lwt.Body.to_string >|= fun _ ->
    code

  (*
TODO:
 - Replace a collection
 - Get partition key ranges for a collection
*)
    module Document = struct
      type indexing_directive =
        | Include
        | Exclude

      let string_of_indexing_directive = function
        | Include -> "Include"
        | Exclude -> "Exclude"

      let apply_to_header_if_some name string_of values headers = match values with
        | None -> headers
        | Some value -> Cohttp.Header.add headers name (string_of value)

      let add_header name value header =
        Cohttp.Header.add header name value

      let create ?is_upsert ?indexing_directive dbname coll_name content =
        let body = Cohttp_lwt.Body.of_string content in
        let path = ("/dbs/" ^ dbname ^ "/colls/" ^ coll_name ^ "/docs") in
        let uri = Uri.make ~scheme:"https" ~host ~port:443 ~path () in
        let headers =
          json_headers Account.Docs Account.Post ("dbs/" ^ dbname ^ "/colls/" ^ coll_name)
          |> apply_to_header_if_some "x-ms-documentdb-is-upsert" Utility.string_of_bool is_upsert
          |> apply_to_header_if_some "x-ms-indexing-directive" string_of_indexing_directive indexing_directive
        in
        Cohttp_lwt_unix.Client.post ~headers ~body uri >>= fun (resp, body) ->
        let code = get_code resp in
        body |> Cohttp_lwt.Body.to_string >|= fun body ->
        let value = match code with
          | 200 -> Some (Json_converter_j.create_collection_result_of_string body)
          | _ -> None
        in
        code, value

      type list_result = {
          rid: string;
          documents: string list;
          count: int;
        }

      let convert_to_list_result value =
        let open Yojson.Basic.Util in
        let json = Yojson.Basic.from_string value in
        let rid = json |> member "_rid" |> to_string in
        let count = json |> member "_count" |> to_int in
        let docs = json |> member "Documents" |> to_list in
        let string_docs = List.map Yojson.Basic.to_string docs in
        Some { rid; documents = string_docs; count }
      
      let list ?max_item_count ?continuation ?consistency_level ?session_token ?a_im ?if_none_match ?partition_key_range_id dbname coll_name =
        let apply_a_im_to_header_if_some name values headers = match values with
          | None -> headers
          | Some false -> headers
          | Some true -> Cohttp.Header.add headers name "Incremental feed"
        in
        let path = ("/dbs/" ^ dbname ^ "/colls/" ^ coll_name ^ "/docs") in
        let uri = Uri.make ~scheme:"https" ~host ~port:443 ~path () in
        let headers =
          json_headers Account.Docs Account.Get ("dbs/" ^ dbname ^ "/colls/" ^ coll_name)
          |> apply_to_header_if_some "x-ms-max-item-count" string_of_int max_item_count
          |> apply_to_header_if_some "x-ms-continuation" (fun x -> x) continuation
          |> apply_to_header_if_some "x-ms-consistency-level" (fun x -> x) consistency_level
          |> apply_to_header_if_some "x-ms-session-token" (fun x -> x) session_token
          |> apply_a_im_to_header_if_some "A-IM" a_im
          |> apply_to_header_if_some "If-None-Match" (fun x -> x) if_none_match
          |> apply_to_header_if_some "x-ms-documentdb-partitionkeyrangeid" (fun x -> x) partition_key_range_id
        in
        Cohttp_lwt_unix.Client.get ~headers uri >>= fun (resp, body) ->
        let code = get_code resp in
        body |> Cohttp_lwt.Body.to_string >|= fun body ->
        let value = match code with
          | 200 -> convert_to_list_result body
          | _ -> None
        in
        code, value

      type consistency_level =
        | Strong
        | Bounded
        | Session
        | Eventual

      let string_of_consistency_level = function
        | Strong -> "Strong"
        | Bounded -> "Bounded"
        | Session -> "Session"
        | Eventual -> "Eventual"

      let get ?if_none_match ?partition_key ?consistency_level ?session_token dbname coll_name doc_id =
        let headers =
          json_headers Account.Docs Account.Get ("dbs/" ^ dbname ^ "/colls/" ^ coll_name ^ "/docs/" ^ doc_id)
          |> apply_to_header_if_some "If-None-Match" (fun x -> x) if_none_match
          |> apply_to_header_if_some "x-ms-documentdb-partitionkey" (fun x -> x) partition_key
          |> apply_to_header_if_some "x-ms-consistency-level" string_of_consistency_level consistency_level
          |> apply_to_header_if_some "x-ms-session-token" (fun x -> x) session_token
        in
        let path = "/dbs/" ^ dbname ^ "/colls/" ^ coll_name ^ "/docs/" ^ doc_id in
        let uri = Uri.make ~scheme:"https" ~host ~port:443 ~path () in
        Cohttp_lwt_unix.Client.get ~headers uri >>= fun (resp, body) ->
        let code = get_code resp in
        body |> Cohttp_lwt.Body.to_string >|= fun body ->
        (code, body)

      let replace ?indexing_directive ?partition_key ?if_match dbname coll_name doc_id content =
        let body = Cohttp_lwt.Body.of_string content in
        let headers =
          json_headers Account.Docs Account.Put ("dbs/" ^ dbname ^ "/colls/" ^ coll_name ^ "/docs/" ^ doc_id)
          |> apply_to_header_if_some "x-ms-indexing-directive" string_of_indexing_directive indexing_directive
          |> apply_to_header_if_some "x-ms-documentdb-partitionkey" (fun x -> x) partition_key
          |> apply_to_header_if_some "If-Match" (fun x -> x) if_match
        in
        let path = "/dbs/" ^ dbname ^ "/colls/" ^ coll_name ^ "/docs/" ^ doc_id in
        let uri = Uri.make ~scheme:"https" ~host ~port:443 ~path () in
        Cohttp_lwt_unix.Client.put ~headers ~body uri >>= fun (resp, body) ->
        let code = get_code resp in
        body |> Cohttp_lwt.Body.to_string >|= fun _ ->
        code, body

      let delete dbname coll_name doc_id =
        let path = "/dbs/" ^ dbname ^ "/colls/" ^ coll_name ^ "/docs/" ^ doc_id in
        let headers = headers Account.Docs Account.Delete ("dbs/" ^ dbname ^ "/colls/" ^ coll_name ^ "/docs/" ^ doc_id) in
        let uri = Uri.make ~scheme:"https" ~host ~port:443 ~path () in
        Cohttp_lwt_unix.Client.delete ~headers uri >>= fun (resp, body) ->
        let code = get_code resp in
        body |> Cohttp_lwt.Body.to_string >|= fun _ ->
        code

      let query ?max_item_count ?continuation ?consistency_level ?session_token ?is_partition dbname coll_name query =
        let headers s =
          let h = headers Account.Docs Account.Post s in
          Cohttp.Header.add h "x-ms-documentdb-isquery" (Utility.string_of_bool true)
          |> apply_to_header_if_some "x-ms-max-item-count" string_of_int max_item_count
          |> apply_to_header_if_some "x-ms-continuation" (fun x -> x) continuation
          |> apply_to_header_if_some "x-ms-consistency-level" (fun x -> x) consistency_level
          |> apply_to_header_if_some "x-ms-session-token" (fun x -> x) session_token
          |> apply_to_header_if_some "x-ms-documentdb-query-enablecrosspartition" Utility.string_of_bool is_partition
          |> add_header "content-type" "application/query+json"
        in
        let path = "/dbs/" ^ dbname ^ "/colls/" ^ coll_name ^ "/docs" in
        let headers = headers ("dbs/" ^ dbname ^ "/colls/" ^ coll_name) in
        let body =
          Json_converter_j.string_of_query query
          |> Cohttp_lwt.Body.of_string
        in
        let uri = Uri.make ~scheme:"https" ~host ~port:443 ~path () in
        Cohttp_lwt_unix.Client.post ~headers ~body uri >>= fun (resp, body) ->
        let code = get_code resp in
        body |> Cohttp_lwt.Body.to_string >|= fun body ->
        let value = match code with
          | 200 -> convert_to_list_result body
          | _ -> None
        in
        code, value
    end
  end
end

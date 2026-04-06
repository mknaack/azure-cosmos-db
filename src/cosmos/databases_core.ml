(*
 Azure cosmos database documentation: https://docs.microsoft.com/en-us/rest/api/cosmos-db/
*)

module type Account = sig
  type resource = Dbs | Colls | Docs | Users | Permissions

  val authorization :
    Utilities.Verb.t -> resource -> Utilities.Ms_time.t -> string -> string

  val endpoint : string
end

module Auth (Keys : Databases_intf.Auth_key) : Account = struct
  type resource = Dbs | Colls | Docs | Users | Permissions

  let string_of_resource = function
    | Dbs -> "dbs"
    | Colls -> "colls"
    | Docs -> "docs"
    | Users -> "users"
    | Permissions -> "permissions"

  let authorization verb resource date db_name =
    Utility.authorization_token_using_master_key
      (Utilities.Verb.string_of_verb verb)
      (string_of_resource resource)
      db_name
      (Utilities.Ms_time.x_ms_date date)
      Keys.master_key

  let endpoint = Keys.endpoint
end

module Response_headers = struct
  type t = {
    content_type : string option;
    date : string option;
    etag : string option;
    x_ms_activity_id : string option;
    x_ms_alt_content_path : string option;
    x_ms_continuation : string option;
    x_ms_item_count : string option;
    x_ms_request_charge : string option;
    x_ms_resource_quota : string option;
    x_ms_resource_usage : string option;
    x_ms_retry_after_ms : string option;
    x_ms_schemaversion : string option;
    x_ms_serviceversion : string option;
    x_ms_session_token : string option;
  }

  let empty =
    {
      content_type = None;
      date = None;
      etag = None;
      x_ms_activity_id = None;
      x_ms_alt_content_path = None;
      x_ms_continuation = None;
      x_ms_item_count = None;
      x_ms_request_charge = None;
      x_ms_resource_quota = None;
      x_ms_resource_usage = None;
      x_ms_retry_after_ms = None;
      x_ms_schemaversion = None;
      x_ms_serviceversion = None;
      x_ms_session_token = None;
    }

  let update t value_tuple =
    let key, value = value_tuple in
    match key with
    | "Content-Type" -> { t with content_type = Some value }
    | "Date" -> { t with date = Some value }
    | "etag" -> { t with etag = Some value }
    | "x-ms-activity-id" -> { t with x_ms_activity_id = Some value }
    | "x-ms-alt-content-path" -> { t with x_ms_alt_content_path = Some value }
    | "x-ms-continuation" -> { t with x_ms_continuation = Some value }
    | "x-ms-item-count" -> { t with x_ms_item_count = Some value }
    | "x-ms-request-charge" -> { t with x_ms_request_charge = Some value }
    | "x-ms-resource-quota" -> { t with x_ms_resource_quota = Some value }
    | "x-ms-resource-usage" -> { t with x_ms_resource_usage = Some value }
    | "x-ms-retry-after-ms" -> { t with x_ms_retry_after_ms = Some value }
    | "x-ms-schemaversion" -> { t with x_ms_schemaversion = Some value }
    | "x-ms-serviceversion" -> { t with x_ms_serviceversion = Some value }
    | "x-ms-session-token" -> { t with x_ms_session_token = Some value }
    | _ -> t

  let get_header resp =
    resp |> Cohttp.Response.headers |> Cohttp.Header.to_list
    |> List.fold_left update empty

  let content_type t = t.content_type
  let date t = t.date
  let etag t = t.etag
  let x_ms_activity_id t = t.x_ms_activity_id
  let x_ms_alt_content_path t = t.x_ms_alt_content_path
  let x_ms_continuation t = t.x_ms_continuation
  let x_ms_item_count t = t.x_ms_item_count
  let x_ms_request_charge t = t.x_ms_request_charge
  let x_ms_resource_quota t = t.x_ms_resource_quota
  let x_ms_resource_usage t = t.x_ms_resource_usage
  let x_ms_retry_after_ms t = t.x_ms_retry_after_ms
  let x_ms_schemaversion t = t.x_ms_schemaversion
  let x_ms_serviceversion t = t.x_ms_serviceversion
  let x_ms_session_token t = t.x_ms_session_token
end

type cosmos_error =
  | Timeout_error
  | Connection_error
  | Azure_error of int * Response_headers.t

module Make
    (IO : Databases_intf.IO)
    (Http : Databases_intf.Http_client with type 'a io := 'a IO.t)
    (Auth_key : Databases_intf.Auth_key) =
struct
  let ( let* ) = IO.bind

  module Account = Auth (Auth_key)

  let host = Utility.adjust_host Account.endpoint
  let timeout_error = IO.return (Error Timeout_error)
  let connection_error = IO.return (Error Connection_error)

  let wrap_timeout timeout command =
    match timeout with
    | None ->
        let* result = command in
        IO.return (Some result)
    | Some t -> IO.with_timeout t command

  let headers resource verb db_name =
    let ms_date = Utilities.Ms_time.create_now () in
    let header = Cohttp.Header.init () in
    let header =
      Cohttp.Header.add header "authorization"
        (Account.authorization verb resource ms_date db_name)
    in
    let header = Cohttp.Header.add header "x-ms-version" "2018-12-31" in
    let header =
      Cohttp.Header.add header "x-ms-date" (Utilities.Ms_time.x_ms_date ms_date)
    in
    header

  let json_headers resource verb db_name =
    let header = headers resource verb db_name in
    let header = Cohttp.Header.add header "content_type" "application/json" in
    header

  let get_code resp =
    resp |> Cohttp.Response.status |> Cohttp.Code.code_of_status

  let result_or_error_with_result expected_code f (resp : Cohttp.Response.t)
      body =
    let code = get_code resp in
    let headers = Response_headers.get_header resp in
    if code = expected_code then IO.return (Ok (expected_code, f body))
    else IO.return (Error (Azure_error (code, headers)))

  let result_or_error expected_code resp =
    let code = get_code resp in
    if code = expected_code then Result.ok expected_code
    else
      let response_header = Response_headers.get_header resp in
      Result.error (Azure_error (code, response_header))

  let with_204_do = result_or_error 204

  let handle_http_error = function
    | Http.Connection_refused -> connection_error
    | Http.Other_error _exn -> connection_error

  let make_uri path = Uri.make ~scheme:"https" ~host ~port:443 ~path ()

  let header_path_of_path path =
    if String.length path > 0 && path.[0] = '/' then
      String.sub path 1 (String.length path - 1)
    else path

  let handle_response response f =
    match response with
    | None -> timeout_error
    | Some (Error e) -> handle_http_error e
    | Some (Ok (resp, body)) -> f resp body

  let list_databases ?timeout () =
    let uri = make_uri "dbs" in
    let* response =
      Http.get ~headers:(headers Account.Dbs Utilities.Verb.Get "") uri
      |> wrap_timeout timeout
    in
    handle_response response (fun resp body ->
        let code = get_code resp in
        let value = Json_converter_j.list_databases_of_string body in
        IO.return @@ Result.ok (code, value))

  let create ?timeout name =
    let body =
      ({ id = name } : Json_converter_j.create_database)
      |> Json_converter_j.string_of_create_database
    in
    let uri = make_uri "dbs" in
    let hdrs = json_headers Account.Dbs Utilities.Verb.Post "" in
    let* response = Http.post ~headers:hdrs ~body uri |> wrap_timeout timeout in
    handle_response response (fun resp body ->
        let result body = Some (Json_converter_j.database_of_string body) in
        result_or_error_with_result 201 result resp body)

  let get ?timeout name =
    let path = "dbs/" ^ name in
    let uri = make_uri path in
    let* response =
      Http.get ~headers:(headers Account.Dbs Utilities.Verb.Get path) uri
      |> wrap_timeout timeout
    in
    handle_response response (fun resp body ->
        let result body = Some (Json_converter_j.database_of_string body) in
        result_or_error_with_result 200 result resp body)

  let create_if_not_exists ?timeout name =
    let* exists = get ?timeout name in
    match exists with
    | Ok (code, result) -> IO.return (Ok (code, result))
    | Error (Azure_error (404, _)) -> create ?timeout name
    | Error x -> IO.return (Error x)

  let delete ?timeout name =
    let path = "dbs/" ^ name in
    let uri = make_uri path in
    let* response =
      Http.delete ~headers:(headers Account.Dbs Utilities.Verb.Delete path) uri
      |> wrap_timeout timeout
    in
    handle_response response (fun resp _body -> IO.return (with_204_do resp))

  module Collection = struct
    let list ?timeout dbname =
      let path = "dbs/" ^ dbname ^ "/colls" in
      let uri = make_uri path in
      let* response =
        Http.get
          ~headers:(headers Account.Colls Utilities.Verb.Get ("dbs/" ^ dbname))
          uri
        |> wrap_timeout timeout
      in
      handle_response response (fun resp body ->
          let value body = Json_converter_j.list_collections_of_string body in
          result_or_error_with_result 200 value resp body)

    let create ?(indexing_policy = None) ?(partition_key = None) ?timeout dbname
        coll_name =
      let body =
        ({ id = coll_name; indexing_policy; partition_key }
          : Json_converter_j.create_collection)
        |> Json_converter_j.string_of_create_collection
      in
      let path = "/dbs/" ^ dbname ^ "/colls" in
      let uri = make_uri path in
      let hdrs =
        json_headers Account.Colls Utilities.Verb.Post ("dbs/" ^ dbname)
      in
      let* response =
        Http.post ~headers:hdrs ~body uri |> wrap_timeout timeout
      in
      handle_response response (fun resp body ->
          let value body = Some (Json_converter_j.collection_of_string body) in
          result_or_error_with_result 201 value resp body)

    let get ?timeout name coll_name =
      let path = "/dbs/" ^ name ^ "/colls/" ^ coll_name in
      let uri = make_uri path in
      let* response =
        Http.get
          ~headers:
            (headers Account.Colls Utilities.Verb.Get (header_path_of_path path))
          uri
        |> wrap_timeout timeout
      in
      handle_response response (fun resp body ->
          let value body = Some (Json_converter_j.collection_of_string body) in
          result_or_error_with_result 200 value resp body)

    let create_if_not_exists ?(indexing_policy = None) ?(partition_key = None)
        ?timeout dbname coll_name =
      let* exists = get ?timeout dbname coll_name in
      match exists with
      | Ok result -> IO.return (Result.ok result)
      | Error (Azure_error (404, _)) ->
          create ?timeout ~indexing_policy ~partition_key dbname coll_name
      | Error x -> IO.return (Error x)

    let delete ?timeout name coll_name =
      let path = "/dbs/" ^ name ^ "/colls/" ^ coll_name in
      let uri = make_uri path in
      let* response =
        Http.delete
          ~headers:
            (headers Account.Colls Utilities.Verb.Delete
               (header_path_of_path path))
          uri
        |> wrap_timeout timeout
      in
      handle_response response (fun resp _body -> IO.return (with_204_do resp))

    module Document = struct
      type indexing_directive = Include | Exclude

      let string_of_indexing_directive = function
        | Include -> "Include"
        | Exclude -> "Exclude"

      let string_of_partition_key s = "[\"" ^ s ^ "\"]"

      let apply_to_header_if_some name string_of values headers =
        match values with
        | None -> headers
        | Some value -> Cohttp.Header.add headers name (string_of value)

      let add_header name value header = Cohttp.Header.add header name value

      let create ?is_upsert ?indexing_directive ?partition_key ?timeout dbname
          coll_name content =
        let path = "/dbs/" ^ dbname ^ "/colls/" ^ coll_name ^ "/docs" in
        let uri = make_uri path in
        let hdrs =
          json_headers Account.Docs Utilities.Verb.Post
            ("dbs/" ^ dbname ^ "/colls/" ^ coll_name)
          |> apply_to_header_if_some "x-ms-documentdb-is-upsert" string_of_bool
               is_upsert
          |> apply_to_header_if_some "x-ms-indexing-directive"
               string_of_indexing_directive indexing_directive
          |> apply_to_header_if_some "x-ms-documentdb-partitionkey"
               string_of_partition_key partition_key
        in
        let () = Random.self_init () in
        let rec post retry () =
          IO.catch
            (fun () ->
              let* post_response = Http.post ~headers:hdrs ~body:content uri in
              match post_response with
              | Error Http.Connection_refused ->
                  if retry > 0 then
                    let sleep_time = Random.int 5 |> float_of_int in
                    let* () = IO.sleep sleep_time in
                    post (retry - 1) ()
                  else connection_error
              | Error (Http.Other_error exn) -> raise exn
              | Ok (resp, body_str) ->
                  let code = get_code resp in
                  let value =
                    match code with
                    | 200 ->
                        Some (Json_converter_j.collection_of_string body_str)
                    | _ -> None
                  in
                  if code = 429 then
                    let response_header = Response_headers.get_header resp in
                    let milliseconds =
                      Response_headers.x_ms_retry_after_ms response_header
                      |> Option.value ~default:"0" |> int_of_string_opt
                      |> Option.value ~default:0 |> Int.to_float
                    in
                    let* () = IO.sleep (milliseconds /. 1000.) in
                    if retry > 0 then post (retry - 1) () else timeout_error
                  else IO.return (Result.ok (code, value)))
            (fun _exn ->
              if retry > 0 then
                let sleep_time = Random.int 5 |> float_of_int in
                let* () = IO.sleep sleep_time in
                post (retry - 1) ()
              else connection_error)
        in
        let with_timeout_wrapper () =
          match timeout with
          | None -> post 10 ()
          | Some t -> (
              let* result = IO.with_timeout t (post 10 ()) in
              match result with Some r -> IO.return r | None -> timeout_error)
        in
        with_timeout_wrapper ()

      let create_multiple ?is_upsert ?indexing_directive ?timeout
          ?(chunk_size = 100) dbname coll_name content_list =
        let rec take_first n acc = function
          | [] -> IO.return @@ acc
          | x ->
              let first, rest = Utilities.take_first n x in
              let* r =
                IO.parallel_map
                  (fun (partition_key, content) ->
                    create ?is_upsert ?indexing_directive ?partition_key
                      ?timeout dbname coll_name content)
                  first
              in
              take_first n (r @ acc) rest
        in
        take_first chunk_size [] content_list

      type list_result_meta_data = {
        rid : string;
        self : string;
        etag : string;
        ts : int;
        attachments : string;
      }

      let convert_to_list_result_meta_data json =
        let open Yojson.Basic.Util in
        try
          let rid = json |> member "_rid" |> to_string in
          let self = json |> member "_self" |> to_string in
          let etag = json |> member "_etag" |> to_string in
          let ts = json |> member "_ts" |> to_int in
          let attachments = json |> member "_attachments" |> to_string in
          Some { rid; self; etag; ts; attachments }
        with Type_error _ -> None

      type list_result = {
        rid : string;
        documents : (string * list_result_meta_data option) list;
        count : int;
      }

      let convert_to_list_result value =
        let open Yojson.Basic.Util in
        let json = Yojson.Basic.from_string value in
        let rid = json |> member "_rid" |> to_string in
        let count = json |> member "_count" |> to_int in
        let docs = json |> member "Documents" |> to_list in
        let string_docs =
          List.map
            (fun x ->
              (Yojson.Basic.to_string x, convert_to_list_result_meta_data x))
            docs
        in
        { rid; documents = string_docs; count }

      let list ?max_item_count ?continuation ?consistency_level ?session_token
          ?a_im ?if_none_match ?partition_key_range_id ?timeout dbname coll_name
          =
        let apply_a_im_to_header_if_some name values headers =
          match values with
          | None -> headers
          | Some false -> headers
          | Some true -> Cohttp.Header.add headers name "Incremental feed"
        in
        let path = "/dbs/" ^ dbname ^ "/colls/" ^ coll_name ^ "/docs" in
        let uri = make_uri path in
        let hdrs =
          json_headers Account.Docs Utilities.Verb.Get
            ("dbs/" ^ dbname ^ "/colls/" ^ coll_name)
          |> apply_to_header_if_some "x-ms-max-item-count" string_of_int
               max_item_count
          |> apply_to_header_if_some "x-ms-continuation" Fun.id continuation
          |> apply_to_header_if_some "x-ms-consistency-level" Fun.id
               consistency_level
          |> apply_to_header_if_some "x-ms-session-token" Fun.id session_token
          |> apply_a_im_to_header_if_some "A-IM" a_im
          |> apply_to_header_if_some "If-None-Match" Fun.id if_none_match
          |> apply_to_header_if_some "x-ms-documentdb-partitionkeyrangeid"
               Fun.id partition_key_range_id
        in
        let* get_action = Http.get ~headers:hdrs uri |> wrap_timeout timeout in
        handle_response get_action (fun resp body ->
            let code = get_code resp in
            let response_header = Response_headers.get_header resp in
            if code = 200 then
              let result = convert_to_list_result body in
              IO.return (Ok (200, response_header, result))
            else IO.return (Error (Azure_error (code, response_header))))

      type consistency_level = Strong | Bounded | Session | Eventual

      let string_of_consistency_level = function
        | Strong -> "Strong"
        | Bounded -> "Bounded"
        | Session -> "Session"
        | Eventual -> "Eventual"

      let get ?if_none_match ?partition_key ?consistency_level ?session_token
          ?timeout dbname coll_name doc_id =
        let hdrs =
          json_headers Account.Docs Utilities.Verb.Get
            ("dbs/" ^ dbname ^ "/colls/" ^ coll_name ^ "/docs/" ^ doc_id)
          |> apply_to_header_if_some "If-None-Match" Fun.id if_none_match
          |> apply_to_header_if_some "x-ms-documentdb-partitionkey"
               string_of_partition_key partition_key
          |> apply_to_header_if_some "x-ms-consistency-level"
               string_of_consistency_level consistency_level
          |> apply_to_header_if_some "x-ms-session-token" Fun.id session_token
        in
        let path =
          "/dbs/" ^ dbname ^ "/colls/" ^ coll_name ^ "/docs/" ^ doc_id
        in
        let uri = make_uri path in
        let* response = Http.get ~headers:hdrs uri |> wrap_timeout timeout in
        handle_response response (fun resp body ->
            let code = get_code resp in
            IO.return (Result.ok (code, body)))

      let replace ?indexing_directive ?partition_key ?if_match ?timeout dbname
          coll_name doc_id content =
        let hdrs =
          json_headers Account.Docs Utilities.Verb.Put
            ("dbs/" ^ dbname ^ "/colls/" ^ coll_name ^ "/docs/" ^ doc_id)
          |> apply_to_header_if_some "x-ms-indexing-directive"
               string_of_indexing_directive indexing_directive
          |> apply_to_header_if_some "x-ms-documentdb-partitionkey"
               string_of_partition_key partition_key
          |> apply_to_header_if_some "If-Match" Fun.id if_match
        in
        let path =
          "/dbs/" ^ dbname ^ "/colls/" ^ coll_name ^ "/docs/" ^ doc_id
        in
        let uri = make_uri path in
        let* response =
          Http.put ~headers:hdrs ~body:content uri |> wrap_timeout timeout
        in
        handle_response response (fun resp body ->
            let code = get_code resp in
            IO.return (Ok (code, body)))

      let delete ?partition_key ?timeout dbname coll_name doc_id =
        let path =
          Printf.sprintf "/dbs/%s/colls/%s/docs/%s" dbname coll_name doc_id
        in
        let hdrs =
          header_path_of_path path
          |> headers Account.Docs Utilities.Verb.Delete
          |> apply_to_header_if_some "x-ms-documentdb-partitionkey"
               string_of_partition_key partition_key
        in
        let uri = make_uri path in
        let rec delete_loop retry () =
          let* response =
            Http.delete ~headers:hdrs uri |> wrap_timeout timeout
          in
          match response with
          | None -> timeout_error
          | Some (Error e) -> handle_http_error e
          | Some (Ok (resp, _body)) ->
              let code = get_code resp in
              if code = 429 then
                let response_header = Response_headers.get_header resp in
                let milliseconds =
                  Response_headers.x_ms_retry_after_ms response_header
                  |> Option.value ~default:"0" |> int_of_string_opt
                  |> Option.value ~default:0 |> Int.to_float
                in
                let* () = IO.sleep (milliseconds /. 1000.) in
                if retry > 0 then delete_loop (retry - 1) () else timeout_error
              else IO.return (Ok code)
        in
        delete_loop 3 ()

      let delete_multiple ?partition_key ?timeout ?(chunk_size = 100) dbname
          coll_name doc_ids =
        let rec take_first n acc = function
          | [] -> IO.return @@ acc
          | x ->
              let first, rest = Utilities.take_first n x in
              let* r =
                IO.parallel_map
                  (delete ?partition_key ?timeout dbname coll_name)
                  first
              in
              take_first n (r @ acc) rest
        in
        take_first chunk_size [] doc_ids

      let query ?max_item_count ?continuation ?consistency_level ?session_token
          ?is_partition ?partition_key ?timeout dbname coll_name query =
        let make_headers s =
          let h = headers Account.Docs Utilities.Verb.Post s in
          Cohttp.Header.add h "x-ms-documentdb-isquery" (string_of_bool true)
          |> apply_to_header_if_some "x-ms-max-item-count" string_of_int
               max_item_count
          |> apply_to_header_if_some "x-ms-continuation" Fun.id continuation
          |> apply_to_header_if_some "x-ms-consistency-level" Fun.id
               consistency_level
          |> apply_to_header_if_some "x-ms-session-token" Fun.id session_token
          |> apply_to_header_if_some
               "x-ms-documentdb-query-enablecrosspartition" string_of_bool
               is_partition
          |> apply_to_header_if_some "x-ms-documentdb-partitionkey"
               string_of_partition_key partition_key
          |> add_header "content-type" "application/query+json"
        in
        let path = "/dbs/" ^ dbname ^ "/colls/" ^ coll_name ^ "/docs" in
        let hdrs = make_headers ("dbs/" ^ dbname ^ "/colls/" ^ coll_name) in
        let body = Json_converter_j.string_of_query query in
        let uri = make_uri path in
        let* response =
          Http.post ~headers:hdrs ~body uri |> wrap_timeout timeout
        in
        handle_response response (fun resp body ->
            let code = get_code resp in
            let response_header = Response_headers.get_header resp in
            if code = 200 then
              let result = convert_to_list_result body in
              IO.return (Ok (200, response_header, result))
            else IO.return (Error (Azure_error (code, response_header))))
    end
  end

  module User = struct
    let resource = Account.Users
    let headers = headers resource

    let create ?timeout dbname user_name =
      let body =
        ({ id = user_name } : Json_converter_j.create_user)
        |> Json_converter_j.string_of_create_user
      in
      let uri = make_uri ("/dbs/" ^ dbname ^ "/users") in
      let hdrs = json_headers resource Utilities.Verb.Post ("dbs/" ^ dbname) in
      let* response =
        Http.post ~headers:hdrs ~body uri |> wrap_timeout timeout
      in
      handle_response response (fun resp body ->
          let value body = Json_converter_j.user_of_string body in
          result_or_error_with_result 201 value resp body)

    let list ?timeout dbname =
      let path = "/dbs/" ^ dbname ^ "/users" in
      let uri = make_uri path in
      let* response =
        Http.get ~headers:(headers Utilities.Verb.Get ("dbs/" ^ dbname)) uri
        |> wrap_timeout timeout
      in
      handle_response response (fun resp body ->
          let value body = Json_converter_j.list_users_of_string body in
          result_or_error_with_result 200 value resp body)

    let get ?timeout dbname user_name =
      let path = "/dbs/" ^ dbname ^ "/users/" ^ user_name in
      let uri = make_uri path in
      let* response =
        Http.get
          ~headers:(headers Utilities.Verb.Get (header_path_of_path path))
          uri
        |> wrap_timeout timeout
      in
      handle_response response (fun resp body ->
          let value body = Json_converter_j.user_of_string body in
          result_or_error_with_result 200 value resp body)

    let replace ?timeout dbname user_name new_user_name =
      let body =
        ({ id = new_user_name } : Json_converter_j.create_user)
        |> Json_converter_j.string_of_create_user
      in
      let path = "/dbs/" ^ dbname ^ "/users/" ^ user_name in
      let uri = make_uri path in
      let* response =
        Http.put
          ~headers:(headers Utilities.Verb.Put (header_path_of_path path))
          ~body uri
        |> wrap_timeout timeout
      in
      handle_response response (fun resp body ->
          let value body = Json_converter_j.user_of_string body in
          result_or_error_with_result 200 value resp body)

    let delete ?timeout dbname user_name =
      let path = "/dbs/" ^ dbname ^ "/users/" ^ user_name in
      let uri = make_uri path in
      let* response =
        Http.delete
          ~headers:(headers Utilities.Verb.Delete (header_path_of_path path))
          uri
        |> wrap_timeout timeout
      in
      handle_response response (fun resp _body ->
          IO.return (result_or_error 204 resp))
  end

  module Permission = struct
    let resource = Account.Permissions
    let headers = headers resource

    type permission_mode = Read | All

    let string_of_permission_mode = function Read -> "Read" | All -> "All"

    let create ?timeout ~dbname ~user_name ~coll_name permission_mode
        ~permission_name =
      let permission_mode = string_of_permission_mode permission_mode in
      let resource_string =
        Printf.sprintf "/dbs/%s/colls/%s" dbname coll_name
      in
      let body =
        ({ id = permission_name; permission_mode; resource = resource_string }
          : Json_converter_j.create_permission)
        |> Json_converter_j.string_of_create_permission
      in
      let path =
        Printf.sprintf "/dbs/%s/users/%s/permissions" dbname user_name
      in
      let uri = make_uri path in
      let hdrs =
        json_headers resource Utilities.Verb.Post
          (Printf.sprintf "dbs/%s/users/%s" dbname user_name)
      in
      let* response =
        Http.post ~headers:hdrs ~body uri |> wrap_timeout timeout
      in
      handle_response response (fun resp body ->
          let value body = Json_converter_j.permission_of_string body in
          result_or_error_with_result 201 value resp body)

    let list ?timeout ~dbname ~user_name () =
      let path =
        Printf.sprintf "/dbs/%s/users/%s/permissions" dbname user_name
      in
      let uri = make_uri path in
      let* response =
        Http.get
          ~headers:
            (headers Utilities.Verb.Get
               (Printf.sprintf "dbs/%s/users/%s" dbname user_name))
          uri
        |> wrap_timeout timeout
      in
      handle_response response (fun resp body ->
          let value body = Json_converter_j.list_permissions_of_string body in
          result_or_error_with_result 200 value resp body)

    let get ?timeout ~dbname ~user_name ~permission_name () =
      let path =
        Printf.sprintf "/dbs/%s/users/%s/permissions/%s" dbname user_name
          permission_name
      in
      let uri = make_uri path in
      let* response =
        Http.get
          ~headers:(headers Utilities.Verb.Get (header_path_of_path path))
          uri
        |> wrap_timeout timeout
      in
      handle_response response (fun resp body ->
          let value body = Json_converter_j.permission_of_string body in
          result_or_error_with_result 200 value resp body)

    let replace ?timeout ~dbname ~user_name ~coll_name permission_mode
        ~permission_name =
      let permission_mode = string_of_permission_mode permission_mode in
      let resource_string =
        Printf.sprintf "/dbs/%s/colls/%s" dbname coll_name
      in
      let body =
        ({ id = permission_name; permission_mode; resource = resource_string }
          : Json_converter_j.create_permission)
        |> Json_converter_j.string_of_create_permission
      in
      let path =
        Printf.sprintf "/dbs/%s/users/%s/permissions/%s" dbname user_name
          permission_name
      in
      let uri = make_uri path in
      let* response =
        Http.put
          ~headers:(headers Utilities.Verb.Put (header_path_of_path path))
          ~body uri
        |> wrap_timeout timeout
      in
      handle_response response (fun resp body ->
          let value body = Json_converter_j.permission_of_string body in
          result_or_error_with_result 200 value resp body)

    let delete ?timeout ~dbname ~user_name ~permission_name () =
      let path =
        Printf.sprintf "/dbs/%s/users/%s/permissions/%s" dbname user_name
          permission_name
      in
      let uri = make_uri path in
      let* response =
        Http.delete
          ~headers:(headers Utilities.Verb.Delete (header_path_of_path path))
          uri
        |> wrap_timeout timeout
      in
      handle_response response (fun resp _body ->
          IO.return (result_or_error 204 resp))
  end
end

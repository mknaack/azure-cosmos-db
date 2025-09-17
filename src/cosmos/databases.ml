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
  type resource = Dbs | Colls | Docs | Users | Permissions

  val authorization :
    Utilities.Verb.t -> resource -> Utilities.Ms_time.t -> string -> string

  val endpoint : string
end

module Auth (Keys : Auth_key) : Account = struct
  type resource = Dbs | Colls | Docs | Users | Permissions

  let string_of_resource = function
    | Dbs -> "dbs"
    | Colls -> "colls"
    | Docs -> "docs"
    | Users -> "users"
    | Permissions -> "permissions"

  let authorization verb resource date db_name =
    (* "type=master&ver=1.0&sig=" ^ key *)
    let verb = Utilities.Verb.string_of_verb verb in
    (* get, post, put *)
    let resource_type = string_of_resource resource in
    (* "dbs", "colls", "docs". *)
    let resource_id = db_name in
    let date = Utilities.Ms_time.x_ms_date date in
    let result =
      Utility.authorization_token_using_master_key verb resource_type
        resource_id date Keys.master_key
    in
    result

  let endpoint = Keys.endpoint
end

let wrap_timeout timeout command =
  match timeout with
  | None -> command
  | Some t ->
      let timeout = Lwt_unix.sleep t >|= fun () -> Option.none in
      Lwt.pick [ timeout; command ]

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
    resp |> Cohttp_lwt_unix.Response.headers |> Cohttp.Header.to_list
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

let ( >>= ) = Lwt.bind
let timeout_error = Lwt.return_error Timeout_error
let connection_error = Lwt.return_error Connection_error

module Database (Auth_key : Auth_key) = struct
  module Account = Auth (Auth_key)

  let host = Utility.adjust_host Account.endpoint

  let headers resource verb db_name =
    let ms_date = Utilities.Ms_time.create_now () in
    let header = Cohttp.Header.init () in
    let header =
      Cohttp.Header.add header "authorization"
        (Account.authorization verb resource ms_date db_name)
    in
    let header = Cohttp.Header.add header "x-ms-version" "2017-02-22" in
    let header =
      Cohttp.Header.add header "x-ms-date" (Utilities.Ms_time.x_ms_date ms_date)
    in
    header

  let json_headers resource verb db_name =
    let header = headers resource verb db_name in
    let header = Cohttp.Header.add header "content_type" "application/json" in
    header

  let get_code resp =
    resp |> Cohttp_lwt_unix.Response.status |> Cohttp.Code.code_of_status

  let result_or_error_with_result expected_code f (resp : Cohttp.Response.t)
      body =
    let code = get_code resp in
    let headers = Response_headers.get_header resp in
    let r =
      if code = expected_code then
        let%lwt result = f body in
        Lwt.return_ok (expected_code, result)
      else Lwt.return_error (Azure_error (code, headers))
    in
    let%lwt () = Cohttp_lwt.Body.drain_body body in
    r

  let result_or_error expected_code resp =
    let code = get_code resp in
    if code = expected_code then Result.ok expected_code
    else
      let response_header = Response_headers.get_header resp in
      Result.error (Azure_error (code, response_header))

  let with_204_do = result_or_error 204

  let list_databases ?timeout () =
    let uri = Uri.make ~scheme:"https" ~host ~port:443 ~path:"dbs" () in
    let response =
      Cohttp_lwt_unix.Client.get
        ~headers:(headers Account.Dbs Utilities.Verb.Get "")
        uri
      >>= Lwt.return_some |> wrap_timeout timeout
    in
    match%lwt response with
    | Some (resp, body) ->
        let code = get_code resp in
        let%lwt body_string = Cohttp_lwt.Body.to_string body in
        let value = Json_converter_j.list_databases_of_string body_string in
        let%lwt () = Cohttp_lwt.Body.drain_body body in
        Lwt.return @@ Result.ok (code, value)
    | None -> timeout_error

  let create ?timeout name =
    let body =
      ({ id = name } : Json_converter_j.create_database)
      |> Json_converter_j.string_of_create_database |> Cohttp_lwt.Body.of_string
    in
    let uri = Uri.make ~scheme:"https" ~host ~port:443 ~path:"dbs" () in
    let headers = json_headers Account.Dbs Utilities.Verb.Post "" in
    let response =
      Cohttp_lwt_unix.Client.post ~headers ~body uri
      >>= Lwt.return_some |> wrap_timeout timeout
    in
    match%lwt response with
    | None -> timeout_error
    | Some (resp, body) ->
        let result body =
          let%lwt body_string = Cohttp_lwt.Body.to_string body in
          Lwt.return_some (Json_converter_j.database_of_string body_string)
        in
        result_or_error_with_result 201 result resp body

  let get ?timeout name =
    let uri =
      Uri.make ~scheme:"https" ~host ~port:443 ~path:("dbs/" ^ name) ()
    in
    let response =
      Cohttp_lwt_unix.Client.get
        ~headers:(headers Account.Dbs Utilities.Verb.Get ("dbs/" ^ name))
        uri
      >>= Lwt.return_some |> wrap_timeout timeout
    in
    match%lwt response with
    | None -> timeout_error
    | Some (resp, body) ->
        let code = get_code resp in
        let response_header = Response_headers.get_header resp in
        if code = 200 then
          let result body =
            let%lwt body_string = Cohttp_lwt.Body.to_string body in
            Lwt.return_some (Json_converter_j.database_of_string body_string)
          in
          result_or_error_with_result 200 result resp body
        else Lwt.return_error (Azure_error (code, response_header))

  let create_if_not_exists ?timeout name =
    let%lwt exists = get ?timeout name in
    match exists with
    | Ok (code, result) -> Lwt.return_ok (code, result)
    | Error (Azure_error (404, _)) -> create ?timeout name
    | Error x -> Lwt.return_error x

  let delete ?timeout name =
    let uri =
      Uri.make ~scheme:"https" ~host ~port:443 ~path:("dbs/" ^ name) ()
    in
    let response =
      Cohttp_lwt_unix.Client.delete
        ~headers:(headers Account.Dbs Utilities.Verb.Delete ("dbs/" ^ name))
        uri
      >>= Lwt.return_some |> wrap_timeout timeout
    in
    match%lwt response with
    | None -> timeout_error
    | Some (resp, body) ->
        let%lwt () = Cohttp_lwt.Body.drain_body body in
        Lwt.return (with_204_do resp)

  module Collection = struct
    let list ?timeout dbname =
      let uri =
        Uri.make ~scheme:"https" ~host ~port:443
          ~path:("dbs/" ^ dbname ^ "/colls")
          ()
      in
      let response =
        Cohttp_lwt_unix.Client.get
          ~headers:(headers Account.Colls Utilities.Verb.Get ("dbs/" ^ dbname))
          uri
        >>= Lwt.return_some |> wrap_timeout timeout
      in
      match%lwt response with
      | None -> timeout_error
      | Some (resp, body) ->
          let value body =
            let%lwt body_string = Cohttp_lwt.Body.to_string body in
            Lwt.return
            @@ Json_converter_j.list_collections_of_string body_string
          in
          result_or_error_with_result 200 value resp body

    let create ?(indexing_policy = None) ?(partition_key = None) ?timeout dbname
        coll_name =
      let body =
        ({ id = coll_name; indexing_policy; partition_key }
          : Json_converter_j.create_collection)
        |> Json_converter_j.string_of_create_collection
        |> Cohttp_lwt.Body.of_string
      in
      let uri =
        Uri.make ~scheme:"https" ~host ~port:443
          ~path:("/dbs/" ^ dbname ^ "/colls")
          ()
      in
      let headers =
        json_headers Account.Colls Utilities.Verb.Post ("dbs/" ^ dbname)
      in
      let response =
        Cohttp_lwt_unix.Client.post ~headers ~body uri
        >>= Lwt.return_some |> wrap_timeout timeout
      in
      match%lwt response with
      | None -> timeout_error
      | Some (resp, body) ->
          let value body =
            let%lwt body_string = Cohttp_lwt.Body.to_string body in
            Lwt.return_some (Json_converter_j.collection_of_string body_string)
          in
          result_or_error_with_result 201 value resp body

    let get ?timeout name coll_name =
      let path = "/dbs/" ^ name ^ "/colls/" ^ coll_name in
      let uri = Uri.make ~scheme:"https" ~host ~port:443 ~path () in
      let response =
        Cohttp_lwt_unix.Client.get
          ~headers:
            (headers Account.Colls Utilities.Verb.Get
               ("dbs/" ^ name ^ "/colls/" ^ coll_name))
          uri
        >>= Lwt.return_some |> wrap_timeout timeout
      in
      match%lwt response with
      | None -> timeout_error
      | Some (resp, body) ->
          let code = get_code resp in
          let headers = Response_headers.get_header resp in
          if code = 200 then
            let value body =
              let%lwt body_string = Cohttp_lwt.Body.to_string body in
              Lwt.return_some
                (Json_converter_j.collection_of_string body_string)
            in
            result_or_error_with_result 200 value resp body
          else Lwt.return_error (Azure_error (code, headers))

    let create_if_not_exists ?(indexing_policy = None) ?(partition_key = None)
        ?timeout dbname coll_name =
      let%lwt exists = get ?timeout dbname coll_name in
      match exists with
      | Ok result -> Lwt.return (Result.ok result)
      | Error (Azure_error (404, _)) ->
          create ?timeout ~indexing_policy ~partition_key dbname coll_name
      | Error x -> Lwt.return_error x

    let delete ?timeout name coll_name =
      let path = "/dbs/" ^ name ^ "/colls/" ^ coll_name in
      let header_path = "dbs/" ^ name ^ "/colls/" ^ coll_name in
      let uri = Uri.make ~scheme:"https" ~host ~port:443 ~path () in
      let response =
        Cohttp_lwt_unix.Client.delete
          ~headers:(headers Account.Colls Utilities.Verb.Delete header_path)
          uri
        >>= Lwt.return_some |> wrap_timeout timeout
      in
      match%lwt response with
      | None -> timeout_error
      | Some (resp, body) ->
          let%lwt () = Cohttp_lwt.Body.drain_body body in
          Lwt.return (with_204_do resp)

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
        let body = Cohttp_lwt.Body.of_string content in
        let path = "/dbs/" ^ dbname ^ "/colls/" ^ coll_name ^ "/docs" in
        let uri = Uri.make ~scheme:"https" ~host ~port:443 ~path () in
        let headers =
          json_headers Account.Docs Utilities.Verb.Post
            ("dbs/" ^ dbname ^ "/colls/" ^ coll_name)
          |> apply_to_header_if_some "x-ms-documentdb-is-upsert"
               Utility.string_of_bool is_upsert
          |> apply_to_header_if_some "x-ms-indexing-directive"
               string_of_indexing_directive indexing_directive
          |> apply_to_header_if_some "x-ms-documentdb-partitionkey"
               string_of_partition_key partition_key
        in
        let rec post retry () =
          try%lwt
            let post_respons =
              Cohttp_lwt_unix.Client.post ~headers ~body uri
              >>= Lwt.return_some |> wrap_timeout timeout
            in
            match%lwt post_respons with
            | Some (resp, body) ->
                let code = get_code resp in
                let%lwt value =
                  match code with
                  | 200 ->
                      let%lwt body_string = Cohttp_lwt.Body.to_string body in
                      Lwt.return
                        (Some
                           (Json_converter_j.collection_of_string body_string))
                  | _ -> Lwt.return None
                in
                let%lwt () = Cohttp_lwt.Body.drain_body body in
                if code = 429 then
                  let response_header = Response_headers.get_header resp in
                  let milliseconds =
                    Response_headers.x_ms_retry_after_ms response_header
                    |> Option.value ~default:"0" |> int_of_string_opt
                    |> Option.value ~default:0 |> Int.to_float
                  in
                  let%lwt () = Lwt_unix.sleep (milliseconds /. 1000.) in
                  if retry > 0 then post (retry - 1) () else timeout_error
                else Lwt.return (Result.ok (code, value))
            | None -> timeout_error
          with Unix.Unix_error (ECONNREFUSED, _, _) ->
            if retry > 0 then
              let () = Random.self_init () in
              let sleep = Random.int 5 |> float_of_int in
              let%lwt () = Lwt_unix.sleep sleep in
              post (retry - 1) ()
            else connection_error
        in
        post 10 ()

      let create_multiple ?is_upsert ?indexing_directive ?timeout
          ?(chunk_size = 100) dbname coll_name content_list =
        let rec take_first n acc = function
          | [] -> Lwt.return @@ acc
          | x ->
              let first, rest = Utilities.take_first n x in
              let%lwt r =
                Lwt_list.map_p
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
        let uri = Uri.make ~scheme:"https" ~host ~port:443 ~path () in
        let headers =
          json_headers Account.Docs Utilities.Verb.Get
            ("dbs/" ^ dbname ^ "/colls/" ^ coll_name)
          |> apply_to_header_if_some "x-ms-max-item-count" string_of_int
               max_item_count
          |> apply_to_header_if_some "x-ms-continuation"
               (fun x -> x)
               continuation
          |> apply_to_header_if_some "x-ms-consistency-level"
               (fun x -> x)
               consistency_level
          |> apply_to_header_if_some "x-ms-session-token"
               (fun x -> x)
               session_token
          |> apply_a_im_to_header_if_some "A-IM" a_im
          |> apply_to_header_if_some "If-None-Match" (fun x -> x) if_none_match
          |> apply_to_header_if_some "x-ms-documentdb-partitionkeyrangeid"
               (fun x -> x)
               partition_key_range_id
        in
        let get_action =
          Cohttp_lwt_unix.Client.get ~headers uri
          >>= Lwt.return_some |> wrap_timeout timeout
        in
        match%lwt get_action with
        | None -> timeout_error
        | Some (resp, body) ->
            let code = get_code resp in
            let response_header = Response_headers.get_header resp in
            let value body =
              let%lwt body_string = Cohttp_lwt.Body.to_string body in
              Lwt.return @@ convert_to_list_result body_string
            in
            let expected_code = 200 in
            let result =
              if code = expected_code then
                let%lwt result = value body in
                Lwt.return_ok (expected_code, response_header, result)
              else Lwt.return_error (Azure_error (code, response_header))
            in
            let%lwt () = Cohttp_lwt.Body.drain_body body in
            result

      type consistency_level = Strong | Bounded | Session | Eventual

      let string_of_consistency_level = function
        | Strong -> "Strong"
        | Bounded -> "Bounded"
        | Session -> "Session"
        | Eventual -> "Eventual"

      let get ?if_none_match ?partition_key ?consistency_level ?session_token
          ?timeout dbname coll_name doc_id =
        let headers =
          json_headers Account.Docs Utilities.Verb.Get
            ("dbs/" ^ dbname ^ "/colls/" ^ coll_name ^ "/docs/" ^ doc_id)
          |> apply_to_header_if_some "If-None-Match" (fun x -> x) if_none_match
          |> apply_to_header_if_some "x-ms-documentdb-partitionkey"
               (fun x -> x)
               partition_key
          |> apply_to_header_if_some "x-ms-consistency-level"
               string_of_consistency_level consistency_level
          |> apply_to_header_if_some "x-ms-session-token"
               (fun x -> x)
               session_token
        in
        let path =
          "/dbs/" ^ dbname ^ "/colls/" ^ coll_name ^ "/docs/" ^ doc_id
        in
        let uri = Uri.make ~scheme:"https" ~host ~port:443 ~path () in
        let response =
          Cohttp_lwt_unix.Client.get ~headers uri
          >>= Lwt.return_some |> wrap_timeout timeout
        in
        match%lwt response with
        | Some (resp, body) ->
            let code = get_code resp in
            body |> Cohttp_lwt.Body.to_string >|= fun body ->
            Result.ok (code, body)
        | None -> timeout_error

      let replace ?indexing_directive ?partition_key ?if_match ?timeout dbname
          coll_name doc_id content =
        let body = Cohttp_lwt.Body.of_string content in
        let headers =
          json_headers Account.Docs Utilities.Verb.Put
            ("dbs/" ^ dbname ^ "/colls/" ^ coll_name ^ "/docs/" ^ doc_id)
          |> apply_to_header_if_some "x-ms-indexing-directive"
               string_of_indexing_directive indexing_directive
          |> apply_to_header_if_some "x-ms-documentdb-partitionkey"
               string_of_partition_key partition_key
          |> apply_to_header_if_some "If-Match" (fun x -> x) if_match
        in
        let path =
          "/dbs/" ^ dbname ^ "/colls/" ^ coll_name ^ "/docs/" ^ doc_id
        in
        let uri = Uri.make ~scheme:"https" ~host ~port:443 ~path () in
        let response =
          Cohttp_lwt_unix.Client.put ~headers ~body uri
          >>= Lwt.return_some |> wrap_timeout timeout
        in
        match%lwt response with
        | Some (resp, body) ->
            let code = get_code resp in
            Lwt.return_ok (code, body)
        | None -> timeout_error

      let delete ?partition_key ?timeout dbname coll_name doc_id =
        let path =
          Printf.sprintf "/dbs/%s/colls/%s/docs/%s" dbname coll_name doc_id
        in
        let headers =
          Printf.sprintf "dbs/%s/colls/%s/docs/%s" dbname coll_name doc_id
          |> headers Account.Docs Utilities.Verb.Delete
          |> apply_to_header_if_some "x-ms-documentdb-partitionkey"
               string_of_partition_key partition_key
        in
        let uri = Uri.make ~scheme:"https" ~host ~port:443 ~path () in
        let rec delete retry () =
          let response =
            Cohttp_lwt_unix.Client.delete ~headers uri
            >>= Lwt.return_some |> wrap_timeout timeout
          in
          match%lwt response with
          | Some (resp, body) ->
              let code = get_code resp in
              let%lwt () = Cohttp_lwt.Body.drain_body body in
              if code = 429 then
                let response_header = Response_headers.get_header resp in
                let milliseconds =
                  Response_headers.x_ms_retry_after_ms response_header
                  |> Option.value ~default:"0" |> int_of_string_opt
                  |> Option.value ~default:0 |> Int.to_float
                in
                let%lwt () = Lwt_unix.sleep (milliseconds /. 1000.) in
                if retry > 0 then delete (retry - 1) () else timeout_error
              else Lwt.return_ok code
          | None -> timeout_error
        in
        delete 3 ()

      let delete_multiple ?partition_key ?timeout ?(chunk_size = 100) dbname
          coll_name doc_ids =
        let rec take_first n acc = function
          | [] -> Lwt.return @@ acc
          | x ->
              let first, rest = Utilities.take_first n x in
              let%lwt r =
                Lwt_list.map_p
                  (delete ?partition_key ?timeout dbname coll_name)
                  first
              in
              take_first n (r @ acc) rest
        in
        take_first chunk_size [] doc_ids

      let query ?max_item_count ?continuation ?consistency_level ?session_token
          ?is_partition ?partition_key ?timeout dbname coll_name query =
        let headers s =
          let h = headers Account.Docs Utilities.Verb.Post s in
          Cohttp.Header.add h "x-ms-documentdb-isquery"
            (Utility.string_of_bool true)
          |> apply_to_header_if_some "x-ms-max-item-count" string_of_int
               max_item_count
          |> apply_to_header_if_some "x-ms-continuation"
               (fun x -> x)
               continuation
          |> apply_to_header_if_some "x-ms-consistency-level"
               (fun x -> x)
               consistency_level
          |> apply_to_header_if_some "x-ms-session-token"
               (fun x -> x)
               session_token
          |> apply_to_header_if_some
               "x-ms-documentdb-query-enablecrosspartition"
               Utility.string_of_bool is_partition
          |> apply_to_header_if_some "x-ms-documentdb-partitionkey"
               string_of_partition_key partition_key
          |> add_header "content-type" "application/query+json"
        in
        let path = "/dbs/" ^ dbname ^ "/colls/" ^ coll_name ^ "/docs" in
        let headers = headers ("dbs/" ^ dbname ^ "/colls/" ^ coll_name) in
        let body =
          Json_converter_j.string_of_query query |> Cohttp_lwt.Body.of_string
        in
        let uri = Uri.make ~scheme:"https" ~host ~port:443 ~path () in
        let response =
          Cohttp_lwt_unix.Client.post ~headers ~body uri
          >>= Lwt.return_some |> wrap_timeout timeout
        in
        match%lwt response with
        | None -> timeout_error
        | Some (resp, body) ->
            let code = get_code resp in
            let response_header = Response_headers.get_header resp in
            let value () =
              let%lwt body_string = Cohttp_lwt.Body.to_string body in
              Lwt.return @@ convert_to_list_result body_string
            in
            let expected_code = 200 in
            let result =
              if code = expected_code then
                let%lwt result = value () in
                Lwt.return_ok (expected_code, response_header, result)
              else Lwt.return_error (Azure_error (code, response_header))
            in
            let%lwt () = Cohttp_lwt.Body.drain_body body in
            result
    end
  end

  module User = struct
    let resource = Account.Users
    let headers = headers resource

    let create ?timeout dbname user_name =
      let body =
        ({ id = user_name } : Json_converter_j.create_user)
        |> Json_converter_j.string_of_create_user |> Cohttp_lwt.Body.of_string
      in
      let uri =
        Uri.make ~scheme:"https" ~host ~port:443
          ~path:("/dbs/" ^ dbname ^ "/users")
          ()
      in
      let headers =
        json_headers resource Utilities.Verb.Post ("dbs/" ^ dbname)
      in
      let response =
        Cohttp_lwt_unix.Client.post ~headers ~body uri
        >>= Lwt.return_some |> wrap_timeout timeout
      in
      match%lwt response with
      | None -> timeout_error
      | Some (resp, body) ->
          let value body =
            let%lwt body_string = Cohttp_lwt.Body.to_string body in
            Lwt.return @@ Json_converter_j.user_of_string body_string
          in
          result_or_error_with_result 201 value resp body

    let list ?timeout dbname =
      let path = "/dbs/" ^ dbname ^ "/users" in
      let uri = Uri.make ~scheme:"https" ~host ~port:443 ~path () in
      let header_path = "dbs/" ^ dbname in
      let response =
        Cohttp_lwt_unix.Client.get
          ~headers:(headers Utilities.Verb.Get header_path)
          uri
        >>= Lwt.return_some |> wrap_timeout timeout
      in
      match%lwt response with
      | None -> timeout_error
      | Some (resp, body) ->
          let value body =
            let%lwt body_string = Cohttp_lwt.Body.to_string body in
            Lwt.return @@ Json_converter_j.list_users_of_string body_string
          in
          result_or_error_with_result 200 value resp body

    let get ?timeout dbname user_name =
      let path = "/dbs/" ^ dbname ^ "/users/" ^ user_name in
      let header_path = "dbs/" ^ dbname ^ "/users/" ^ user_name in
      let uri = Uri.make ~scheme:"https" ~host ~port:443 ~path () in
      let response =
        Cohttp_lwt_unix.Client.get
          ~headers:(headers Utilities.Verb.Get header_path)
          uri
        >>= Lwt.return_some |> wrap_timeout timeout
      in
      match%lwt response with
      | None -> timeout_error
      | Some (resp, body) ->
          let value body =
            let%lwt body_string = Cohttp_lwt.Body.to_string body in
            Lwt.return @@ Json_converter_j.user_of_string body_string
          in
          result_or_error_with_result 200 value resp body

    let replace ?timeout dbname user_name new_user_name =
      let body =
        ({ id = new_user_name } : Json_converter_j.create_user)
        |> Json_converter_j.string_of_create_user |> Cohttp_lwt.Body.of_string
      in
      let path = "/dbs/" ^ dbname ^ "/users/" ^ user_name in
      let header_path = "dbs/" ^ dbname ^ "/users/" ^ user_name in
      let uri = Uri.make ~scheme:"https" ~host ~port:443 ~path () in
      let response =
        Cohttp_lwt_unix.Client.put
          ~headers:(headers Utilities.Verb.Put header_path)
          ~body uri
        >>= Lwt.return_some |> wrap_timeout timeout
      in
      match%lwt response with
      | None -> timeout_error
      | Some (resp, body) ->
          let value body =
            let%lwt body_string = Cohttp_lwt.Body.to_string body in
            Lwt.return @@ Json_converter_j.user_of_string body_string
          in
          result_or_error_with_result 200 value resp body

    let delete ?timeout dbname user_name =
      let path = "/dbs/" ^ dbname ^ "/users/" ^ user_name in
      let header_path = "dbs/" ^ dbname ^ "/users/" ^ user_name in
      let uri = Uri.make ~scheme:"https" ~host ~port:443 ~path () in
      let response =
        Cohttp_lwt_unix.Client.delete
          ~headers:(headers Utilities.Verb.Delete header_path)
          uri
        >>= Lwt.return_some |> wrap_timeout timeout
      in
      match%lwt response with
      | None -> timeout_error
      | Some (resp, body) ->
          let%lwt () = Cohttp_lwt.Body.drain_body body in
          Lwt.return (result_or_error 204 resp)
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
        |> Cohttp_lwt.Body.of_string
      in
      let uri =
        let path =
          Printf.sprintf "/dbs/%s/users/%s/permissions" dbname user_name
        in
        Uri.make ~scheme:"https" ~host ~port:443 ~path ()
      in
      let headers =
        json_headers resource Utilities.Verb.Post
          (Printf.sprintf "dbs/%s/users/%s" dbname user_name)
      in
      let response =
        Cohttp_lwt_unix.Client.post ~headers ~body uri
        >>= Lwt.return_some |> wrap_timeout timeout
      in
      match%lwt response with
      | None -> timeout_error
      | Some (resp, body) ->
          let value body =
            let%lwt body_string = Cohttp_lwt.Body.to_string body in
            Lwt.return @@ Json_converter_j.permission_of_string body_string
          in
          result_or_error_with_result 201 value resp body

    let list ?timeout ~dbname ~user_name () =
      let path =
        Printf.sprintf "/dbs/%s/users/%s/permissions" dbname user_name
      in
      let uri = Uri.make ~scheme:"https" ~host ~port:443 ~path () in
      let header_path = Printf.sprintf "dbs/%s/users/%s" dbname user_name in

      let response =
        Cohttp_lwt_unix.Client.get
          ~headers:(headers Utilities.Verb.Get header_path)
          uri
        >>= Lwt.return_some |> wrap_timeout timeout
      in
      match%lwt response with
      | None -> timeout_error
      | Some (resp, body) ->
          let value body =
            let%lwt body_string = Cohttp_lwt.Body.to_string body in
            Lwt.return
            @@ Json_converter_j.list_permissions_of_string body_string
          in
          result_or_error_with_result 200 value resp body

    let get ?timeout ~dbname ~user_name ~permission_name () =
      let path =
        Printf.sprintf "/dbs/%s/users/%s/permissions/%s" dbname user_name
          permission_name
      in
      let header_path =
        Printf.sprintf "dbs/%s/users/%s/permissions/%s" dbname user_name
          permission_name
      in
      let uri = Uri.make ~scheme:"https" ~host ~port:443 ~path () in
      let response =
        Cohttp_lwt_unix.Client.get
          ~headers:(headers Utilities.Verb.Get header_path)
          uri
        >>= Lwt.return_some |> wrap_timeout timeout
      in
      match%lwt response with
      | None -> timeout_error
      | Some (resp, body) ->
          let value body =
            let%lwt body_string = Cohttp_lwt.Body.to_string body in
            Lwt.return @@ Json_converter_j.permission_of_string body_string
          in
          result_or_error_with_result 200 value resp body

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
        |> Cohttp_lwt.Body.of_string
      in
      let path =
        Printf.sprintf "/dbs/%s/users/%s/permissions/%s" dbname user_name
          permission_name
      in
      let header_path =
        Printf.sprintf "dbs/%s/users/%s/permissions/%s" dbname user_name
          permission_name
      in
      let uri = Uri.make ~scheme:"https" ~host ~port:443 ~path () in
      let response =
        Cohttp_lwt_unix.Client.put
          ~headers:(headers Utilities.Verb.Put header_path)
          ~body uri
        >>= Lwt.return_some |> wrap_timeout timeout
      in
      match%lwt response with
      | None -> timeout_error
      | Some (resp, body) ->
          let value body =
            let%lwt body_string = Cohttp_lwt.Body.to_string body in
            Lwt.return @@ Json_converter_j.permission_of_string body_string
          in
          result_or_error_with_result 200 value resp body
  end
end

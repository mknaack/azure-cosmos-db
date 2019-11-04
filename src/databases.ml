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

let status_of_header {Ocsigen_http_frame.Http_header.mode = mode; proto = proto; headers = headers } =
  let string_of_mode = function
    | Ocsigen_http_frame.Http_header.Query _ -> "Query"
    | Answer i -> "Status code: " ^ string_of_int i
    | Nofirstline -> ""
  in
  let string_of_proto = function
    | Ocsigen_http_frame.Http_header.HTTP10 -> "HTTP/1.0"
    | HTTP11 -> "HTTP/1.1"
  in
  let header_value_safe name =
    let string_name = Http_headers.name_to_string name in
    try
      (string_name ^ ": ") :: ((Http_headers.find_all name headers) @ [", "])
    with
    | Not_found -> ["No " ^ string_name ^ ", "]
  in
  let names = [
    Http_headers.content_length;
    Http_headers.name "x-ms-request-charge";
    Http_headers.name "x-ms-session-token";
  ]
  in
  "Protocol " ^ string_of_proto proto ^ "\n" ^
  "Mode " ^ string_of_mode mode ^ "\n" ^
  (List.fold_left (^) " " (List.flatten (List.map header_value_safe names)))

let status = function
  | { Ocsigen_http_frame.frame_content = _; frame_header = http_header; frame_abort = _ } ->
    status_of_header http_header

let content { Ocsigen_http_frame.frame_content = content; frame_header = _; frame_abort = _ } =
  match content with
  | Some v ->
    let r = Ocsigen_stream.string_of_stream 100000 (Ocsigen_stream.get v) in
    let _ = Ocsigen_stream.finalize v `Success in
    r
  | None ->
    return ""

(* list databases: *)
let convert_list_databases s =
  Json_converter_j.list_databases_of_string s

module Database (Auth_key : Auth_key) = struct
  module Account = Auth(Auth_key)

  let old_headers resource verb db_name =
    let ms_date =
      let now = Unix.time () in
      Utility.x_ms_date now
    in
    Http_headers.empty
    |> Http_headers.add (Http_headers.name "authorization") (Account.authorization verb resource ms_date db_name)
    |> Http_headers.add (Http_headers.name "x-ms-version") "2017-02-22"
    |> Http_headers.add (Http_headers.name "x-ms-date") ms_date

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

  let old_list_databases () =
    let headers = old_headers Account.Dbs Account.Get in
    let get = Ocsigen_http_client.get
        ~https:true
        ~host
        ~uri:"/dbs"
        ~headers: (headers "")
        ~port:443
        ()
    in
    get

  (* create database: *)

  let create name =
    let body =
      let value = ({id = name}: Json_converter_j.create_database) in
      let json = Json_converter_j.string_of_create_database value in
      print_endline ("json: " ^ json);
      let body = Cohttp_lwt.Body.of_string json in
      body
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

  let old_create name =
    let post_content =
      let value = ({id = name}: Json_converter_j.create_database) in
      Json_converter_j.string_of_create_database value
    in
    let content_type = "application", "json" in
    let headers = old_headers Account.Dbs Account.Post "" in
    Http_headers.iter (fun name value -> print_endline ("Header: " ^ (Http_headers.name_to_string name) ^  " : " ^ value)) headers;
    let post = Ocsigen_http_client.post_string
        ~https:true
        ~host
        ~uri:"/dbs"
        ~headers: (headers)
        ~port:443
        ~content:post_content
        ~content_type
        ()
    in
    post

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
  
  let old_get name =
    let headers = old_headers Account.Dbs Account.Get in
    let get = Ocsigen_http_client.get
        ~https:true
        ~host
        ~uri: ("/dbs/" ^ name)
        ~headers: (headers ("dbs/" ^ name))
        ~port:443
        ()
    in
    get

  let delete name =
    let uri = Uri.make ~scheme:"https" ~host ~port:443 ~path:("dbs/" ^ name) () in
    Cohttp_lwt_unix.Client.delete ~headers:(headers Account.Dbs Account.Delete ("dbs/" ^ name)) uri >>= fun (resp, body) ->
    let code = get_code resp in
    body |> Cohttp_lwt.Body.to_string >|= fun _ ->
    code
  
  let old_delete name =
    let headers = old_headers Account.Dbs Account.Delete in
    let command = Ocsigen_extra.delete
        ~https:true
        ~host
        ~uri: ("/dbs/" ^ name)
        ~headers: (headers ("dbs/" ^ name))
        ~port:443
        ()
    in
    command

  module Collection = struct
    let list dbname =
      let uri = Uri.make ~scheme:"https" ~host ~port:443 ~path:("dbs/" ^ dbname ^ "/colls")  () in
      Cohttp_lwt_unix.Client.get ~headers:(headers Account.Colls Account.Get ("dbs/" ^ dbname)) uri >>= fun (resp, body) ->
      let code = get_code resp in
      body |> Cohttp_lwt.Body.to_string >|= fun body ->
      let value = Json_converter_j.list_collections_of_string body in
      (* let value = body in *)
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

  (* let old_list dbname =
     *   let headers = old_headers Account.Colls Account.Get in
     *   Ocsigen_http_client.get
     *     ~https:true
     *     ~host
     *     ~uri:("/dbs/" ^ dbname ^ "/colls")
     *     ~headers: (headers ("dbs/" ^ dbname))
     *     ~port:443
     *     () *)

    (* let old_create dbname coll_name =
     *   let post_content =
     *     let value = ({id = coll_name; indexingPolicy = None; partitionKey = None}: Json_converter_j.create_collection) in
     *     Json_converter_j.string_of_create_collection value
     *   in
     *   let content_type = "application", "json" in
     *   let headers = old_headers Account.Colls Account.Post in
     *   let post = Ocsigen_http_client.post_string
     *       ~https:true
     *       ~host
     *       ~uri:("/dbs/" ^ dbname ^ "/colls")
     *       ~headers: (headers ("dbs/" ^ dbname))
     *       ~port:443
     *       ~content:post_content
     *       ~content_type
     *       ()
     *   in
     *   post *)

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

    (* let old_get name coll_name =
     *   let headers = old_headers Account.Colls Account.Get in
     *   let get = Ocsigen_http_client.get
     *       ~https:true
     *       ~host
     *       ~uri: ("/dbs/" ^ name ^ "/colls/" ^ coll_name)
     *       ~headers: (headers ("dbs/" ^ name^ "/colls/" ^ coll_name))
     *       ~port:443
     *       ()
     *   in
     *   get *)

  let delete name coll_name =
    let path = "/dbs/" ^ name ^ "/colls/" ^ coll_name in
    let header_path = "dbs/" ^ name ^ "/colls/" ^ coll_name in
    let uri = Uri.make ~scheme:"https" ~host ~port:443 ~path () in
    Cohttp_lwt_unix.Client.delete ~headers:(headers Account.Colls Account.Delete header_path) uri >>= fun (resp, body) ->
    let code = get_code resp in
    body |> Cohttp_lwt.Body.to_string >|= fun _ ->
    code

  
    (* let old_delete name coll_name =
     *   let headers = old_headers Account.Colls Account.Delete in
     *   let command = Ocsigen_extra.delete
     *       ~https:true
     *       ~host
     *       ~uri: ("/dbs/" ^ name ^ "/colls/" ^ coll_name)
     *       ~headers: (headers ("dbs/" ^ name ^ "/colls/" ^ coll_name))
     *       ~port:443
     *       ()
     *   in
     *   command *)

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
        | Some value -> Http_headers.add name (string_of value) headers

      let create ?is_upsert ?indexing_directive dbname coll_name content =
        let content_type = "application", "json" in
        let headers s =
          old_headers Account.Docs Account.Post s
          |> apply_to_header_if_some (Http_headers.name "x-ms-documentdb-is-upsert") Utility.string_of_bool is_upsert
          |> apply_to_header_if_some (Http_headers.name "x-ms-indexing-directive") string_of_indexing_directive indexing_directive
        in
        Ocsigen_http_client.post_string
          ~https:true
          ~host
          ~uri:("/dbs/" ^ dbname ^ "/colls/" ^ coll_name ^ "/docs")
          ~headers: (headers ("dbs/" ^ dbname ^ "/colls/" ^ coll_name))
          ~port:443
          ~content:content
          ~content_type
          ()

      let list ?max_item_count ?continuation ?consistency_level ?session_token ?a_im ?if_none_match ?partition_key_range_id dbname coll_name =
        let apply_a_im_to_header_if_some name values headers = match values with
          | None -> headers
          | Some false -> headers
          | Some true -> Http_headers.add name "Incremental feed" headers
        in
        let headers s =
          old_headers Account.Docs Account.Get s
          |> apply_to_header_if_some (Http_headers.name "x-ms-max-item-count") string_of_int max_item_count
          |> apply_to_header_if_some (Http_headers.name "x-ms-continuation") (fun x -> x) continuation
          |> apply_to_header_if_some (Http_headers.name "x-ms-consistency-level") (fun x -> x) consistency_level
          |> apply_to_header_if_some (Http_headers.name "x-ms-session-token") (fun x -> x) session_token
          |> apply_a_im_to_header_if_some (Http_headers.name "A-IM") a_im
          |> apply_to_header_if_some (Http_headers.name "If-None-Match") (fun x -> x) if_none_match
          |> apply_to_header_if_some (Http_headers.name "x-ms-documentdb-partitionkeyrangeid") (fun x -> x) partition_key_range_id
        in
        Ocsigen_http_client.get
          ~https:true
          ~host
          ~uri:("/dbs/" ^ dbname ^ "/colls/" ^ coll_name ^ "/docs")
          ~headers: (headers ("dbs/" ^ dbname ^ "/colls/" ^ coll_name))
          ~port:443
          ()

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
        let headers s =
          old_headers Account.Docs Account.Get s
          |> apply_to_header_if_some (Http_headers.name "If-None-Match") (fun x -> x) if_none_match
          |> apply_to_header_if_some (Http_headers.name "x-ms-documentdb-partitionkey") (fun x -> x) partition_key
          |> apply_to_header_if_some (Http_headers.name "x-ms-consistency-level") string_of_consistency_level consistency_level
          |> apply_to_header_if_some (Http_headers.name "x-ms-session-token") (fun x -> x) session_token
        in
        let get = Ocsigen_http_client.get
            ~https:true
            ~host
            ~uri: ("/dbs/" ^ dbname ^ "/colls/" ^ coll_name ^ "/docs/" ^ doc_id)
            ~headers: (headers ("dbs/" ^ dbname ^ "/colls/" ^ coll_name ^ "/docs/" ^ doc_id))
            ~port:443
            ()
        in
        get

      let replace ?indexing_directive ?partition_key ?if_match dbname coll_name doc_id content =
        let content_type = "application", "json" in
        let headers s =
          old_headers Account.Docs Account.Put s
          |> apply_to_header_if_some (Http_headers.name "x-ms-indexing-directive") string_of_indexing_directive indexing_directive
          |> apply_to_header_if_some (Http_headers.name "x-ms-documentdb-partitionkey") (fun x -> x) partition_key
          |> apply_to_header_if_some (Http_headers.name "If-Match") (fun x -> x) if_match
        in
        Ocsigen_extra.put_string
          ~https:true
          ~host
          ~uri: ("/dbs/" ^ dbname ^ "/colls/" ^ coll_name ^ "/docs/" ^ doc_id)
          ~headers: (headers ("dbs/" ^ dbname ^ "/colls/" ^ coll_name ^ "/docs/" ^ doc_id))
          ~port:443
          ~content
          ~content_type
          ()

      let delete dbname coll_name doc_id =
        let headers = old_headers Account.Docs Account.Delete in
        Ocsigen_extra.delete
          ~https:true
          ~host
          ~uri: ("/dbs/" ^ dbname ^ "/colls/" ^ coll_name ^ "/docs/" ^ doc_id)
          ~headers: (headers ("dbs/" ^ dbname ^ "/colls/" ^ coll_name ^ "/docs/" ^ doc_id))
          ~port:443
          ()

      let query ?max_item_count ?continuation ?consistency_level ?session_token ?is_partition dbname coll_name query =
        let headers s =
          old_headers Account.Docs Account.Post s
          |> Http_headers.add (Http_headers.name "x-ms-documentdb-isquery") (Utility.string_of_bool true)
          |> apply_to_header_if_some (Http_headers.name "x-ms-max-item-count") string_of_int max_item_count
          |> apply_to_header_if_some (Http_headers.name "x-ms-continuation") (fun x -> x) continuation
          |> apply_to_header_if_some (Http_headers.name "x-ms-consistency-level") (fun x -> x) consistency_level
          |> apply_to_header_if_some (Http_headers.name "x-ms-session-token") (fun x -> x) session_token
          |> apply_to_header_if_some (Http_headers.name "x-ms-documentdb-query-enablecrosspartition") Utility.string_of_bool is_partition
        in
        let content =   Json_converter_j.string_of_query query in
        let content_type = "application", "query+json" in
        Ocsigen_http_client.post_string
          ~https:true
          ~host
          ~uri:("/dbs/" ^ dbname ^ "/colls/" ^ coll_name ^ "/docs")
          ~headers: (headers ("dbs/" ^ dbname ^ "/colls/" ^ coll_name))
          ~port:443
          ~content:content
          ~content_type
          ()

    end
  end
end

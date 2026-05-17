let make_response ?(status = 200) ?(headers = Cohttp.Header.init ()) body =
  let response =
    Cohttp.Response.make ~status:(Cohttp.Code.status_of_code status) ~headers ()
  in
  (response, body)

let database_response ~id ~_rid () =
  Printf.sprintf
    {|{"id": "%s", "_rid": "%s", "_self": "dbs/%s/", 
    "_etag": "\"00000000-0000-0000-0000-000000000001\"", 
    "_ts": 1234567890, "_colls": "colls/", "_users": "users/"}|}
    id _rid _rid

let collection_response ~id ~_rid ~partition_key () =
  Printf.sprintf
    {|{"id": "%s", "_rid": "%s", "_self": "dbs/db/colls/%s/",
    "_etag": "\"00000000-0000-0000-0000-000000000001\"",
    "_ts": 1234567890,
    "partitionKey": {"paths": ["/%s"], "kind": "Hash"},
    "_conflicts": "conflicts/", "_docs": "docs/", "_sprocs": "sprocs/",
    "_triggers": "triggers/", "_udfs": "udfs/"}|}
    id _rid id partition_key

let document_response ~id ~_rid ~json () =
  let inner =
    if String.length json > 0 then String.sub json 1 (String.length json - 2)
    else ""
  in
  let attachments_field =
    if String.length inner > 0 then
      Printf.sprintf {|"_attachments": "attachments/", %s|} inner
    else {|"_attachments": "attachments/"|}
  in
  Printf.sprintf
    {|{"id": "%s", "_rid": "%s", "_self": "dbs/db/colls/coll/docs/%s/",
    "_etag": "\"00000000-0000-0000-0000-000000000001\"",
    "_ts": 1234567890,
    %s}|}
    id _rid id attachments_field

let list_databases_response dbs =
  let db_entries =
    List.map
      (fun (id, _rid) ->
        Printf.sprintf
          {|{"id": "%s", "_rid": "%s", "_self": "dbs/%s/",
        "_etag": "\"00000000-0000-0000-0000-000000000001\"",
        "_ts": 1234567890, "_colls": "colls/", "_users": "users/"}|}
          id _rid _rid)
      dbs
  in
  Printf.sprintf {|{"_rid": "", "Databases": [%s], "_count": %d}|}
    (String.concat "," db_entries)
    (List.length dbs)

let list_collections_response colls =
  let coll_entries =
    List.map
      (fun (id, _rid, partition_key) ->
        Printf.sprintf
          {|{"id": "%s", "_rid": "%s", "_self": "dbs/db/colls/%s/",
        "_etag": "\"00000000-0000-0000-0000-000000000001\"",
        "_ts": 1234567890,
        "partitionKey": {"paths": ["/%s"], "kind": "Hash"},
        "_conflicts": "conflicts/", "_docs": "docs/", "_sprocs": "sprocs/",
        "_triggers": "triggers/", "_udfs": "udfs/"}|}
          id _rid id partition_key)
      colls
  in
  Printf.sprintf {|{"_rid": "db", "DocumentCollections": [%s], "_count": %d}|}
    (String.concat "," coll_entries)
    (List.length colls)

let list_documents_response docs =
  let doc_entries =
    List.map
      (fun (id, _rid) ->
        Printf.sprintf
          {|{"id": "%s", "_rid": "%s", "_self": "dbs/db/colls/coll/docs/%s/",
        "_etag": "\"00000000-0000-0000-0000-000000000001\"",
        "_ts": 1234567890, "_attachments": "attachments/"}|}
          id _rid id)
      docs
  in
  Printf.sprintf {|{"_rid": "coll", "Documents": [%s], "_count": %d}|}
    (String.concat "," doc_entries)
    (List.length docs)

let error_response ~code ~message =
  let body = Printf.sprintf {|{"code": "%d", "message": "%s"}|} code message in
  let headers = Cohttp.Header.init () in
  let response =
    Cohttp.Response.make ~status:(Cohttp.Code.status_of_code code) ~headers ()
  in
  (response, body)

let throttled_response ~retry_after_ms =
  let body = "" in
  let headers =
    Cohttp.Header.of_list
      [ ("x-ms-retry-after-ms", string_of_int retry_after_ms) ]
  in
  let response =
    Cohttp.Response.make ~status:(Cohttp.Code.status_of_code 429) ~headers ()
  in
  (response, body)

let empty_body_response ~status =
  let response =
    Cohttp.Response.make
      ~status:(Cohttp.Code.status_of_code status)
      ~headers:(Cohttp.Header.init ()) ()
  in
  (response, "")

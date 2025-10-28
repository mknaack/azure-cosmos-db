open Cosmos
open Databases
open Json_j
open Test_common

let create_value counter =
  let string_counter = string_of_int counter in
  ({
     id = document_id ^ string_counter;
     firstName = "A First name " ^ string_counter;
     lastName = "a Last name";
   }
    : create_document)
  |> string_of_create_document

let replace_value counter =
  let string_counter = string_of_int counter in
  ({
     id = document_id ^ string_counter;
     firstName = "Something different";
     lastName = "a Last name";
   }
    : create_document)
  |> string_of_create_document

let create_database_test _ () =
  let%lwt res = D.create dbname in
  match res with
  | Result.Ok (code, body) ->
      let _ = Alcotest.(check int) "Status same int" 201 code in
      let _ =
        match body with
        | Some { id; _ } ->
            Alcotest.(check string) "Create name is correct" dbname id
        | None -> ()
      in
      Lwt.return_unit
  | Result.Error _ -> Alcotest.fail "Should not return error"

let check_result f = function
  | Result.Ok result -> f result
  | Result.Error _ -> Alcotest.fail "Should not return error"

let create_database_if_not_exists_test _ () =
  let%lwt res = D.create_if_not_exists dbname in
  match res with
  | Result.Ok (code, body) ->
      let _ = Alcotest.(check int) "Status same int" 200 code in
      let _ =
        match body with
        | Some { id; _ } ->
            Alcotest.(check string) "Create name is correct" dbname id
        | None -> ()
      in
      Lwt.return_unit
  | Result.Error _ -> Alcotest.fail "Should not return error"

let list_databases _ () =
  let%lwt res = D.list_databases () in
  match res with
  | Result.Ok (code, { _rid; databases; _count = count }) ->
      let db =
        List.filter
          (fun (x : Json_converter_t.database) -> x.id = dbname)
          databases
      in
      let _ = Alcotest.(check int) "Status same int" 200 code in
      let _ = Alcotest.(check bool) "Count" true (count > 0) in
      let _ =
        Alcotest.(check string) "Name of databases" dbname (List.hd db).id
      in
      Lwt.return_unit
  | Result.Error _ -> Alcotest.fail "Should not return error"

let list_databases_timeout_test _ () =
  let%lwt res = D.list_databases ~timeout:0.0 () in
  match res with
  | Result.Error Timeout_error ->
      Alcotest.(check unit) "Timeout error" () ();
      Lwt.return_unit
  | Result.Error _ -> Alcotest.fail "Should fail with timeout error"
  | Result.Ok _ -> Alcotest.fail "Should fail with timeout error"

(* let list_databases _ () =
   let res = D.list_databases () in
   res >>= fun (code, {_rid; databases; _count = count}) ->
   let db = List.filter (fun (x : Json_converter_t.database)-> x.id = dbname) databases in
   let _ = Alcotest.(check int) "Status same int" 200 code in
   let _ = Alcotest.(check bool) "Count" true (count > 0) in
   let _ = Alcotest.(check string) "Name of databases" dbname ((List.hd db).id) in
   return () *)

let get_database_test _ () =
  let%lwt res = D.get dbname in
  match res with
  | Result.Ok (code, body) ->
      let _ = Alcotest.(check int) "Status same int" 200 code in
      let _ =
        match body with
        | Some { _rid; id; _self; _etag; _colls; _users; _ts } ->
            Alcotest.(check string) "Name of database" dbname id
        | None -> ()
      in
      Lwt.return_unit
  | Result.Error _ -> Alcotest.fail "Should not return error"

let get_database_timeout_test _ () =
  let%lwt res = D.get ~timeout:0.0 dbname in
  match res with
  | Result.Error Timeout_error ->
      Alcotest.(check unit) "Timeout error" () ();
      Lwt.return_unit
  | Result.Error _ -> Alcotest.fail "Should fail with timeout error"
  | Result.Ok _ -> Alcotest.fail "Should fail with timeout error"

let create_collection_test _ () =
  let%lwt res = D.Collection.create dbname collection_name in
  match res with
  | Result.Ok (code, body) ->
      let _ = Alcotest.(check int) "Status same int" 201 code in
      let _ =
        match body with
        | Some { id; _ } ->
            Alcotest.(check string) "Create name is correct" collection_name id
        | None -> ()
      in
      Lwt.return_unit
  | Result.Error _ -> Alcotest.fail "Should not return error"

let create_collection_if_not_exists_test _ () =
  let%lwt res = D.Collection.create_if_not_exists dbname collection_name in
  match res with
  | Result.Ok (code, body) ->
      let _ = Alcotest.(check int) "Status same int" 200 code in
      let _ =
        match body with
        | Some { id; _ } ->
            Alcotest.(check string) "Create name is correct" collection_name id
        | None -> ()
      in
      Lwt.return_unit
  | Result.Error _ -> Alcotest.fail "Should not return error"

let create_collection_if_not_exists_test_new_collection _ () =
  let collection_name_test = collection_name ^ "test_collection" in
  let%lwt () =
    match%lwt D.Collection.create_if_not_exists dbname collection_name_test with
    | Result.Ok (code, body) ->
        let _ =
          let expected = code = 200 || code = 201 in
          Alcotest.(check' bool) ~msg:"Status same int" ~expected ~actual:true
        in
        let _ =
          match body with
          | Some { id; _ } ->
              Alcotest.(check string)
                "Create name is correct" collection_name_test id
          | None -> ()
        in
        Lwt.return_unit
    | Result.Error _ -> Alcotest.fail "Should not return error"
  in
  match%lwt D.Collection.delete dbname collection_name_test with
  | Result.Error _ -> Alcotest.fail "Should not return error"
  | Result.Ok code ->
      let _ = Alcotest.(check int) "Status same int" 204 code in
      Lwt.return_unit

let list_collection_test _ () =
  let%lwt res = D.Collection.list dbname in
  match res with
  | Result.Ok (code, { rid = _; document_collections = _; count }) ->
      let _ = Alcotest.(check int) "Status same int" 200 code in
      let _ = Alcotest.(check int) "Count" 1 count in
      Lwt.return_unit
  | Result.Error _ -> Alcotest.fail "Should not return error"

let get_collection_test _ () =
  let%lwt res = D.Collection.get dbname collection_name in
  match res with
  | Result.Error _ -> Alcotest.fail "Should not return error"
  | Result.Ok (code, body) ->
      let _ = Alcotest.(check int) "Status same int" 200 code in
      let _ =
        match body with
        | Some
            Json_converter_t.
              {
                id;
                rid = _;
                self = _;
                etag = _;
                ts = _;
                sprocs = _;
                triggers = _;
                docs = _;
                udfs = _;
                conflicts = _;
                indexing_policy = _;
                partition_key = _;
              } ->
            Alcotest.(check string) "Name of database" collection_name id
        | None -> ()
      in
      Lwt.return_unit

let create_document_test _ () =
  let%lwt res =
    D.Collection.Document.create dbname collection_name (create_value 1)
  in
  match res with
  | Result.Error _ -> Alcotest.fail "Should not return error"
  | Result.Ok (code, _) ->
      let _ = Alcotest.(check int) "Status same int" 201 code in
      Lwt.return_unit

let list_document_test _ () =
  let%lwt res = D.Collection.Document.list dbname collection_name in
  match res with
  | Result.Error _ -> Alcotest.fail "Should not return error"
  | Result.Ok (code, headers, { rid = _; documents; count }) ->
      let _ = Alcotest.(check int) "Status same int" 200 code in
      (* let header_none = Alcotest.(check (option string)) in *)
      let _ =
        let open Alcotest in
        (check (option string))
          "Continuation" None
          (Response_headers.x_ms_continuation headers);
        (check (option string))
          "content_type" (Some "application/json")
          (Response_headers.content_type headers);
        (check bool) "date" true
          (Option.is_some @@ Response_headers.date headers);
        (check bool) "x_ms_activity_id" true
          (Option.is_some @@ Response_headers.x_ms_activity_id headers);
        (check bool) "x_ms_alt_content_path" true
          (Option.is_some @@ Response_headers.x_ms_alt_content_path headers);
        (check bool) "x_ms_item_count" true
          (Option.is_some @@ Response_headers.x_ms_item_count headers);
        (check bool) "x_ms_request_charge" true
          (Option.is_some @@ Response_headers.x_ms_request_charge headers);
        (check bool) "x_ms_resource_quota" true
          (Option.is_some @@ Response_headers.x_ms_resource_quota headers);
        (check bool) "x_ms_resource_usage" true
          (Option.is_some @@ Response_headers.x_ms_resource_usage headers);
        (check bool) "x_ms_schemaversion" true
          (Option.is_some @@ Response_headers.x_ms_schemaversion headers);
        (check bool) "x_ms_serviceversion" true
          (Option.is_some @@ Response_headers.x_ms_serviceversion headers);
        (check bool) "x_ms_session_token" true
          (Option.is_some @@ Response_headers.x_ms_session_token headers)
      in

      let _ =
        let docs =
          List.map (fun (x, _) -> create_document_of_string x) documents
        in
        let { id; firstName; lastName } = List.hd docs in
        Alcotest.(check int) "Count field" count 1;
        Alcotest.(check int) "Count list" count (List.length documents);
        Alcotest.(check string) "id" id "document_id1";
        Alcotest.(check string) "firstName" firstName "A First name 1";
        Alcotest.(check string) "lastName" lastName "a Last name"
      in
      Lwt.return_unit

let range i j =
  let rec loop acc k = if i = k then k :: acc else loop (k :: acc) (pred k) in
  loop [] j

let create_a_lot_of_documents_test _ () =
  let ids = range 21 100 in
  let%lwt result_list =
    Lwt_list.map_p
      (fun id ->
        let res =
          D.Collection.Document.create dbname collection_name (create_value id)
        in
        res)
      ids
  in
  let check expected_code = function
    | Result.Ok (code, _) -> code = expected_code
    | _ -> false
  in
  let results_length = List.filter (check 201) result_list |> List.length in
  let length_429 = List.filter (check 429) result_list |> List.length in
  let _ = Alcotest.(check int) "Documents that was rejected" 0 length_429 in
  let _ =
    Alcotest.(check int)
      "All documents should be created full list" (List.length ids)
      (List.length result_list)
  in
  let _ =
    Alcotest.(check int)
      "All documents should be created succesfully" (List.length ids)
      results_length
  in
  Lwt.return_unit

let list_multiple_documents_test _ () =
  let list_values =
    let rec make_values i acc =
      if i <= 1 then acc else make_values (i - 1) (i :: acc)
    in
    make_values 10 []
  in
  let values = List.map create_value list_values in
  let%lwt x =
    Lwt_list.map_p
      (fun x -> D.Collection.Document.create dbname collection_name x)
      values
  in
  let is_201 x = function Result.Ok (y, _) -> y = 201 && x | _ -> false in
  let all_is_inserted = List.fold_left is_201 true x in
  (* let all_is_inserted = List.fold_left (fun x (y, _) -> y = 201 && x) true x in *)
  let _ = Alcotest.(check bool) "all_is_inserted" true all_is_inserted in
  let%lwt res =
    D.Collection.Document.list ~max_item_count:5 dbname collection_name
  in
  match res with
  | Result.Error _ -> Alcotest.fail "Should not return error"
  | Result.Ok (code, headers, { rid = _; documents = _; count }) -> (
      let _ = Alcotest.(check int) "Status same int" 200 code in
      let continuation =
        Option.get (Response_headers.x_ms_continuation headers)
      in
      let%lwt res2 =
        D.Collection.Document.list ~max_item_count:5 ~continuation dbname
          collection_name
      in
      match res2 with
      | Result.Error _ -> Alcotest.fail "Should not return error"
      | Result.Ok (code2, headers2, { rid = _; documents = _; count = count2 })
        ->
          let _ = Alcotest.(check int) "Status same int" 200 code2 in
          let _ =
            Alcotest.(check (option string))
              "Continuation" None
              (Response_headers.x_ms_continuation headers2)
          in
          let _ = Alcotest.(check int) "Count field" (count + count2) 10 in
          Lwt.return_unit)

let change_feed_test _ () =
  (* list all documents *)
  let%lwt res = D.Collection.Document.list ~a_im:true dbname collection_name in
  match res with
  | Result.Error _ -> Alcotest.fail "Should not return error"
  | Result.Ok (code, headers, _result) -> (
      let _ = Alcotest.(check int) "Status same int" 200 code in

      (* list all documents again, none returned *)
      let if_none_match = Option.get @@ Response_headers.etag headers in
      let%lwt res =
        D.Collection.Document.list ~a_im:true ~if_none_match dbname
          collection_name
      in
      match res with
      | Result.Error Timeout_error ->
          Alcotest.fail "1 Should not return Timeout_error"
      | Result.Error Connection_error ->
          Alcotest.fail "Should not fail with Connection_error"
      | Result.Ok (code, _headers, _result) ->
          Alcotest.fail
            ("1 Should not return error code : " ^ string_of_int code)
      | Result.Error (Azure_error (code, _headers)) -> (
          let _ = Alcotest.(check int) "Status same int" 304 code in

          (* insert record *)
          let%lwt res =
            D.Collection.Document.create dbname collection_name
              (create_value 20)
          in
          match res with
          | Result.Error Timeout_error ->
              Alcotest.fail "2 Should not return Timeout_error"
          | Result.Error Connection_error ->
              Alcotest.fail "Should not fail with Connection_error"
          | Result.Error (Azure_error (error_code, _)) ->
              Alcotest.fail
                ("2 Should not return error code : " ^ string_of_int error_code)
          | Result.Ok (code, _) -> (
              let _ = Alcotest.(check int) "Status same int" 201 code in

              (* list all documents, one returned *)
              let%lwt res =
                D.Collection.Document.list ~a_im:true ~if_none_match dbname
                  collection_name
              in
              match res with
              | Result.Error Timeout_error ->
                  Alcotest.fail "3 Should not return Timeout_error"
              | Result.Error Connection_error ->
                  Alcotest.fail "Should not fail with Connection_error"
              | Result.Error (Azure_error (error_code, _)) ->
                  Alcotest.fail
                    ("3 Should not return error code : "
                   ^ string_of_int error_code)
              | Result.Ok (code, _headers, _result) ->
                  let _ = Alcotest.(check int) "Status same int" 200 code in
                  Lwt.return_unit)))

let query_document_test _ () =
  let query =
    Json_converter_t.
      {
        query =
          "SELECT * FROM " ^ collection_name ^ " f WHERE f.firstName = @fname";
        parameters = [ { name = "@fname"; value = "A First name 1" } ];
      }
  in
  let%lwt res = D.Collection.Document.query dbname collection_name query in
  match res with
  | Result.Error _ -> Alcotest.fail "Should not return error"
  | Result.Ok (code, _, { rid = _; documents; count }) ->
      let _ = Alcotest.(check int) "Status same int" 200 code in
      let _ =
        let docs =
          List.map (fun (x, _) -> create_document_of_string x) documents
        in
        let { id; firstName; lastName } = List.hd docs in
        Alcotest.(check int) "Count field" count 1;
        Alcotest.(check int) "Count list" count (List.length documents);
        Alcotest.(check string) "id" id "document_id1";
        Alcotest.(check string) "firstName" firstName "A First name 1";
        Alcotest.(check string) "lastName" lastName "a Last name"
      in
      Lwt.return_unit

let query_document_count_test _ () =
  let query =
    Json_converter_t.{ query = "SELECT VALUE COUNT(1) FROM f"; parameters = [] }
  in
  let%lwt res = D.Collection.Document.query dbname collection_name query in
  match res with
  | Result.Error _ -> Alcotest.fail "Should not return error"
  | Result.Ok (code, _, { rid = _; documents; count }) ->
      let _ = Alcotest.(check int) "Status same int" 200 code in
      let _ =
        Alcotest.(check int) "Count field" count 1;
        let docs = List.map (fun (x, _) -> x) documents in
        let first = List.hd docs in
        let value_count = int_of_string first in
        Alcotest.(check int) "Value count field" value_count 11
      in
      Lwt.return_unit

let get_document_test _ () =
  let%lwt res =
    D.Collection.Document.get dbname collection_name (document_id ^ "1")
  in
  match res with
  | Result.Error _ -> Alcotest.fail "Should not return error"
  | Result.Ok (code, _) ->
      let _ = Alcotest.(check int) "Status same int" 200 code in
      Lwt.return_unit

let replace_document_test _ () =
  let%lwt res =
    D.Collection.Document.replace dbname collection_name (document_id ^ "1")
      (replace_value 1)
  in
  match res with
  | Result.Error _ -> Alcotest.fail "Should not return error"
  | Result.Ok (code, _) ->
      let _ = Alcotest.(check int) "Status same int" 200 code in
      Lwt.return_unit

let upsert_document_test _ () =
  let%lwt res =
    D.Collection.Document.create ~is_upsert:true dbname collection_name
      (create_value 1)
  in
  match res with
  | Result.Error _ -> Alcotest.fail "Should not return error"
  | Result.Ok (code, _) ->
      let _ = Alcotest.(check int) "Status same int" 200 code in
      Lwt.return_unit

let delete_document_test _ () =
  let%lwt res =
    D.Collection.Document.delete dbname collection_name (document_id ^ "1")
  in
  match res with
  | Result.Error _ -> Alcotest.fail "Should not return error"
  | Result.Ok code ->
      let _ = Alcotest.(check int) "Status same int" 204 code in
      Lwt.return_unit

let delete_collection_test _ () =
  let%lwt res = D.Collection.delete dbname collection_name in
  match res with
  | Result.Error _ -> Alcotest.fail "Should not return error"
  | Result.Ok code ->
      let _ = Alcotest.(check int) "Status same int" 204 code in
      Lwt.return_unit

let delete_database_test _ () =
  let%lwt res = D.delete dbname in
  match res with
  | Result.Error _ -> Alcotest.fail "Should not return error"
  | Result.Ok code ->
      let _ = Alcotest.(check int) "Status same int" 204 code in
      Lwt.return_unit

let create_database_if_not_exists_test_new_database _ () =
  let test_name = dbname ^ "create_new" in
  let%lwt () =
    match%lwt D.create_if_not_exists test_name with
    | Result.Ok (code, body) ->
        let _ =
          let expected = code = 200 || code = 201 in
          Alcotest.(check' bool) ~msg:"Status same int" ~expected ~actual:true
        in
        let _ =
          match body with
          | Some { id; _ } ->
              Alcotest.(check string) "Create name is correct" test_name id
          | None -> ()
        in
        Lwt.return_unit
    | Result.Error _ -> Alcotest.fail "Should not return error"
  in
  match%lwt D.delete test_name with
  | Result.Error _ -> Alcotest.fail "Should not return error"
  | Result.Ok code ->
      let _ = Alcotest.(check int) "Status same int" 204 code in
      Lwt.return_unit

let cosmos_test =
  [
    Alcotest_lwt.test_case "create database" `Slow create_database_test;
    Alcotest_lwt.test_case "create database if not exists" `Slow
      create_database_if_not_exists_test;
    Alcotest_lwt.test_case "list database" `Slow list_databases;
    Alcotest_lwt.test_case "list database timeout" `Slow
      list_databases_timeout_test;
    Alcotest_lwt.test_case "get database" `Slow get_database_test;
    Alcotest_lwt.test_case "get database timeout" `Slow
      get_database_timeout_test;
    Alcotest_lwt.test_case "create collection" `Slow create_collection_test;
    Alcotest_lwt.test_case "create collection if not exists" `Slow
      create_collection_if_not_exists_test;
    Alcotest_lwt.test_case "create collection if not exists with new collection"
      `Slow create_collection_if_not_exists_test_new_collection;
    Alcotest_lwt.test_case "list collection" `Slow list_collection_test;
    Alcotest_lwt.test_case "get collection" `Slow get_collection_test;
    Alcotest_lwt.test_case "create document" `Slow create_document_test;
    Alcotest_lwt.test_case "list document" `Slow list_document_test;
    Alcotest_lwt.test_case "list multiple documents" `Slow
      list_multiple_documents_test;
    Alcotest_lwt.test_case "change feed" `Slow change_feed_test;
    Alcotest_lwt.test_case "query document" `Slow query_document_test;
    Alcotest_lwt.test_case "query document count" `Slow
      query_document_count_test;
    Alcotest_lwt.test_case "get document" `Slow get_document_test;
    Alcotest_lwt.test_case "replace document" `Slow replace_document_test;
    Alcotest_lwt.test_case "upsert document" `Slow upsert_document_test;
    Alcotest_lwt.test_case "create a lot document test" `Slow
      create_a_lot_of_documents_test;
    Alcotest_lwt.test_case "delete document" `Slow delete_document_test;
    Alcotest_lwt.test_case "delete collection" `Slow delete_collection_test;
    Alcotest_lwt.test_case "delete database" `Slow delete_database_test;
    Alcotest_lwt.test_case "create and delete database" `Slow
      create_database_if_not_exists_test_new_database;
  ]

let test = if should_run () then cosmos_test else []

let create_database_with_partition_key_test _ () =
  let%lwt res = D.create dbname_partition in
  match res with
  | Result.Error _ -> Alcotest.fail "Should not return error"
  | Result.Ok (code, body) ->
      let _ = Alcotest.(check int) "Status same int" 201 code in
      let _ =
        match body with
        | Some { id; _ } ->
            Alcotest.(check string) "Create name is correct" dbname_partition id
        | None -> ()
      in
      Lwt.return_unit

let create_collection_with_partition_key_test _ () =
  let partition_key =
    Some
      Json_converter_t.
        { paths = [ "/lastName" ]; kind = "Hash"; version = None }
  in
  let%lwt res =
    D.Collection.create ~partition_key dbname_partition
      collection_name_partition
  in
  match res with
  | Result.Error _ -> Alcotest.fail "Should not return error"
  | Result.Ok (code, body) ->
      let _ = Alcotest.(check int) "Status same int" 201 code in
      let _ =
        match body with
        | Some { id; _ } ->
            Alcotest.(check string)
              "Create name is correct" collection_name_partition id
        | None -> ()
      in
      Lwt.return_unit

let create_document_with_partition_key_test _ () =
  let%lwt res =
    D.Collection.Document.create ~partition_key:"a Last name" dbname_partition
      collection_name_partition (create_value 1)
  in
  match res with
  | Result.Error _ -> Alcotest.fail "Should not return error"
  | Result.Ok (code, _) ->
      let _ = Alcotest.(check int) "Status same int" 201 code in
      Lwt.return_unit

let create_document_with_partition_key_include_index_test _ () =
  let%lwt res =
    D.Collection.Document.create ~partition_key:"a Last name"
      ~indexing_directive:D.Collection.Document.Include dbname_partition
      collection_name_partition (create_value 2)
  in
  match res with
  | Result.Error _ -> Alcotest.fail "Should not return error"
  | Result.Ok (code, _) ->
      let _ = Alcotest.(check int) "Status same int" 201 code in
      Lwt.return_unit

let create_document_with_partition_key_exclude_index_test _ () =
  let%lwt res =
    D.Collection.Document.create ~partition_key:"a Last name"
      ~indexing_directive:D.Collection.Document.Exclude dbname_partition
      collection_name_partition (create_value 3)
  in
  match res with
  | Result.Error _ -> Alcotest.fail "Should not return error"
  | Result.Ok (code, _) ->
      let _ = Alcotest.(check int) "Status same int" 201 code in
      Lwt.return_unit

let replace_document_with_partition_key_test _ () =
  let%lwt res =
    D.Collection.Document.replace ~partition_key:"a Last name" dbname_partition
      collection_name_partition (document_id ^ "1") (replace_value 1)
  in
  match res with
  | Result.Error _ -> Alcotest.fail "Should not return error"
  | Result.Ok (code, _) ->
      let _ = Alcotest.(check int) "Status same int" 200 code in
      Lwt.return_unit

let upsert_document_with_partition_key_test _ () =
  let%lwt res =
    D.Collection.Document.create ~is_upsert:true ~partition_key:"a Last name"
      dbname_partition collection_name_partition (create_value 1)
  in
  match res with
  | Result.Error _ -> Alcotest.fail "Should not return error"
  | Result.Ok (code, _) ->
      let _ = Alcotest.(check int) "Status same int" 200 code in
      Lwt.return_unit

let query_document_with_partition_key_test _ () =
  let query =
    Json_converter_t.
      {
        query =
          "SELECT * FROM " ^ collection_name ^ " f WHERE f.firstName = @fname";
        parameters = [ { name = "@fname"; value = "A First name 1" } ];
      }
  in
  let%lwt res =
    D.Collection.Document.query ~is_partition:true dbname_partition
      collection_name_partition query
  in
  match res with
  | Result.Error _ -> Alcotest.fail "Should not return error"
  | Result.Ok (code, _, { rid = _; documents; count }) ->
      let _ = Alcotest.(check int) "Status same int" 200 code in
      let _ =
        let docs =
          List.map (fun (x, _) -> create_document_of_string x) documents
        in
        let { id; firstName; lastName } = List.hd docs in
        Alcotest.(check int) "Count field" count 1;
        Alcotest.(check int) "Count list" count (List.length documents);
        Alcotest.(check string) "id" id "document_id1";
        Alcotest.(check string) "firstName" firstName "A First name 1";
        Alcotest.(check string) "lastName" lastName "a Last name"
      in
      Lwt.return_unit

let query_document_count_without_partition_key_test _ () =
  (* Should fail due to the partition *)
  let query =
    Json_converter_t.{ query = "SELECT VALUE COUNT(1) FROM f"; parameters = [] }
  in
  let%lwt res =
    D.Collection.Document.query ~is_partition:true dbname_partition
      collection_name_partition query
  in
  match res with
  | Result.Ok _ | Result.Error Timeout_error | Result.Error Connection_error ->
      Alcotest.fail "Should not return error"
  | Result.Error (Azure_error (code, _)) ->
      let _ = Alcotest.(check int) "Status same int" 400 code in
      Lwt.return_unit

let query_document_count_with_partition_key_test _ () =
  let query =
    Json_converter_t.{ query = "SELECT VALUE COUNT(1) FROM f"; parameters = [] }
  in
  let%lwt res =
    D.Collection.Document.query ~partition_key:"a Last name" ~is_partition:true
      dbname_partition collection_name_partition query
  in
  match res with
  | Result.Error _ -> Alcotest.fail "Should not return error"
  | Result.Ok (code, _, { rid = _; documents; count }) ->
      let _ = Alcotest.(check int) "Status same int" 200 code in
      let _ =
        Alcotest.(check int) "Count field" count 1;
        let docs = List.map (fun (x, _) -> x) documents in
        let first = List.hd docs in
        let value_count = int_of_string first in
        Alcotest.(check int) "Value count field" value_count 3
      in
      Lwt.return_unit

let delete_document_with_partition_key_test _ () =
  let%lwt res =
    D.Collection.Document.delete ~partition_key:"a Last name" dbname_partition
      collection_name_partition (document_id ^ "1")
  in
  match res with
  | Result.Error _ -> Alcotest.fail "Should not return error"
  | Result.Ok code ->
      let _ = Alcotest.(check int) "Status same int" 204 code in
      Lwt.return_unit

let delete_collection_with_partition_key_test _ () =
  let%lwt res =
    D.Collection.delete dbname_partition collection_name_partition
  in
  match res with
  | Result.Error _ -> Alcotest.fail "Should not return error"
  | Result.Ok code ->
      let _ = Alcotest.(check int) "Status same int" 204 code in
      Lwt.return_unit

let delete_database_with_partition_test _ () =
  let%lwt res = D.delete dbname_partition in
  match res with
  | Result.Error _ -> Alcotest.fail "Should not return error"
  | Result.Ok code ->
      let _ = Alcotest.(check int) "Status same int" 204 code in
      Lwt.return_unit

let create_collection_with_partition_key_fail_test _ () =
  let partition_key =
    Some
      Json_converter_t.
        { paths = [ "/lastName" ]; kind = "Hash"; version = None }
  in
  let%lwt res =
    D.Collection.create ~partition_key dbname_partition
      collection_name_partition
  in
  match res with
  | Result.Error (Azure_error (code, _)) ->
      Alcotest.(check int) "Status same int" 404 code;
      Lwt.return_unit
  | Result.Error Timeout_error ->
      Alcotest.fail "Should not fail with Timeout_error"
  | Result.Error Connection_error ->
      Alcotest.fail "Should not fail with Connection_error"
  | Result.Ok (_code, _body) -> Alcotest.fail "Should fail"

let delete_database_with_partition_fail_test _ () =
  let%lwt res = D.delete dbname_partition in
  match res with
  | Result.Error (Azure_error (code, _)) ->
      Alcotest.(check int) "Status same int" 404 code;
      Lwt.return_unit
  | Result.Error Timeout_error ->
      Alcotest.fail "Should not fail with Timeout_error"
  | Result.Error Connection_error ->
      Alcotest.fail "Should not fail with Connection_error"
  | Result.Ok _code -> Alcotest.fail "Should fail"

let delete_database_with_partition_timeout_test _ () =
  let%lwt res = D.delete ~timeout:0. dbname_partition in
  match res with
  | Result.Error (Azure_error _) ->
      Alcotest.fail "Should not fail with Azure_error"
  | Result.Error Timeout_error ->
      Alcotest.(check unit) "Timeout" () ();
      Lwt.return_unit
  | Result.Error Connection_error ->
      Alcotest.fail "Should not fail with Connection_error"
  | Result.Ok _code -> Alcotest.fail "Should fail"

let test_partition_key_cosmos =
  [
    Alcotest_lwt.test_case "create database" `Slow
      create_database_with_partition_key_test;
    Alcotest_lwt.test_case "create collection with partition key" `Slow
      create_collection_with_partition_key_test;
    Alcotest_lwt.test_case "create document with partition key" `Slow
      create_document_with_partition_key_test;
    Alcotest_lwt.test_case "create document with partition key include index"
      `Slow create_document_with_partition_key_include_index_test;
    Alcotest_lwt.test_case "create document with partition key exclude index"
      `Slow create_document_with_partition_key_exclude_index_test;
    Alcotest_lwt.test_case "replace document with partition key" `Slow
      replace_document_with_partition_key_test;
    Alcotest_lwt.test_case "upsert document with partition key" `Slow
      upsert_document_with_partition_key_test;
    Alcotest_lwt.test_case "query document with partition key" `Slow
      query_document_with_partition_key_test;
    Alcotest_lwt.test_case "query count document without partition key" `Slow
      query_document_count_without_partition_key_test;
    Alcotest_lwt.test_case "query count document with partition key" `Slow
      query_document_count_with_partition_key_test;
    Alcotest_lwt.test_case "delete document with partition key" `Slow
      delete_document_with_partition_key_test;
    Alcotest_lwt.test_case "delete collection with partition key" `Slow
      delete_collection_with_partition_key_test;
    Alcotest_lwt.test_case "delete database" `Slow
      delete_database_with_partition_test;
    Alcotest_lwt.test_case "create collection with partition key" `Slow
      create_collection_with_partition_key_fail_test;
    Alcotest_lwt.test_case "delete database with partition fail test" `Slow
      delete_database_with_partition_fail_test;
    Alcotest_lwt.test_case "delete database with partition timeout test" `Slow
      delete_database_with_partition_timeout_test;
  ]

let test_partition_key = if should_run () then test_partition_key_cosmos else []

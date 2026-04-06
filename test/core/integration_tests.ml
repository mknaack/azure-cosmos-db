open Test_common_core
open Cosmos.Databases_core

module Make
    (Cfg : Test_io_intf.Config)
    (IO : Test_io_intf.IO)
    (D : Test_io_intf.DB with type 'a io := 'a IO.t) =
struct
  let ( let* ) = IO.bind
  let dbname_partition = dbname_partition Cfg.prefix

  let create_value counter =
    let string_counter = string_of_int counter in
    ({
       id = document_id ^ string_counter;
       firstName = "A First name " ^ string_counter;
       lastName = "a Last name";
     }
      : Json_j.create_document)
    |> Json_j.string_of_create_document

  let replace_value counter =
    let string_counter = string_of_int counter in
    ({
       id = document_id ^ string_counter;
       firstName = "Something different";
       lastName = "a Last name";
     }
      : Json_j.create_document)
    |> Json_j.string_of_create_document

  let range i j =
    let rec loop acc k = if i = k then k :: acc else loop (k :: acc) (pred k) in
    loop [] j

  let create_database_with_partition_key_test () =
    let* res = D.create dbname_partition in
    match res with
    | Result.Error _ -> Alcotest.fail "Should not return error"
    | Result.Ok (code, body) ->
        let _ = Alcotest.(check int) "Status same int" 201 code in
        let _ =
          match body with
          | Some { id; _ } ->
              Alcotest.(check string)
                "Create name is correct" dbname_partition id
          | None -> ()
        in
        IO.return ()

  let create_database_with_partition_key_timeout_test () =
    let* res = D.create ~timeout:0.0 dbname_partition in
    match res with
    | Result.Error Timeout_error ->
        Alcotest.(check unit) "Timeout error" () ();
        IO.return ()
    | Result.Error _ -> Alcotest.fail "Should fail with timeout error"
    | Result.Ok _ -> Alcotest.fail "Should fail with timeout error"

  let create_database_if_not_exists_with_partition_key_test () =
    let* res = D.create_if_not_exists dbname_partition in
    match res with
    | Result.Ok (code, body) ->
        let _ = Alcotest.(check int) "Status same int" 200 code in
        let _ =
          match body with
          | Some { id; _ } ->
              Alcotest.(check string)
                "Create name is correct" dbname_partition id
          | None -> ()
        in
        IO.return ()
    | Result.Error _ -> Alcotest.fail "Should not return error"

  let list_databases_with_partition_key_test () =
    let* res = D.list_databases () in
    match res with
    | Result.Ok (code, { _rid; databases; _count = count }) ->
        let db =
          List.filter
            (fun (x : Cosmos.Json_converter_t.database) ->
              x.id = dbname_partition)
            databases
        in
        let _ = Alcotest.(check int) "Status same int" 200 code in
        let _ = Alcotest.(check bool) "Count" true (count > 0) in
        let _ =
          Alcotest.(check string)
            "Name of databases" dbname_partition (List.hd db).id
        in
        IO.return ()
    | Result.Error _ -> Alcotest.fail "Should not return error"

  let list_databases_with_partition_key_timeout_test () =
    let* res = D.list_databases ~timeout:0.0 () in
    match res with
    | Result.Error Timeout_error ->
        Alcotest.(check unit) "Timeout error" () ();
        IO.return ()
    | Result.Error _ -> Alcotest.fail "Should fail with timeout error"
    | Result.Ok _ -> Alcotest.fail "Should fail with timeout error"

  let get_database_with_partition_key_test () =
    let* res = D.get dbname_partition in
    match res with
    | Result.Ok (code, body) ->
        let _ = Alcotest.(check int) "Status same int" 200 code in
        let _ =
          match body with
          | Some { _rid; id; _self; _etag; _colls; _users; _ts } ->
              Alcotest.(check string) "Name of database" dbname_partition id
          | None -> ()
        in
        IO.return ()
    | Result.Error _ -> Alcotest.fail "Should not return error"

  let get_database_with_partition_key_timeout_test () =
    let* res = D.get ~timeout:0.0 dbname_partition in
    match res with
    | Result.Error Timeout_error ->
        Alcotest.(check unit) "Timeout error" () ();
        IO.return ()
    | Result.Error _ -> Alcotest.fail "Should fail with timeout error"
    | Result.Ok _ -> Alcotest.fail "Should fail with timeout error"

  let create_database_if_not_exists_with_partition_key_test_new_database () =
    let test_name = dbname_partition ^ "create_new" in
    let* () =
      let* res = D.create_if_not_exists test_name in
      match res with
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
          IO.return ()
      | Result.Error _ -> Alcotest.fail "Should not return error"
    in
    let* res = D.delete test_name in
    match res with
    | Result.Error _ -> Alcotest.fail "Should not return error"
    | Result.Ok code ->
        let _ = Alcotest.(check int) "Status same int" 204 code in
        IO.return ()

  let create_collection_with_partition_key_test () =
    let partition_key =
      Some
        Cosmos.Json_converter_t.
          { paths = [ "/lastName" ]; kind = "Hash"; version = None }
    in
    let* res =
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
        IO.return ()

  let create_collection_with_partition_key_timeout_test () =
    let partition_key =
      Some
        Cosmos.Json_converter_t.
          { paths = [ "/lastName" ]; kind = "Hash"; version = None }
    in
    let* res =
      D.Collection.create ~timeout:0.0 ~partition_key dbname_partition
        collection_name_partition
    in
    match res with
    | Result.Error Timeout_error ->
        Alcotest.(check unit) "Timeout error" () ();
        IO.return ()
    | Result.Error _ -> Alcotest.fail "Should fail with timeout error"
    | Result.Ok _ -> Alcotest.fail "Should fail with timeout error"

  let create_collection_with_partition_key_if_not_exists_test () =
    let partition_key =
      Some
        Cosmos.Json_converter_t.
          { paths = [ "/lastName" ]; kind = "Hash"; version = None }
    in
    let* res =
      D.Collection.create_if_not_exists ~partition_key dbname_partition
        collection_name_partition
    in
    match res with
    | Result.Ok (code, body) ->
        let _ = Alcotest.(check int) "Status same int" 200 code in
        let _ =
          match body with
          | Some { id; _ } ->
              Alcotest.(check string)
                "Create name is correct" collection_name_partition id
          | None -> ()
        in
        IO.return ()
    | Result.Error _ -> Alcotest.fail "Should not return error"

  let create_collection_with_partition_key_if_not_exists_test_new_collection ()
      =
    let collection_name_test = collection_name_partition ^ "test_collection" in
    let partition_key =
      Some
        Cosmos.Json_converter_t.
          { paths = [ "/lastName" ]; kind = "Hash"; version = None }
    in
    let* () =
      let* res =
        D.Collection.create_if_not_exists ~partition_key dbname_partition
          collection_name_test
      in
      match res with
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
          IO.return ()
      | Result.Error _ -> Alcotest.fail "Should not return error"
    in
    let* res = D.Collection.delete dbname_partition collection_name_test in
    match res with
    | Result.Error _ -> Alcotest.fail "Should not return error"
    | Result.Ok code ->
        let _ = Alcotest.(check int) "Status same int" 204 code in
        IO.return ()

  let list_collection_with_partition_key_test () =
    let* res = D.Collection.list dbname_partition in
    match res with
    | Result.Ok (code, { rid = _; document_collections = _; count }) ->
        let _ = Alcotest.(check int) "Status same int" 200 code in
        let _ = Alcotest.(check int) "Count" 1 count in
        IO.return ()
    | Result.Error _ -> Alcotest.fail "Should not return error"

  let get_collection_with_partition_key_test () =
    let* res = D.Collection.get dbname_partition collection_name_partition in
    match res with
    | Result.Error _ -> Alcotest.fail "Should not return error"
    | Result.Ok (code, body) ->
        let _ = Alcotest.(check int) "Status same int" 200 code in
        let _ =
          match body with
          | Some
              Cosmos.Json_converter_t.
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
              Alcotest.(check string)
                "Name of database" collection_name_partition id
          | None -> ()
        in
        IO.return ()

  let create_document_with_partition_key_test () =
    let* res =
      D.Collection.Document.create ~partition_key:"a Last name" dbname_partition
        collection_name_partition (create_value 1)
    in
    match res with
    | Result.Error _ -> Alcotest.fail "Should not return error"
    | Result.Ok (code, _) ->
        let _ = Alcotest.(check int) "Status same int" 201 code in
        IO.return ()

  let create_document_with_partition_key_timeout_test () =
    let* res =
      D.Collection.Document.create ~timeout:0.0 ~partition_key:"a Last name"
        dbname_partition collection_name_partition (create_value 1)
    in
    match res with
    | Result.Error Timeout_error ->
        Alcotest.(check unit) "Timeout error" () ();
        IO.return ()
    | Result.Error _ -> Alcotest.fail "Should fail with timeout error"
    | Result.Ok _ -> Alcotest.fail "Should fail with timeout error"

  let create_document_with_partition_key_include_index_test () =
    let* res =
      D.Collection.Document.create ~partition_key:"a Last name"
        ~indexing_directive:D.Collection.Document.Include dbname_partition
        collection_name_partition (create_value 2)
    in
    match res with
    | Result.Error _ -> Alcotest.fail "Should not return error"
    | Result.Ok (code, _) ->
        let _ = Alcotest.(check int) "Status same int" 201 code in
        IO.return ()

  let create_document_with_partition_key_exclude_index_test () =
    let* res =
      D.Collection.Document.create ~partition_key:"a Last name"
        ~indexing_directive:D.Collection.Document.Exclude dbname_partition
        collection_name_partition (create_value 3)
    in
    match res with
    | Result.Error _ -> Alcotest.fail "Should not return error"
    | Result.Ok (code, _) ->
        let _ = Alcotest.(check int) "Status same int" 201 code in
        IO.return ()

  let replace_document_with_partition_key_test () =
    let* res =
      D.Collection.Document.replace ~partition_key:"a Last name"
        dbname_partition collection_name_partition (document_id ^ "1")
        (replace_value 1)
    in
    match res with
    | Result.Error _ -> Alcotest.fail "Should not return error"
    | Result.Ok (code, _) ->
        let _ = Alcotest.(check int) "Status same int" 200 code in
        IO.return ()

  let upsert_document_with_partition_key_test () =
    let* res =
      D.Collection.Document.create ~is_upsert:true ~partition_key:"a Last name"
        dbname_partition collection_name_partition (create_value 1)
    in
    match res with
    | Result.Error _ -> Alcotest.fail "Should not return error"
    | Result.Ok (code, _) ->
        let _ = Alcotest.(check int) "Status same int" 200 code in
        IO.return ()

  let query_document_with_partition_key_test () =
    let query =
      Cosmos.Json_converter_t.
        {
          query =
            "SELECT * FROM " ^ collection_name ^ " f WHERE f.firstName = @fname";
          parameters = [ { name = "@fname"; value = "A First name 1" } ];
        }
    in
    let* res =
      D.Collection.Document.query ~is_partition:true dbname_partition
        collection_name_partition query
    in
    match res with
    | Result.Error _ -> Alcotest.fail "Should not return error"
    | Result.Ok (code, _, { rid = _; documents; count }) ->
        let _ = Alcotest.(check int) "Status same int" 200 code in
        let _ =
          let docs =
            List.map
              (fun (x, _) -> Json_j.create_document_of_string x)
              documents
          in
          let { Json_j.id; firstName; lastName } = List.hd docs in
          Alcotest.(check int) "Count field" count 1;
          Alcotest.(check int) "Count list" count (List.length documents);
          Alcotest.(check string) "id" id "document_id1";
          Alcotest.(check string) "firstName" firstName "A First name 1";
          Alcotest.(check string) "lastName" lastName "a Last name"
        in
        IO.return ()

  let query_document_count_without_partition_key_test () =
    (* Should fail due to the partition *)
    let query =
      Cosmos.Json_converter_t.
        { query = "SELECT VALUE COUNT(1) FROM f"; parameters = [] }
    in
    let* res =
      D.Collection.Document.query ~is_partition:true dbname_partition
        collection_name_partition query
    in
    match res with
    | Result.Ok _ | Result.Error Timeout_error | Result.Error Connection_error
      ->
        Alcotest.fail "Should not return error"
    | Result.Error (Azure_error (code, _)) ->
        let _ = Alcotest.(check int) "Status same int" 400 code in
        IO.return ()

  let query_document_count_with_partition_key_test () =
    let query =
      Cosmos.Json_converter_t.
        { query = "SELECT VALUE COUNT(1) FROM f"; parameters = [] }
    in
    let* res =
      D.Collection.Document.query ~partition_key:"a Last name"
        ~is_partition:true dbname_partition collection_name_partition query
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
          Alcotest.(check int) "Value count field" value_count 11
        in
        IO.return ()

  let list_document_with_partition_key_test () =
    let* res =
      D.Collection.Document.list dbname_partition collection_name_partition
    in
    match res with
    | Result.Error _ -> Alcotest.fail "Should not return error"
    | Result.Ok (code, _, { rid = _; documents; count }) ->
        let _ = Alcotest.(check int) "Status same int" 200 code in
        let _ = Alcotest.(check bool) "Count" true (count > 0) in
        let _ =
          Alcotest.(check int) "Count list" count (List.length documents)
        in
        IO.return ()

  let list_multiple_documents_with_partition_key_test () =
    let list_values =
      let rec make_values i acc =
        if i <= 1 then acc else make_values (i - 1) (i :: acc)
      in
      make_values 10 []
    in
    let values = List.map create_value list_values in
    let* x =
      IO.parallel_map
        (fun x ->
          D.Collection.Document.create ~partition_key:"a Last name"
            dbname_partition collection_name_partition x)
        values
    in
    let is_201 x = function Result.Ok (y, _) -> y = 201 && x | _ -> false in
    let all_is_inserted = List.fold_left is_201 true x in
    let _ = Alcotest.(check bool) "all_is_inserted" true all_is_inserted in
    let* res =
      D.Collection.Document.list ~max_item_count:5 dbname_partition
        collection_name_partition
    in
    match res with
    | Result.Error _ -> Alcotest.fail "Should not return error"
    | Result.Ok (code, headers, { rid = _; documents = _; count }) -> (
        let _ = Alcotest.(check int) "Status same int" 200 code in
        let continuation =
          Option.get (Response_headers.x_ms_continuation headers)
        in
        let* res2 =
          D.Collection.Document.list ~max_item_count:5 ~continuation
            dbname_partition collection_name_partition
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
            IO.return ())

  let change_feed_with_partition_key_test () =
    let* res =
      D.Collection.Document.list ~a_im:true dbname_partition
        collection_name_partition
    in
    match res with
    | Result.Error _ -> Alcotest.fail "Should not return error"
    | Result.Ok (code, headers, _result) -> (
        let _ = Alcotest.(check int) "Status same int" 200 code in

        let if_none_match = Option.get @@ Response_headers.etag headers in
        let* res =
          D.Collection.Document.list ~a_im:true ~if_none_match dbname_partition
            collection_name_partition
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

            let* res =
              D.Collection.Document.create ~partition_key:"a Last name"
                dbname_partition collection_name_partition (create_value 20)
            in
            match res with
            | Result.Error Timeout_error ->
                Alcotest.fail "2 Should not return Timeout_error"
            | Result.Error Connection_error ->
                Alcotest.fail "Should not fail with Connection_error"
            | Result.Error (Azure_error (error_code, _)) ->
                Alcotest.fail
                  ("2 Should not return error code : "
                 ^ string_of_int error_code)
            | Result.Ok (code, _) -> (
                let _ = Alcotest.(check int) "Status same int" 201 code in

                let* res =
                  D.Collection.Document.list ~a_im:true ~if_none_match
                    dbname_partition collection_name_partition
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
                    IO.return ())))

  let get_document_with_partition_key_test () =
    let* res =
      D.Collection.Document.get ~partition_key:"a Last name" dbname_partition
        collection_name_partition (document_id ^ "1")
    in
    match res with
    | Result.Error _ -> Alcotest.fail "Should not return error"
    | Result.Ok (code, _body) ->
        let _ = Alcotest.(check int) "Status same int" 200 code in
        IO.return ()

  let create_a_lot_of_documents_with_partition_key_test () =
    let ids = range 21 100 in
    let* result_list =
      IO.parallel_map
        (fun id ->
          D.Collection.Document.create ~partition_key:"a Last name"
            dbname_partition collection_name_partition (create_value id))
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
    IO.return ()

  let delete_document_with_partition_key_test () =
    let* res =
      D.Collection.Document.delete ~partition_key:"a Last name" dbname_partition
        collection_name_partition (document_id ^ "1")
    in
    match res with
    | Result.Error _ -> Alcotest.fail "Should not return error"
    | Result.Ok code ->
        let _ = Alcotest.(check int) "Status same int" 204 code in
        IO.return ()

  let delete_collection_with_partition_key_test () =
    let* res = D.Collection.delete dbname_partition collection_name_partition in
    match res with
    | Result.Error _ -> Alcotest.fail "Should not return error"
    | Result.Ok code ->
        let _ = Alcotest.(check int) "Status same int" 204 code in
        IO.return ()

  let delete_database_with_partition_test () =
    let* res = D.delete dbname_partition in
    match res with
    | Result.Error _ -> Alcotest.fail "Should not return error"
    | Result.Ok code ->
        let _ = Alcotest.(check int) "Status same int" 204 code in
        IO.return ()

  let create_collection_with_partition_key_fail_test () =
    let partition_key =
      Some
        Cosmos.Json_converter_t.
          { paths = [ "/lastName" ]; kind = "Hash"; version = None }
    in
    let* res =
      D.Collection.create ~partition_key dbname_partition
        collection_name_partition
    in
    match res with
    | Result.Error (Azure_error (code, _)) ->
        Alcotest.(check int) "Status same int" 404 code;
        IO.return ()
    | Result.Error Timeout_error ->
        Alcotest.fail "Should not fail with Timeout_error"
    | Result.Error Connection_error ->
        Alcotest.fail "Should not fail with Connection_error"
    | Result.Ok (_code, _body) -> Alcotest.fail "Should fail"

  let delete_database_with_partition_fail_test () =
    let* res = D.delete dbname_partition in
    match res with
    | Result.Error (Azure_error (code, _)) ->
        Alcotest.(check int) "Status same int" 404 code;
        IO.return ()
    | Result.Error Timeout_error ->
        Alcotest.fail "Should not fail with Timeout_error"
    | Result.Error Connection_error ->
        Alcotest.fail "Should not fail with Connection_error"
    | Result.Ok _code -> Alcotest.fail "Should fail"

  let delete_database_with_partition_timeout_test () =
    let* res = D.delete ~timeout:0. dbname_partition in
    match res with
    | Result.Error (Azure_error _) ->
        Alcotest.fail "Should not fail with Azure_error"
    | Result.Error Timeout_error ->
        Alcotest.(check unit) "Timeout" () ();
        IO.return ()
    | Result.Error Connection_error ->
        Alcotest.fail "Should not fail with Connection_error"
    | Result.Ok _code -> Alcotest.fail "Should fail"

  let tests =
    [
      ("create database", create_database_with_partition_key_test);
      ( "create database if not exists",
        create_database_if_not_exists_with_partition_key_test );
      ("list database", list_databases_with_partition_key_test);
      ("list database timeout", list_databases_with_partition_key_timeout_test);
      ("get database", get_database_with_partition_key_test);
      ("get database timeout", get_database_with_partition_key_timeout_test);
      ( "create collection with partition key",
        create_collection_with_partition_key_test );
      ( "create collection with partition key timeout",
        create_collection_with_partition_key_timeout_test );
      ( "create collection with partition key if not exists",
        create_collection_with_partition_key_if_not_exists_test );
      ( "create collection with partition key if not exists with new collection",
        create_collection_with_partition_key_if_not_exists_test_new_collection
      );
      ( "list collection with partition key",
        list_collection_with_partition_key_test );
      ( "get collection with partition key",
        get_collection_with_partition_key_test );
      ( "create document with partition key",
        create_document_with_partition_key_test );
      ("list document with partition key", list_document_with_partition_key_test);
      ( "list multiple documents with partition key",
        list_multiple_documents_with_partition_key_test );
      ("change feed with partition key", change_feed_with_partition_key_test);
      ( "query document with partition key",
        query_document_with_partition_key_test );
      ( "query count document without partition key",
        query_document_count_without_partition_key_test );
      ( "query count document with partition key",
        query_document_count_with_partition_key_test );
      ("get document with partition key", get_document_with_partition_key_test);
      ( "replace document with partition key",
        replace_document_with_partition_key_test );
      ( "upsert document with partition key",
        upsert_document_with_partition_key_test );
      ( "create a lot document with partition key",
        create_a_lot_of_documents_with_partition_key_test );
      ( "delete document with partition key",
        delete_document_with_partition_key_test );
      ( "delete collection with partition key",
        delete_collection_with_partition_key_test );
      ("delete database", delete_database_with_partition_test);
      ( "create and delete database",
        create_database_if_not_exists_with_partition_key_test_new_database );
      ( "create collection with partition key",
        create_collection_with_partition_key_fail_test );
      ( "delete database with partition fail test",
        delete_database_with_partition_fail_test );
      ( "delete database with partition timeout test",
        delete_database_with_partition_timeout_test );
    ]
end

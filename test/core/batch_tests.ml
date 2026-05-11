open Test_common_core
open Cosmos.Databases_core

module Make
    (Cfg : Test_io_intf.Config)
    (IO : Test_io_intf.IO)
    (D : Test_io_intf.DB with type 'a io := 'a IO.t) =
struct
  let ( let* ) = IO.bind
  let dbname = dbname_partition Cfg.prefix
  let coll_name = collection_name_partition

  let create_doc_json id name pk =
    Printf.sprintf {|{"id": "%s", "firstName": "%s", "lastName": "%s"}|} id name
      pk

  let setup_collection () =
    let partition_key =
      Cosmos.Json_converter_t.
        { paths = [ "/lastName" ]; kind = "Hash"; version = None }
    in
    let* _ = D.create_if_not_exists dbname in
    let* _ =
      D.Collection.create_if_not_exists ~partition_key dbname coll_name
    in
    IO.return ()

  let create_batch_test () =
    let* () = setup_collection () in
    let partition_key = "a Last name" in
    let* _ =
      D.Collection.Document.delete ~partition_key dbname coll_name "batch1"
    in
    let* _ =
      D.Collection.Document.delete ~partition_key dbname coll_name "batch2"
    in
    let ops =
      [
        D.Collection.Batch.Create
          {
            if_match = None;
            if_none_match = None;
            body = create_doc_json "batch1" "Alice" partition_key;
          };
        D.Collection.Batch.Create
          {
            if_match = None;
            if_none_match = None;
            body = create_doc_json "batch2" "Bob" partition_key;
          };
      ]
    in
    let* result =
      D.Collection.Batch.execute ~partition_key dbname coll_name ops
    in
    match result with
    | Ok { outcomes; _ } ->
        List.iteri
          (fun i r ->
            Alcotest.(check bool)
              (Printf.sprintf "Operation %d succeeded" i)
              true
              (D.Collection.Batch.is_success r))
          outcomes;
        IO.return ()
    | Error _ -> Alcotest.fail "Batch should succeed"

  let atomic_batch_rollback_test () =
    let* () = setup_collection () in
    let partition_key = "test-pk" in
    (* First create a document *)
    let doc = create_doc_json "conflict-doc" "Test" partition_key in
    let* _ = D.Collection.Document.create ~partition_key dbname coll_name doc in
    (* Now try to batch create the same document again with another new doc *)
    let ops =
      [
        D.Collection.Batch.Create
          { if_match = None; if_none_match = None; body = doc };
        D.Collection.Batch.Create
          {
            if_match = None;
            if_none_match = None;
            body = create_doc_json "new-doc" "New" partition_key;
          };
      ]
    in
    let* result =
      D.Collection.Batch.execute ~partition_key ~atomic:true dbname coll_name
        ops
    in
    match result with
    | Ok { outcomes; _ } ->
        (* In atomic mode with conflict, both should fail *)
        let all_failed =
          List.for_all (fun r -> not (D.Collection.Batch.is_success r)) outcomes
        in
        Alcotest.(check bool)
          "Atomic batch should fail entirely on conflict" true all_failed;
        IO.return ()
    | Error _ ->
        Alcotest.fail
          "Batch request should succeed even with operation failures"

  let mixed_operations_test () =
    let* () = setup_collection () in
    let partition_key = "mixed-pk" in
    let* _ =
      D.Collection.Document.delete ~partition_key dbname coll_name "mixed1"
    in
    let* _ =
      D.Collection.Document.delete ~partition_key dbname coll_name "mixed2"
    in
    (* Create a document first *)
    let doc = create_doc_json "mixed1" "First" partition_key in
    let* _ = D.Collection.Document.create ~partition_key dbname coll_name doc in
    (* Now batch read it and create another *)
    let ops =
      [
        D.Collection.Batch.Read
          { id = "mixed1"; if_match = None; if_none_match = None };
        D.Collection.Batch.Create
          {
            if_match = None;
            if_none_match = None;
            body = create_doc_json "mixed2" "Second" partition_key;
          };
      ]
    in
    let* result =
      D.Collection.Batch.execute ~partition_key dbname coll_name ops
    in
    match result with
    | Ok { outcomes; _ } ->
        List.iteri
          (fun i r ->
            Alcotest.(check bool)
              (Printf.sprintf "Operation %d succeeded" i)
              true
              (D.Collection.Batch.is_success r))
          outcomes;
        IO.return ()
    | Error _ -> Alcotest.fail "Mixed operations batch should succeed"

  let empty_batch_test () =
    let* () = setup_collection () in
    let ops = [] in
    try
      let* _ =
        D.Collection.Batch.execute ~partition_key:"pk" dbname coll_name ops
      in
      Alcotest.fail "Empty batch should fail"
    with Failure msg ->
      Alcotest.(check string) "Empty batch error message" "Empty batch" msg;
      IO.return ()

  let max_operations_test () =
    let ops =
      List.init 101 (fun i ->
          D.Collection.Batch.Create
            {
              if_match = None;
              if_none_match = None;
              body = create_doc_json (string_of_int i) "name" "pk";
            })
    in
    match D.Collection.Batch.validate ops with
    | Error (Too_many_operations 101) -> IO.return ()
    | _ -> Alcotest.fail "Should fail with Too_many_operations"

  let batch_builder_test () =
    let open D.Collection.Batch_builder in
    let builder =
      empty
      |> add_create ~body:(create_doc_json "b1" "Builder" "pk")
      |> add_read ~id:"existing-doc"
    in
    Alcotest.(check int) "Builder has 2 operations" 2 (length builder);
    let ops = to_operations builder in
    Alcotest.(check int) "Converted to 2 operations" 2 (List.length ops);
    IO.return ()

  let non_atomic_partial_test () =
    let* () = setup_collection () in
    let partition_key = "partial-pk" in
    (* Create a document first *)
    let doc = create_doc_json "existing" "Test" partition_key in
    let* _ = D.Collection.Document.create ~partition_key dbname coll_name doc in
    (* Try to create same doc (conflict) + new doc in non-atomic mode *)
    let ops =
      [
        D.Collection.Batch.Create
          { if_match = None; if_none_match = None; body = doc };
        D.Collection.Batch.Create
          {
            if_match = None;
            if_none_match = None;
            body = create_doc_json "new-partial" "New" partition_key;
          };
      ]
    in
    let* result =
      D.Collection.Batch.execute ~partition_key ~atomic:false dbname coll_name
        ops
    in
    match result with
    | Ok { outcomes; _ } ->
        (* In non-atomic mode, one may succeed while other fails *)
        let _ =
          List.iteri
            (fun i (r : D.Collection.Batch.operation_result) ->
              Printf.printf "Operation %d: status=%d\n" i r.status_code)
            outcomes
        in
        IO.return ()
    | Error _ ->
        Alcotest.fail
          "Non-atomic batch should return results even with partial failures"

  let teardown_test () =
    let* _ = D.Collection.delete dbname coll_name in
    let* _ = D.delete dbname in
    IO.return ()

  let tests =
    [
      ("create batch", create_batch_test);
      ("atomic rollback", atomic_batch_rollback_test);
      ("mixed operations", mixed_operations_test);
      ("empty batch", empty_batch_test);
      ("max operations limit", max_operations_test);
      ("batch builder", batch_builder_test);
      ("non-atomic partial", non_atomic_partial_test);
      ("teardown", teardown_test);
    ]
end

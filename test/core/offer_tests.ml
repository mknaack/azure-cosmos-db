open Cosmos.Databases_core

module Make
    (Cfg : Test_io_intf.Config)
    (IO : Test_io_intf.IO)
    (D : Test_io_intf.DB with type 'a io := 'a IO.t) =
struct
  let ( let* ) = IO.bind
  let dbname = Cfg.prefix ^ "offer_database"
  let coll_name = "offerCollection"

  let partition_key =
    Cosmos.Json_converter_t.{ paths = [ "/id" ]; kind = "Hash"; version = None }

  let check_success name code =
    Alcotest.(check bool) name true (code = 200 || code = 201)

  let rec with_offer_retry attempts f =
    let* result = f () in
    match result with
    | Error (Azure_error ((423 | 429 | 449), _)) when attempts > 0 ->
        let* () = IO.sleep 2.0 in
        with_offer_retry (attempts - 1) f
    | result -> IO.return result

  let restore_throughput () =
    let* result = D.Offer.get_for_collection dbname coll_name in
    match result with
    | Ok (_, Some _) ->
        let* _ =
          with_offer_retry 5 (fun () ->
              D.Offer.set_throughput dbname coll_name
                (D.Offer.Throughput.Manual 400))
        in
        IO.return ()
    | Ok (_, None) | Error _ -> IO.return ()

  let teardown () =
    let* () = restore_throughput () in
    let* _ = D.Collection.delete dbname coll_name in
    let* _ = D.delete dbname in
    IO.return ()

  let with_teardown f =
    IO.catch
      (fun () ->
        let* result = f () in
        let* () = teardown () in
        IO.return result)
      (fun exn ->
        let* () = teardown () in
        raise exn)

  let assert_manual expected = function
    | Some (D.Offer.Throughput.Manual actual) ->
        Alcotest.(check int) "manual throughput" expected actual
    | Some (D.Offer.Throughput.Autoscale _) ->
        Alcotest.fail "Expected manual throughput"
    | None -> Alcotest.fail "Expected a throughput value"

  let lifecycle_test () =
    with_teardown (fun () ->
        let* () = teardown () in
        let* database = D.create_if_not_exists dbname in
        let database_code =
          match database with
          | Ok (code, _) -> code
          | Error _ -> Alcotest.fail "Database creation failed"
        in
        check_success "database created or found" database_code;
        let* collection =
          D.Collection.create_if_not_exists ~offer_throughput:400 ~partition_key
            dbname coll_name
        in
        let collection_code =
          match collection with
          | Ok (code, _) -> code
          | Error _ -> Alcotest.fail "Collection creation failed"
        in
        check_success "collection created or found" collection_code;
        let* offers = D.Offer.list () in
        (match offers with
        | Ok (code, _) -> Alcotest.(check int) "offer list status" 200 code
        | Error _ -> Alcotest.fail "Offer.list failed");
        let* database_offer = D.Offer.get_for_database dbname in
        (match database_offer with
        | Ok _ -> ()
        | Error _ -> Alcotest.fail "Database offer lookup failed");
        let* collection_offer = D.Offer.get_for_collection dbname coll_name in
        match collection_offer with
        | Error _ -> Alcotest.fail "Collection offer lookup failed"
        | Ok (_, None) -> IO.return ()
        | Ok (_, Some offer) ->
            let offer_rid = offer.Cosmos.Json_converter_t.rid in
            let* fetched = D.Offer.get offer_rid in
            (match fetched with
            | Ok (code, fetched_offer) ->
                Alcotest.(check int) "offer get status" 200 code;
                Alcotest.(check string)
                  "offer rid" offer_rid
                  fetched_offer.Cosmos.Json_converter_t.rid
            | Error _ -> Alcotest.fail "Offer.get failed");
            let* throughput = D.Offer.get_throughput dbname coll_name in
            (match throughput with
            | Ok (code, value) ->
                Alcotest.(check int) "throughput get status" 200 code;
                assert_manual 400 value
            | Error _ -> Alcotest.fail "Initial throughput lookup failed");
            let* updated =
              with_offer_retry 5 (fun () ->
                  D.Offer.set_throughput dbname coll_name
                    (D.Offer.Throughput.Manual 500))
            in
            (match updated with
            | Ok (code, _) ->
                Alcotest.(check int) "throughput update status" 200 code
            | Error _ -> Alcotest.fail "Throughput update failed");
            let* updated_throughput = D.Offer.get_throughput dbname coll_name in
            (match updated_throughput with
            | Ok (_, value) -> assert_manual 500 value
            | Error _ -> Alcotest.fail "Updated throughput lookup failed");
            let* restored =
              with_offer_retry 5 (fun () ->
                  D.Offer.set_throughput dbname coll_name
                    (D.Offer.Throughput.Manual 400))
            in
            (match restored with
            | Ok (code, _) ->
                Alcotest.(check int) "throughput restore status" 200 code
            | Error _ -> Alcotest.fail "Throughput restore failed");
            IO.return ())

  let tests = [ ("offer lifecycle", lifecycle_test) ]
end

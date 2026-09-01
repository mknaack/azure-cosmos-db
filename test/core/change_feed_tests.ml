open Cosmos.Databases_core

module Make
    (Cfg : Test_io_intf.Config)
    (IO : Test_io_intf.IO)
    (D : Test_io_intf.DB with type 'a io := 'a IO.t) =
struct
  let ( let* ) = IO.bind
  let dbname = Cfg.prefix ^ "change_feed_database"
  let coll_name = "changeFeedCollection"
  let paging_coll_name = "changeFeedPagingCollection"

  let partition_key_definition =
    Cosmos.Json_converter_t.{ paths = [ "/pk" ]; kind = "Hash"; version = None }

  let partition_key = "pk"

  let document id =
    Printf.sprintf {|{"id":"%s","pk":"pk","value":"original"}|} id

  let replacement id =
    Printf.sprintf {|{"id":"%s","pk":"pk","value":"replaced"}|} id

  let check_success name = function
    | Ok (code, _) -> Alcotest.(check bool) name true (code = 200 || code = 201)
    | Error _ -> Alcotest.fail (name ^ ": request failed")

  let check_served_by_azure headers =
    (match Response_headers.x_ms_activity_id headers with
    | Some id ->
        Alcotest.(check bool)
          "server activity id present" true
          (String.length id > 0)
    | None ->
        Alcotest.fail "no x-ms-activity-id: response did not come from Azure");
    match Response_headers.x_ms_request_charge headers with
    | Some charge ->
        Alcotest.(check bool)
          "request charge > 0" true
          (float_of_string charge > 0.)
    | None ->
        Alcotest.fail "no x-ms-request-charge: response did not come from Azure"

  let checkpoint_of_headers headers =
    match Response_headers.etag headers with
    | Some checkpoint -> checkpoint
    | None -> Alcotest.fail "feed response did not include an etag"

  let checkpoint_of_read = function
    | Ok (304, headers, None) -> checkpoint_of_headers headers
    | Ok (200, headers, Some (page : D.Collection.Change_feed.page)) ->
        ignore headers;
        page.continuation
    | Ok _ -> Alcotest.fail "unexpected change feed response"
    | Error _ -> Alcotest.fail "change feed read failed"

  let rec until_changes attempts f =
    let* result = f () in
    match result with
    | Ok (304, _, _) when attempts > 0 ->
        let* () = IO.sleep 1.0 in
        until_changes (attempts - 1) f
    | Ok (304, _, _) -> Alcotest.fail "change never appeared in the feed"
    | result -> IO.return result

  let ids (page : D.Collection.Change_feed.page) =
    List.map fst page.documents |> List.sort String.compare

  let page_count (page : D.Collection.Change_feed.page) = page.count

  let page_continuation (page : D.Collection.Change_feed.page) =
    page.continuation

  let page_documents (page : D.Collection.Change_feed.page) = page.documents

  let page_has_more_pages (page : D.Collection.Change_feed.page) =
    page.has_more_pages

  let check_page_ids expected page =
    Alcotest.(check (list string))
      "document ids"
      (List.sort String.compare expected)
      (ids page)

  let with_teardown f =
    IO.catch
      (fun () ->
        let* result = f () in
        let* _ = D.Collection.delete dbname coll_name in
        let* _ = D.delete dbname in
        IO.return result)
      (fun exn ->
        let* _ = D.Collection.delete dbname coll_name in
        let* _ = D.delete dbname in
        raise exn)

  let with_paging_teardown f =
    IO.catch
      (fun () ->
        let* result = f () in
        let* _ = D.Collection.delete dbname paging_coll_name in
        IO.return result)
      (fun exn ->
        let* _ = D.Collection.delete dbname paging_coll_name in
        raise exn)

  let create_documents name coll ids =
    let rec loop = function
      | [] -> IO.return ()
      | id :: rest ->
          let* result =
            D.Collection.Document.create ~partition_key dbname coll
              (document id)
          in
          check_success (name ^ " " ^ id) result;
          loop rest
    in
    loop ids

  let lifecycle_test () =
    with_teardown (fun () ->
        let* _ = D.delete dbname in
        let* database = D.create_if_not_exists dbname in
        check_success "database created or found" database;
        let* collection =
          D.Collection.create_if_not_exists ~offer_throughput:400
            ~partition_key:partition_key_definition dbname coll_name
        in
        check_success "collection created or found" collection;
        let* initial =
          D.Collection.Change_feed.read
            ~start_from:D.Collection.Change_feed.Start_from.Now dbname coll_name
        in
        (match initial with
        | Ok (304, headers, None) -> check_served_by_azure headers
        | Ok (200, headers, Some _) -> check_served_by_azure headers
        | _ -> Alcotest.fail "initial change feed read failed");
        let checkpoint = checkpoint_of_read initial in
        let* () =
          create_documents "create" coll_name [ "doc1"; "doc2"; "doc3" ]
        in
        let* after_create =
          until_changes 5 (fun () ->
              D.Collection.Change_feed.read
                ~start_from:
                  (D.Collection.Change_feed.Start_from.Continuation checkpoint)
                dbname coll_name)
        in
        let create_page =
          match after_create with
          | Ok (200, _, Some page) -> page
          | _ -> Alcotest.fail "created documents did not reach the feed"
        in
        Alcotest.(check int) "created document count" 3 (page_count create_page);
        check_page_ids [ "doc1"; "doc2"; "doc3" ] create_page;
        let* caught_up =
          D.Collection.Change_feed.read
            ~start_from:
              (D.Collection.Change_feed.Start_from.Continuation
                 (page_continuation create_page))
            dbname coll_name
        in
        (match caught_up with
        | Ok (304, _, None) -> ()
        | _ -> Alcotest.fail "feed did not report caught up");
        let* replaced =
          D.Collection.Document.replace ~partition_key dbname coll_name "doc1"
            (replacement "doc1")
        in
        check_success "replace document" replaced;
        let* after_replace =
          until_changes 5 (fun () ->
              D.Collection.Change_feed.read
                ~start_from:
                  (D.Collection.Change_feed.Start_from.Continuation
                     (page_continuation create_page))
                dbname coll_name)
        in
        (match after_replace with
        | Ok (200, _, Some page) ->
            Alcotest.(check bool)
              "replaced document present" true
              (List.mem_assoc "doc1" (page_documents page))
        | _ -> Alcotest.fail "replacement did not reach the feed");
        let* deleted =
          D.Collection.Document.delete ~partition_key dbname coll_name "doc2"
        in
        (match deleted with
        | Ok (200 | 204) -> ()
        | Ok code -> Alcotest.failf "delete document returned %d" code
        | Error _ -> Alcotest.fail "delete document failed");
        let* sentinel =
          D.Collection.Document.replace ~partition_key dbname coll_name "doc3"
            (replacement "doc3")
        in
        check_success "replace sentinel" sentinel;
        let* after_delete =
          until_changes 5 (fun () ->
              D.Collection.Change_feed.read
                ~start_from:
                  (D.Collection.Change_feed.Start_from.Continuation
                     (page_continuation create_page))
                dbname coll_name)
        in
        (match after_delete with
        | Ok (200, _, Some page) ->
            Alcotest.(check bool)
              "sentinel present" true
              (List.mem_assoc "doc3" (page_documents page));
            Alcotest.(check bool)
              "delete absent in latest-version feed" false
              (List.mem_assoc "doc2" (page_documents page))
        | _ -> Alcotest.fail "delete/replacement changes did not reach the feed");
        let* ranges = D.Collection.Partition_key_range.ids dbname coll_name in
        let range_ids =
          match ranges with
          | Ok (200, ids) ->
              Alcotest.(check bool)
                "partition ranges non-empty" true
                (List.length ids > 0);
              ids
          | Ok (code, _) -> Alcotest.failf "unexpected ranges status %d" code
          | Error _ -> Alcotest.fail "partition range lookup failed"
        in
        let rec read_ranges total = function
          | [] -> IO.return total
          | range_id :: rest ->
              let* result =
                D.Collection.Change_feed.read
                  ~scope:
                    (D.Collection.Change_feed.Scope.Partition_key_range range_id)
                  ~start_from:D.Collection.Change_feed.Start_from.Beginning
                  dbname coll_name
              in
              let count =
                match result with
                | Ok (200, _, Some page) -> page_count page
                | Ok (304, _, None) -> 0
                | _ -> Alcotest.fail "partition-range feed read failed"
              in
              read_ranges (total + count) rest
        in
        let* range_count = read_ranges 0 range_ids in
        let* container =
          D.Collection.Change_feed.read
            ~start_from:D.Collection.Change_feed.Start_from.Beginning dbname
            coll_name
        in
        let container_count =
          match container with
          | Ok (200, _, Some page) -> page_count page
          | Ok (304, _, None) -> 0
          | _ -> Alcotest.fail "container feed read failed"
        in
        Alcotest.(check int) "range fan-out count" container_count range_count;
        let* partition =
          D.Collection.Change_feed.read
            ~scope:(D.Collection.Change_feed.Scope.Partition_key "pk")
            ~start_from:D.Collection.Change_feed.Start_from.Beginning dbname
            coll_name
        in
        (match partition with
        | Ok (200, _, Some page) ->
            Alcotest.(check bool)
              "partition feed non-empty" true
              (page_count page > 0)
        | _ -> Alcotest.fail "partition-key feed read failed");
        IO.return ())

  let paging_test () =
    with_paging_teardown (fun () ->
        let* collection =
          D.Collection.create_if_not_exists ~offer_throughput:400
            ~partition_key:partition_key_definition dbname paging_coll_name
        in
        check_success "paging collection created" collection;
        let* () =
          create_documents "paging create" paging_coll_name
            [ "page1"; "page2"; "page3" ]
        in
        let* first =
          D.Collection.Change_feed.read
            ~start_from:D.Collection.Change_feed.Start_from.Beginning
            ~max_item_count:1 dbname paging_coll_name
        in
        let first_page =
          match first with
          | Ok (200, _, Some page) -> page
          | _ -> Alcotest.fail "paging feed did not return its first page"
        in
        Alcotest.(check int) "first page count" 1 (page_count first_page);
        Alcotest.(check bool)
          "first page has continuation" true
          (page_has_more_pages first_page);
        let* drained =
          D.Collection.Change_feed.drain
            ~start_from:D.Collection.Change_feed.Start_from.Beginning
            ~max_item_count:1 dbname paging_coll_name
        in
        let drained =
          match drained with
          | Ok result -> result
          | Error _ -> Alcotest.fail "drain failed"
        in
        Alcotest.(check bool) "drain caught up" true drained.caught_up;
        Alcotest.(check bool)
          "drain has multiple pages" true
          (List.length drained.pages >= 2);
        Alcotest.(check int)
          "drain total count" 3
          (List.fold_left
             (fun count page -> count + page_count page)
             0 drained.pages);
        let drained_ids =
          List.flatten (List.map ids drained.pages) |> List.sort String.compare
        in
        Alcotest.(check (list string))
          "drain ids"
          [ "page1"; "page2"; "page3" ]
          drained_ids;
        let* folded =
          D.Collection.Change_feed.fold
            ~start_from:D.Collection.Change_feed.Start_from.Beginning
            ~max_item_count:1 ~max_polls:1 ~init:0
            ~f:(fun count page -> IO.return (Ok (count + page_count page)))
            dbname paging_coll_name
        in
        (match folded with
        | Ok (count, checkpoint) ->
            Alcotest.(check int) "fold count" 3 count;
            Alcotest.(check bool)
              "fold checkpoint" true
              (String.length checkpoint > 0)
        | Error _ -> Alcotest.fail "fold failed");
        IO.return ())

  let missing_collection_test () =
    let* result =
      D.Collection.Change_feed.read
        ~start_from:D.Collection.Change_feed.Start_from.Now dbname
        "collection-that-does-not-exist"
    in
    match result with
    | Error (Azure_error (404, _)) -> IO.return ()
    | Error _ -> Alcotest.fail "missing collection did not return 404"
    | Ok _ -> Alcotest.fail "missing collection unexpectedly succeeded"

  let tests =
    [
      ("change feed lifecycle", lifecycle_test);
      ("change feed paging", paging_test);
      ("change feed missing collection", missing_collection_test);
    ]
end

(*
how to run:
dune exec ./src/cosmos_runner/cosmos_runner.exe

stress test (reproduces the >200k-calls hang; verifies connection reuse):
dune exec ./src/cosmos_runner/cosmos_runner.exe -- stress [num_calls] [parallelism]
e.g. dune exec ./src/cosmos_runner/cosmos_runner.exe -- stress 200000 50
*)

open Cosmos_lwt
open Databases

let master_key_env = "AZURE_COSMOS_KEY"
let endpoint_env = "AZURE_COSMOS_ENDPOINT"

module MyAuthKeys : Auth_key = struct
  let getenv s = match Sys.getenv_opt s with None -> "" | Some x -> x
  let master_key = getenv master_key_env
  let endpoint = getenv endpoint_env
end

module D = Database (MyAuthKeys)

let dbname = "test"
let collection_name = "testCollection"
let dbname_partition = "testPartition"
let collection_name_partition = "testPartition"
let document_id = "document_id"

let create_database () =
  let%lwt res = D.create dbname in
  match res with
  | Result.Ok (code, body) ->
      let _ =
        match body with
        | Some { id; _ } ->
            Lwt_io.printf "create_database %i %s %s\n" code dbname id
        | None -> Lwt.return_unit
      in
      Lwt.return_unit
  | Result.Error _ -> Lwt_io.printf "ERROR create_database %s\n" dbname

let create_collection partition_key () =
  let res = D.Collection.create ~partition_key dbname collection_name in
  match%lwt res with
  | Result.Ok (code, body) ->
      let _ =
        match body with
        | Some { id; _ } ->
            Lwt_io.printf "create_database %i %s %s\n" code dbname id
        | None -> Lwt.return_unit
      in
      Lwt.return_unit
  | Result.Error _ ->
      Lwt_io.printf "ERROR create_collection %s %s\n" dbname collection_name

let make_id counter =
  let string_counter = string_of_int counter in
  document_id ^ string_counter

let create_value counter =
  let string_counter = string_of_int counter in
  ({
     id = make_id counter;
     firstName = "A First name " ^ string_counter;
     lastName = "a Last name";
   }
    : Json_t.create_document)
  |> Json_j.string_of_create_document

let update_value counter =
  let string_counter = string_of_int counter in
  ({
     id = document_id ^ string_counter;
     firstName = "A First " ^ string_counter;
     lastName = "a Last name";
   }
    : Json_t.create_document)
  |> Json_j.string_of_create_document

let range i j =
  let rec loop acc k = if i = k then k :: acc else loop (k :: acc) (pred k) in
  loop [] j

let ids = range 0 1000

let create_a_lot_of_documents partition_key () =
  let start_time = Unix.time () in
  let%lwt () = Lwt_io.print "create_a_lot_of_documents " in
  let values =
    List.map (fun content -> (partition_key, update_value content)) ids
  in
  let%lwt result_list =
    D.Collection.Document.create_multiple dbname collection_name values
  in
  let check expected_code = function
    | Result.Ok (code, _) -> code = expected_code
    | _ -> false
  in
  let check_fail = function
    | Result.Error Connection_error -> true
    | _ -> false
  in
  let results_length = List.filter (check 201) result_list |> List.length in
  let length_429 = List.filter (check 429) result_list |> List.length in
  let failures = List.filter check_fail result_list |> List.length in
  let end_time = Unix.time () in
  let time = end_time -. start_time in
  Lwt_io.printf
    "results_length: %i length_429: %i failures: %i, time: %.2f sec\n"
    results_length length_429 failures time

let update_a_lot_of_documents_with_upsert partition_key () =
  let start_time = Unix.time () in
  let%lwt () = Lwt_io.print "update_a_lot_of_documents_with_upsert " in
  let values =
    List.map (fun content -> (partition_key, update_value content)) ids
  in
  let%lwt result_list =
    D.Collection.Document.create_multiple ~is_upsert:true dbname collection_name
      values
  in
  let check expected_code = function
    | Result.Ok (code, _) ->
        (* let () =
             Printf.printf "update_a_lot_of_documents_with_upsert %i\n" code
           in *)
        code = expected_code
    | _ -> false
  in
  let check_fail = function
    | Result.Error Connection_error -> true
    | _ -> false
  in
  let results_length = List.filter (check 200) result_list |> List.length in
  let length_429 = List.filter (check 429) result_list |> List.length in
  let failures = List.filter check_fail result_list |> List.length in
  let end_time = Unix.time () in
  let time = end_time -. start_time in
  Lwt_io.printf
    "results_length: %i length_429: %i failures: %i, time: %.2f sec\n"
    results_length length_429 failures time

let delete_a_lot_of_documents partition_key () =
  let start_time = Unix.time () in
  let%lwt () = Lwt_io.print "delete_a_lot_of_documents " in
  let values = List.map make_id ids in
  let%lwt result_list =
    D.Collection.Document.delete_multiple ~partition_key dbname collection_name
      values
  in
  let check expected_code = function
    | Result.Ok code -> code = expected_code
    | _ -> false
  in
  let check_fail = function
    | Result.Error Connection_error -> true
    | _ -> false
  in
  let results_length = List.filter (check 204) result_list |> List.length in
  let length_429 = List.filter (check 429) result_list |> List.length in
  let failures = List.filter check_fail result_list |> List.length in
  let end_time = Unix.time () in
  let time = end_time -. start_time in
  Lwt_io.printf
    "results_length: %i length_429: %i failures: %i, time: %.2f sec\n"
    results_length length_429 failures time

let get_a_lot_of_documents partition_key_range_id () =
  let start_time = Unix.time () in
  let%lwt () = Lwt_io.print "get_a_lot_of_documents " in
  let%lwt result_list =
    D.Collection.Document.list ?partition_key_range_id dbname collection_name
  in
  let get = function
    | Result.Ok (_, _, { D.Collection.Document.rid = _; documents; count = _ })
      ->
        documents
    | _ -> []
  in
  let get_code = function Result.Ok (code, _, _) -> code | _ -> 0 in
  (* let check expected_code = function
       | Result.Ok (code, _, _) -> code = expected_code
       | _ -> false
     in *)
  let documents = get result_list |> List.length in
  let code = get_code result_list in
  (* let results_length = check 200 result_list in *)
  (* let length_429 = check 429 result_list in *)
  let end_time = Unix.time () in
  let time = end_time -. start_time in
  Lwt_io.printf "results_length: %i code: %i, time: %.2f sec\n" documents code
    time

let delete_database () =
  let%lwt res = D.delete dbname in
  match res with
  | Result.Error _ -> Lwt_io.printf "ERROR delete_database %s\n" dbname
  | Result.Ok code -> Lwt_io.printf "delete_database %i %s\n" code dbname

let do_with_partition partition_key partition () =
  let%lwt () = create_database () in
  let%lwt () = create_collection partition_key () in
  let%lwt () = create_a_lot_of_documents partition () in
  let%lwt () = update_a_lot_of_documents_with_upsert partition () in
  let%lwt () = get_a_lot_of_documents None () in
  let%lwt () = delete_a_lot_of_documents partition () in
  let%lwt () = delete_database () in
  Lwt.return_unit

(* Number of open file descriptors of this process. Under the old
   connection-per-request behaviour this (together with kernel TIME_WAIT
   sockets, see `ss -s`) grows until requests hang. With the connection
   cache it stays flat. *)
let fd_count () =
  try Sys.readdir "/proc/self/fd" |> Array.length with Sys_error _ -> -1

let stress_get_documents partition total parallelism () =
  let start_time = Unix.gettimeofday () in
  let%lwt () =
    Lwt_io.printf "stress: %i get calls, parallelism %i\n" total parallelism
  in
  let doc_id = make_id 0 in
  let report_every = max 1000 (total / 20) in
  let rec loop remaining ok throttled fail next_report =
    if remaining <= 0 then Lwt.return (ok, throttled, fail)
    else
      let batch = min parallelism remaining in
      let%lwt results =
        List.init batch (fun _ -> ())
        |> Lwt_list.map_p (fun () ->
            let%lwt r =
              D.Collection.Document.get ~partition_key:partition dbname
                collection_name doc_id
            in
            match r with
            | Result.Ok (code, _) -> Lwt.return code
            | Result.Error _ -> Lwt.return 0)
      in
      let count code = List.filter (Int.equal code) results |> List.length in
      let succeeded = count 200 in
      let ok = ok + succeeded in
      let throttled = throttled + count 429 in
      let fail = fail + (batch - succeeded - count 429) in
      let done_count = ok + throttled + fail in
      let%lwt next_report =
        if done_count >= next_report then
          let elapsed = Unix.gettimeofday () -. start_time in
          let%lwt () =
            Lwt_io.printf
              "  %i/%i ok: %i 429: %i fail: %i fds: %i rate: %.0f req/s \
               elapsed: %.1f sec\n"
              done_count total ok throttled fail (fd_count ())
              (float_of_int done_count /. elapsed)
              elapsed
          in
          Lwt.return (next_report + report_every)
        else Lwt.return next_report
      in
      loop (remaining - batch) ok throttled fail next_report
  in
  let%lwt ok, throttled, fail = loop total 0 0 0 report_every in
  let elapsed = Unix.gettimeofday () -. start_time in
  Lwt_io.printf
    "stress done: ok: %i 429: %i fail: %i fds: %i rate: %.0f req/s time: %.1f \
     sec\n"
    ok throttled fail (fd_count ())
    (float_of_int (ok + throttled + fail) /. elapsed)
    elapsed

let stress_with_partition partition_key partition total parallelism () =
  let%lwt () = create_database () in
  let%lwt () = create_collection partition_key () in
  let%lwt _ =
    D.Collection.Document.create ~partition_key:partition dbname collection_name
      (create_value 0)
  in
  let%lwt () = stress_get_documents partition total parallelism () in
  let%lwt () = delete_database () in
  Lwt.return_unit

let main () =
  let () = print_endline "start" in
  let partition_key =
    Cosmos.Json_converter_t.
      { paths = [ "/lastName" ]; kind = "Hash"; version = None }
  in
  let partition = "a Last name" in
  match Sys.argv with
  | [| _; "stress" |] ->
      stress_with_partition partition_key partition 200_000 50 ()
  | [| _; "stress"; n |] ->
      stress_with_partition partition_key partition (int_of_string n) 50 ()
  | [| _; "stress"; n; p |] ->
      stress_with_partition partition_key partition (int_of_string n)
        (int_of_string p) ()
  | _ ->
      let%lwt () = do_with_partition partition_key partition () in
      Lwt.return_unit

let () = Lwt_main.run (main ())

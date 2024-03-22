(*
how to run:
dune exec ./src/cosmos_runner/cosmos_runner.exe   
*)

open Cosmos
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

let create_collection () =
  let res = D.Collection.create dbname collection_name in
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

let create_value counter =
  let string_counter = string_of_int counter in
  ({
     id = document_id ^ string_counter;
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
     lastName = "a Last ";
   }
    : Json_t.create_document)
  |> Json_j.string_of_create_document

let range i j =
  let rec loop acc k = if i = k then k :: acc else loop (k :: acc) (pred k) in
  loop [] j

let ids = range 0 10

let create_a_lot_of_documents () =
  let start_time = Unix.time () in
  let%lwt () = Lwt_io.print "create_a_lot_of_documents " in
  let values = List.map create_value ids in
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

let update_a_lot_of_documents_with_upsert () =
  let start_time = Unix.time () in
  let%lwt () = Lwt_io.print "update_a_lot_of_documents_with_upsert " in
  let values = List.map update_value ids in
  let%lwt result_list =
    D.Collection.Document.create_multiple ~is_upsert:true dbname collection_name
      values
  in
  let check expected_code = function
    | Result.Ok (code, _) -> code = expected_code
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

let delete_a_lot_of_documents () =
  let start_time = Unix.time () in
  let%lwt () = Lwt_io.print "delete_a_lot_of_documents " in
  let values = List.map string_of_int ids in
  let%lwt result_list =
    D.Collection.Document.delete_multiple dbname collection_name values
  in
  let check expected_code = function
    | Result.Ok code -> code = expected_code
    | _ -> false
  in
  let check_fail = function
    | Result.Error Connection_error -> true
    | _ -> false
  in
  let result_list_to_string = function
    | Result.Ok x -> string_of_int x
    | Result.Error _ -> "error"
  in
  let () =
    List.map result_list_to_string result_list |> List.iter print_endline
  in
  let results_length = List.filter (check 204) result_list |> List.length in
  let length_429 = List.filter (check 429) result_list |> List.length in
  let failures = List.filter check_fail result_list |> List.length in
  let end_time = Unix.time () in
  let time = end_time -. start_time in
  Lwt_io.printf
    "results_length: %i length_429: %i failures: %i, time: %.2f sec\n"
    results_length length_429 failures time

let delete_database () =
  let%lwt res = D.delete dbname in
  match res with
  | Result.Error _ -> Lwt_io.printf "ERROR delete_database %s\n" dbname
  | Result.Ok code -> Lwt_io.printf "delete_database %i %s\n" code dbname

let main () =
  let () = print_endline "start" in
  let%lwt () = create_database () in
  let%lwt () = create_collection () in
  let%lwt () = create_a_lot_of_documents () in
  let%lwt () = update_a_lot_of_documents_with_upsert () in
  (* Replace *)
  (* Delete *)
  (* Get??? *)
  let%lwt () = delete_a_lot_of_documents () in
  let%lwt () = delete_database () in
  Lwt.return_unit

let () = Lwt_main.run (main ())
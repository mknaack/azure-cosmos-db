open Lwt
open Cosmos
open Databases
open Json_j

module MyAuthKeys : Auth_key = struct
  let getenv s =
    match Sys.getenv_opt s with
    | None -> failwith ("Environment variable " ^ s ^ " not defined")
    | Some x -> x
  let master_key = getenv "AZURE_COSMOS_KEY"
  let endpoint = getenv "AZURE_COSMOS_ENDPOINT"
end

module D = Database(MyAuthKeys)

let dbname = "test"
let collection_name = "testCollection"
let document_id = "document_id"

let create_value =
  ({id = document_id; firstName = "A First name"; lastName = "a Last name"}: create_document)
  |> string_of_create_document

let replace_value =
  ({id = document_id; firstName = "Something different"; lastName = "a Last name"}: create_document)
  |> string_of_create_document

let create_database_test _switch () =
  let res = D.create dbname in
  res >>= fun (code, body) ->
  let _ = Alcotest.(check int) "Status same int" 201 code in
  let _ =
    match body with
    | Some {id; _} -> Alcotest.(check string) "Create name is correct" dbname id
    | None -> ()
  in
  return ()

let list_databases _ () =
  let res = D.list_databases () in
  res >>= fun (code, {_rid; databases; _count = count}) ->
  let _ = Alcotest.(check int) "Status same int" 200 code in
  let _ = Alcotest.(check int) "Count" 1 count in
  let _ = Alcotest.(check string) "Name of databases" dbname ((List.hd databases).id) in
  return ()

let get_database_test _ () =
  let res = D.get dbname in
  res >>= fun (code, body) ->
  let _ = Alcotest.(check int) "Status same int" 200 code in
  let _ =
    match body with
    | Some {_rid; id; _self; _etag; _colls; _users; _ts} ->
      Alcotest.(check string) "Name of database" dbname id
    | None -> ()
  in
  return ()

let create_collection_test _ () =
  let res = D.Collection.create dbname collection_name in
  res >>= fun (code, body) ->
  let _ = Alcotest.(check int) "Status same int" 201 code in
  let _ =
    match body with
    | Some {id; _} -> Alcotest.(check string) "Create name is correct" collection_name id
    | None -> ()
  in
  return ()

let list_collection_test _ () =
  let res = D.Collection.list dbname in
  res >>= fun (code, {rid = _; document_collections = _; count}) ->
  let _ = Alcotest.(check int) "Status same int" 200 code in
  let _ = Alcotest.(check int) "Count" 1 count in
  return ()

let get_collection_test _ () =
  let res = D.Collection.get dbname collection_name in
  res >>= fun (code, body) ->
  let _ = Alcotest.(check int) "Status same int" 200 code in
  let _ =
    match body with
    | Some Json_converter_t.{id; rid = _; self = _; etag = _; ts = _; sprocs = _; triggers = _; docs = _; udfs = _; conflicts = _; indexing_policy = _} ->
      Alcotest.(check string) "Name of database" collection_name id
    | None -> ()
  in
  return ()

let create_document_test _ () =
  let res = D.Collection.Document.create dbname collection_name create_value in
  res >>= fun (code, _) ->
  let _ = Alcotest.(check int) "Status same int" 201 code in
  return ()

let list_document_test _ () =
  let res = D.Collection.Document.list dbname collection_name in
  res >>= fun (code, values) ->
  let _ = Alcotest.(check int) "Status same int" 200 code in
  let _ =
    match values with
    | Some {rid = _; documents; count} ->
      let docs = List.map (fun (x, _) -> create_document_of_string x) documents in
      let {id; firstName; lastName} = List.hd docs in
      Alcotest.(check int) "Count field" count 1;
      Alcotest.(check int) "Count list" count (List.length documents);
      Alcotest.(check string) "id" id "document_id";
      Alcotest.(check string) "firstName" firstName "A First name";
      Alcotest.(check string) "lastName" lastName "a Last name"
    | _ ->
      Alcotest.(check int) "list_document_test fail" 1 0
  in
  return ()

let query_document_test _ () =
  let query =
    Json_converter_t.{query = "SELECT * FROM " ^ collection_name ^ " f WHERE f.firstName = @fname";
     parameters = [{name = "@fname"; value = "A First name"}]
    }
  in
  let res = D.Collection.Document.query dbname collection_name query in
  res >>= fun (code, values) ->
  let _ = Alcotest.(check int) "Status same int" 200 code in
  let _ =
    match values with
    | Some {rid = _; documents; count} ->
      let docs = List.map (fun (x, _) -> create_document_of_string x) documents in
      let {id; firstName; lastName} = List.hd docs in
      Alcotest.(check int) "Count field" count 1;
      Alcotest.(check int) "Count list" count (List.length documents);
      Alcotest.(check string) "id" id "document_id";
      Alcotest.(check string) "firstName" firstName "A First name";
      Alcotest.(check string) "lastName" lastName "a Last name"
    | _ ->
      Alcotest.(check int) "list_document_test fail" 1 0
  in
  return ()

let get_document_test _ () =
  let res = D.Collection.Document.get dbname collection_name document_id in
  res >>= fun (code, _) ->
  let _ = Alcotest.(check int) "Status same int" 200 code in
  return ()

let replace_document_test _ () =
  let res = D.Collection.Document.replace dbname collection_name document_id replace_value in
  res >>= fun (code, _) ->
  let _ = Alcotest.(check int) "Status same int" 200 code in
  return ()

let delete_document_test _ () =
  let res = D.Collection.Document.delete dbname collection_name document_id in
  res >>= fun code ->
  let _ = Alcotest.(check int) "Status same int" 204 code in
  return ()

let delete_collection_test _ () =
  let res = D.Collection.delete dbname collection_name in
  res >>= fun code ->
  let _ = Alcotest.(check int) "Status same int" 204 code in
  return ()

let delete_database_test _ () =
  let res = D.delete dbname in
  res >>= fun code ->
  let _ = Alcotest.(check int) "Status same int" 204 code in
  return ()

let test = [
  Alcotest_lwt.test_case "create database" `Slow create_database_test;
  Alcotest_lwt.test_case "list database" `Slow list_databases;
  Alcotest_lwt.test_case "get database" `Slow get_database_test;

  Alcotest_lwt.test_case "create collection" `Slow create_collection_test;
  Alcotest_lwt.test_case "list collection" `Slow list_collection_test;
  Alcotest_lwt.test_case "get collection" `Slow get_collection_test;

  Alcotest_lwt.test_case "create document" `Slow create_document_test;
  Alcotest_lwt.test_case "list document" `Slow list_document_test;
  Alcotest_lwt.test_case "query document" `Slow query_document_test;
  Alcotest_lwt.test_case "get document" `Slow get_document_test;
  Alcotest_lwt.test_case "replace document" `Slow replace_document_test;

  Alcotest_lwt.test_case "delete document" `Slow delete_document_test;
  Alcotest_lwt.test_case "delete collection" `Slow delete_collection_test;
  Alcotest_lwt.test_case "delete database" `Slow delete_database_test;
]

open Lwt
open Src
open Databases
open Json_j

module MyAuthKeys : Auth_key = struct
  let master_key = "SB1mrDcsPfPnHN2lCLYLTXDJMEqXsjvWqS2BXbvBbro94dxVHem3gyXKLPruSeMVE7ZKf36EGC5ArCkJqJaoOg=="
  let endpoint = "mknnack"
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

let test_command p expected_status =
  let status_of_header {Ocsigen_http_frame.Http_header.mode = mode; _} =
    let int_of_mode = function
      | Ocsigen_http_frame.Http_header.Query _ -> 0
      | Answer i -> i
      | Nofirstline -> 0
    in
    int_of_mode mode
  in
  let status = function
    | { Ocsigen_http_frame.frame_content = _; frame_header = http_header; frame_abort = _ } ->
      status_of_header http_header
  in
  let px = p >>=
    fun l -> let res = content l in
    res >>= fun _content ->
      let s = status l in
      let _ = Alcotest.(check int) "Status same int" expected_status s in
      return ();
  in
  px

let create_database_test _switch () =
  test_command (D.create dbname) 201

let list_databases_test _ () =
  test_command (D.list_databases ()) 200

let get_database_test _ () =
  test_command (D.get dbname) 200

let create_collection_test _ () =
  test_command (D.Collection.create dbname collection_name) 201

let list_collection_test _ () =
  test_command (D.Collection.list dbname) 200

let get_collection_test _ () =
  test_command (D.Collection.get dbname collection_name) 200

let create_document_test _ () =
  test_command (D.Collection.Document.create dbname collection_name create_value) 201

let list_document_test _ () =
  test_command (D.Collection.Document.list dbname collection_name) 200

let query_document_test _ () =
  let query =
    Json_converter_t.{query = "SELECT * FROM " ^ collection_name ^ " f WHERE f.firstName = @fname";
     parameters = [{name = "@fname"; value = "A First name"}]
    }
  in
  test_command (D.Collection.Document.query dbname collection_name query) 200

let get_document_test _ () =
  test_command (D.Collection.Document.get dbname collection_name document_id) 200

let replace_document_test _ () =
  test_command (D.Collection.Document.replace dbname collection_name document_id replace_value) 200

let delete_document_test _ () =
  test_command (D.Collection.Document.delete dbname collection_name document_id) 204

let delete_collection_test _ () =
  test_command (D.Collection.delete dbname collection_name) 204

let delete_database_test _ () =
  test_command (D.delete dbname) 204


let test = [
  Alcotest_lwt.test_case "create database" `Slow create_database_test;
  Alcotest_lwt.test_case "list database" `Slow list_databases_test;
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

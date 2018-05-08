open OUnit2
open Databases
(* open Utility *)

let printer_to_string x = x
  
let empty_list = []
let list_a = [1;2;3]

let test_list_append _ =
  let list_b = List.append empty_list [1;2;3] in
  assert_equal list_b list_a

let auth_key_test _ =
  let verb = "GET" in
  let resource_type = "dbs" in
  let resource_link = "dbs/ToDoList" in
  let date = "Thu, 27 Apr 2017 00:51:12 GMT" in
  let master_key = "dsZQi3KtZmCv1ljt3VNWNm7sQUF1y5rJfC6kv5JiwvW0EndXdDku/dkKBp8/ufDToSxLzR4y+O/0H/t4bQtVNw==" in
  let result = authorization_token_using_master_key verb resource_type resource_link date master_key in
  (* let expected_result = "type%3dmaster%26ver%3d1.0%26sig%3dc09PEVJrgp2uQRkr934kFbTqhByc7TVr3OHyqlu%2bc%2bc%3d" in *)
  let expected_result = "type%3Dmaster%26ver%3D1.0%26sig%3Dc09PEVJrgp2uQRkr934kFbTqhByc7TVr3OHyqlu%2Bc%2Bc%3D" in
  assert_equal ~printer:printer_to_string expected_result result

let tests = [
  "test_list_append" >:: test_list_append;
  "auth_key_test" >:: auth_key_test;
]


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

let do_command name  p =
  let px = p >>= content in
  let result = Lwt_main.run px in
  print_endline (name ^ ":");
  print_endline result

let create_value =
  ({id = "create_value"; firstName = "A First name"; lastName = "a Last name"}: create_document)
  |> string_of_create_document

let _ = do_command "create database" (D.create dbname)

let _ = do_command "list databases" (D.list_databases ())

let _ = do_command "get database" (D.get dbname)

let _ = do_command "create collection" (D.Collection.create dbname collection_name)

let _ = do_command "list collection" (D.Collection.list dbname)

let _ = do_command "get collection" (D.Collection.get dbname collection_name)

let _ = do_command "create document" (D.Collection.Document.create dbname collection_name create_value)

let _ = do_command "delete collection" (D.Collection.delete dbname collection_name)

let _ = do_command "delete database" (D.delete dbname)

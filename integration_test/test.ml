open Lwt
open Src
open Databases

module MyAuthKeys : Auth_key = struct
  let master_key = "SB1mrDcsPfPnHN2lCLYLTXDJMEqXsjvWqS2BXbvBbro94dxVHem3gyXKLPruSeMVE7ZKf36EGC5ArCkJqJaoOg=="
  let endpoint = "mknnack"
end

module MyAuth = Auth(MyAuthKeys)

module D = Database(MyAuth)

let dbname = "test"

let do_command name  p =
  let px = p >>= content in
  let result = Lwt_main.run px in
  print_endline (name ^ ":");
  print_endline result

let _ = do_command "create" (D.create dbname)

let _ = do_command "list_databases" (D.list_databases ())

let _ = do_command "get" (D.get dbname)

let _ = do_command "delete" (D.delete dbname)




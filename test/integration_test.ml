open Lwt
open Src
open Databases
(* open Json_j *)

module MyAuthKeys : Auth_key = struct
  let master_key = "SB1mrDcsPfPnHN2lCLYLTXDJMEqXsjvWqS2BXbvBbro94dxVHem3gyXKLPruSeMVE7ZKf36EGC5ArCkJqJaoOg=="
  let endpoint = "mknnack"
end

module D = Database(MyAuthKeys)

let dbname = "test"
let collection_name = "testCollection"
let document_id = "document_id"

let test_command p expected_status =
  let px = p >>=
    fun l -> let res = content l in
    res >>= fun _content ->
      let s = status l in
      let status_pass = s = expected_status in
      match status_pass with
      | true -> return ();
      | false -> return ();
  in
  px

let create_database_test _switch () =
  test_command (D.create dbname) "status"
  (* let () = c >>= *)
  (*   let header, _content = Lwt_switch.add_hook (Some switch) c in *)
  (* Alcotest.(check string) "Same string" "" header *)

    
let test = [
  Alcotest_lwt.test_case "create_database_test" `Slow create_database_test;
]

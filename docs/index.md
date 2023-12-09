# This is the documentation

```ocaml
let a = 8 + 9
let b = 8 + 8
```
And with another example:

```ocaml
# 1 + 2;;
- : int = 3
# "a" ^ "bc";;
- : string = "abc"
```

<!-- $MDX set-FOO=bar,set-BAR=foo -->
```ocaml
# print_endline (Sys.getenv "FOO")
bar
- : unit = ()
# print_endline (Sys.getenv "BAR")
foo
- : unit = ()
```

```ocaml
open Cosmos
open Databases
open Lwt

let master_key_env = "AZURE_COSMOS_KEY"
let endpoint_env = "AZURE_COSMOS_ENDPOINT"

module MyAuthKeys : Auth_key = struct
  let getenv s = match Sys.getenv_opt s with None -> "" | Some x -> x
  let master_key = getenv master_key_env
  let endpoint = getenv endpoint_env
end

module D = Database (MyAuthKeys)

let res = D.list_databases ()
let s =
  let res = D.list_databases () in
 res >>= function
  | Result.Ok (code, { _rid; databases; _count = count }) ->
      let db =
        List.filter
          (fun (x : Json_converter_t.database) -> x.id = dbname)
          databases
      in
      let _ = Alcotest.(check int) "Status same int" 200 code in
      let _ = Alcotest.(check bool) "Count" true (count > 0) in
      let _ =
        Alcotest.(check string) "Name of databases" dbname (List.hd db).id
      in
      return ()
  | Result.Error _ -> Alcotest.fail "Should not return error"
```

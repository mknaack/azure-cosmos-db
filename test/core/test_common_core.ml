let master_key_env = "AZURE_COSMOS_KEY"
let endpoint_env = "AZURE_COSMOS_ENDPOINT"
let run_id_env = "AZURE_COSMOS_TEST_RUN_ID"

let run_id =
  match Sys.getenv_opt run_id_env with
  | Some id when id <> "" -> id
  | None | Some _ ->
      Random.self_init ();
      Printf.sprintf "%d_%04x" (Unix.getpid ()) (Random.bits () land 0xffff)

let prefix runner = Printf.sprintf "t_%s_%s_" run_id runner
let collection_name = "testCollection"
let dbname_partition prefix = prefix ^ "testPartition"
let collection_name_partition = "testPartition"
let document_id = "document_id"

let should_run () =
  (Option.is_some @@ Sys.getenv_opt master_key_env)
  && (Option.is_some @@ Sys.getenv_opt endpoint_env)

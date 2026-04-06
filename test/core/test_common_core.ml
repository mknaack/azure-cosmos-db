let master_key_env = "AZURE_COSMOS_KEY"
let endpoint_env = "AZURE_COSMOS_ENDPOINT"
let dbname = "test"
let collection_name = "testCollection"
let dbname_partition = "testPartition"
let collection_name_partition = "testPartition"
let document_id = "document_id"

let should_run () =
  (Option.is_some @@ Sys.getenv_opt master_key_env)
  && (Option.is_some @@ Sys.getenv_opt endpoint_env)

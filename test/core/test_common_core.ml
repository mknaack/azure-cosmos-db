let master_key_env = "AZURE_COSMOS_KEY"
let endpoint_env = "AZURE_COSMOS_ENDPOINT"
let live_required_env = "COSMOS_REQUIRE_LIVE_TESTS"
let collection_name = "testCollection"
let dbname_partition prefix = prefix ^ "testPartition"
let collection_name_partition = "testPartition"
let document_id = "document_id"

let should_run () =
  (Option.is_some @@ Sys.getenv_opt master_key_env)
  && (Option.is_some @@ Sys.getenv_opt endpoint_env)

let live_required () = Sys.getenv_opt live_required_env = Some "1"

let live_wiring_test ~suite ~registered () =
  if live_required () then begin
    Alcotest.(check bool)
      (suite ^ ": AZURE_COSMOS_KEY and AZURE_COSMOS_ENDPOINT must be set")
      true (should_run ());
    Alcotest.(check bool)
      (suite ^ ": live test cases must be registered")
      true (registered > 0)
  end

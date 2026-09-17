Run tests:

Add environment variables:

export AZURE_COSMOS_KEY=[key from azure]
export AZURE_COSMOS_ENDPOINT=[endpoint of azure (all in front of .documents.azure.com)]

Integration tests create databases named `t_<run id>_<lwt|eio>_...` so that
concurrent runs against the same account do not interfere. The run id defaults
to `<pid>_<random>`; set `AZURE_COSMOS_TEST_RUN_ID` to choose it explicitly
(CI uses the GitHub run id).
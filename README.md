[![Coverage Status](https://coveralls.io/repos/github/mknaack/azure-cosmos-db/badge.svg?branch=actions)](https://coveralls.io/github/mknaack/azure-cosmos-db?branch=main)

Azure cosmos db
===============

Provides an interface to Microsoft Azure Cosmos db.

# Documentation

[Microsoft documentation](https://docs.microsoft.com/en-us/rest/api/cosmos-db/)

[API documentation](https://mknaack.github.io/azure-cosmos-db/)

# Quick start

```ocaml
module MyAuthKeys : Auth_key = struct
  let master_key = "key guid found in azure portal"
  let endpoint = "endpoint found in azure portal (e.g. endpoint.documents.azure.com)"
end

module D = Database (MyAuthKeys)
D.list_databases ()
```

# Throughput management

Offers expose provisioned throughput for databases and collections:

```ocaml
let%lwt throughput =
  D.Offer.get_throughput "my-database" "my-collection"
in
let%lwt updated =
  D.Offer.set_throughput "my-database" "my-collection"
    (D.Offer.Throughput.Manual 500)
in
match (throughput, updated) with
| Ok (_, current), Ok (_, offer) -> Lwt.return (current, offer)
| Error _error, _ | _, Error _error -> Lwt.fail_with "Cosmos request failed"
```

Offer operations require master-key authentication. Serverless accounts do not
expose provisioned-throughput offers, so lookup functions return `None`.
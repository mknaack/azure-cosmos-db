[![Coverage Status](https://coveralls.io/repos/github/mknaack/azure-cosmos-db/badge.svg?branch=actions)](https://coveralls.io/github/mknaack/azure-cosmos-db?branch=main)

Azure Cosmos DB for OCaml
=========================

An OCaml client for the [Azure Cosmos DB REST API](https://learn.microsoft.com/en-us/rest/api/cosmos-db/):
databases, collections, documents, queries, transactional batches, the change
feed, users, permissions and resource tokens, and throughput offers.

| Package | Use it when |
|---|---|
| `azure-cosmos-db-lwt` | your application uses [Lwt](https://github.com/ocsigen/lwt) |
| `azure-cosmos-db-eio` | your application uses [Eio](https://github.com/ocaml-multicore/eio) (OCaml 5) |
| `azure-cosmos-db` | shared core used by both backends; you normally don't depend on it directly |

# Install

```sh
opam install azure-cosmos-db-lwt
```

For the Eio backend, install `azure-cosmos-db-eio`.

# Example

```ocaml
open Cosmos_lwt.Databases

module Keys : Auth_key = struct
  let master_key = Sys.getenv "AZURE_COSMOS_KEY"
  let endpoint = Sys.getenv "AZURE_COSMOS_ENDPOINT"
end

module D = Database (Keys)

let () =
  Lwt_main.run
    (match%lwt D.list_databases () with
     | Ok (_, { databases; _ }) ->
         Lwt_list.iter_s
           (fun (db : Cosmos.Json_converter_t.database) -> Lwt_io.printl db.id)
           databases
     | Error _ -> Lwt_io.eprintl "Cosmos request failed")
```

With Eio the API is the same, but calls return thunks and must run inside
`Cosmos_eio.Databases.with_env`; see the documentation.

# Documentation

- [Documentation](https://mknaack.github.io/azure-cosmos-db/azure-cosmos-db/index.html):
  how-to guides, explanations and the API reference
- [Azure Cosmos DB REST API](https://learn.microsoft.com/en-us/rest/api/cosmos-db/)

Running the tests is described in [test/README.md](test/README.md).
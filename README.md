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

# Resource tokens

A resource token minted through `User`/`Permission` can be used instead of the
master key. The token is sent as-is in the `authorization` header, so only the
resources it was granted access to are reachable:

```ocaml
module D_token =
  Database_as ((val credentials_of_token ~endpoint "type=resource&ver=1&sig=..."))

(* Refresh the token on every request, e.g. from a token broker: *)
module D_refreshing =
  Database_as ((val credentials_of_token_provider ~endpoint fetch_token))
```

Permission tokens expire after about an hour by default; pass
`?expiry_seconds` (1..18000) to `Permission.create`, `Permission.get`, or
`Permission.replace` to choose the lifetime.

# Entra ID (AAD) authentication

Accounts created with `disableLocalAuth` reject the master key entirely and
only accept Microsoft Entra ID bearer tokens (`type=aad`). Unlike resource
tokens, an Entra token authorizes every operation, including
`list_databases`, `User`, `Permission` and `Offer`.

The SDK can acquire and refresh tokens itself from a service principal's
client credentials:

```ocaml
module Sp = struct
  let endpoint = "https://acct.documents.azure.com"
  let tenant_id = "…"
  let client_id = "…"
  let client_secret = "…"
end

module Db = Database_aad (Sp)
(* The first request acquires and caches a token; it is refreshed before
   expiry. *)
```

Or bring a token you obtain yourself — Azure CLI, managed identity, or a
corporate token broker:

```ocaml
(* az account get-access-token --resource https://cosmos.azure.com *)
module D_aad =
  Database_as ((val credentials_of_aad_token ~endpoint "<access token>"))

(* Callers running their own refresh loop keep the getter synchronous: *)
module D_aad_refreshing =
  Database_as ((val credentials_of_aad_token_provider ~endpoint fetch_aad_token))
```

The Entra identity needs a *data-plane* role assignment on the account, e.g.
`Cosmos DB Built-in Data Contributor` (control-plane `Reader`/`Contributor`
roles are not sufficient — requests return 401/403 without one). Token-endpoint
HTTP failures surface as `Azure_error` carrying the token endpoint's status
code; network failures are `Connection_error` or `Timeout_error`. A data-plane
401/403 stays `Azure_error`.

# Change feed

The pull-model change feed returns the latest version of changed documents.
Treat `304 Not Modified` as the normal idle result, and persist the returned
ETag checkpoint in the application:

```ocaml
let%lwt first = D.Collection.Change_feed.read
  ~start_from:D.Collection.Change_feed.Start_from.Now
  "my-database" "my-collection"
in
match first with
| Ok (304, _headers, None) -> Lwt.return_unit
| Ok (200, _headers, Some page) ->
    (* Process [page.documents], then persist [page.continuation]. *)
    let%lwt next = D.Collection.Change_feed.read
      ~start_from:
        (D.Collection.Change_feed.Start_from.Continuation page.continuation)
      "my-database" "my-collection"
    in
    ignore next;
    Lwt.return_unit
| Error _error -> Lwt.fail_with "Change feed request failed"
```

The checkpoint is the caller's responsibility. Ordering is guaranteed only
within a partition key. For independent partition-range checkpoints, enumerate
the ranges and fan out explicitly with the backend's `IO.parallel_map`:

```ocaml
let%lwt ranges = D.Collection.Partition_key_range.ids
  "my-database" "my-collection"
in
(* Keep one checkpoint per range while polling each range independently. *)
```

Latest-version mode does not surface deletes. If deletions must be represented
in the feed, use a soft-delete flag and TTL instead.

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
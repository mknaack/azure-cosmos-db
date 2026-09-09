# Implementation Plan: Entra ID (AAD) Authentication

## Overview

The SDK supports two credentials — the account master key and service-issued resource tokens — and
both derive from the account key. On an account created with
[`disableLocalAuth = true`](https://learn.microsoft.com/en-us/azure/cosmos-db/nosql/how-to-disable-key-based-authentication)
(the configuration Microsoft recommends for new secure workloads) **this SDK cannot connect at
all**. This plan adds Microsoft Entra ID (AAD) bearer authentication (`type=aad`), which is the
only credential such an account accepts, and unlike resource tokens it authorizes **every**
operation — including `list_databases`, `User.*`, `Permission.*` and `Offer.*` — closing the
401/403 caveat documented in `RESOURCE_TOKEN_AUTH_PLAN.md`.

**REST API reference:**
https://learn.microsoft.com/en-us/rest/api/cosmos-db/access-control-on-cosmosdb-resources

| Credential | `authorization` header | Signing |
|------------|------------------------|---------|
| Master key | `type=master&ver=1.0&sig={hmac-sha256}` | HMAC-SHA256 over verb/resource-type/resource-link/date |
| Resource token | `_token` verbatim | None — pre-signed by the service |
| **Entra ID** | `type=aad&ver=1.0&sig={access-token}` | None — the Entra access token goes in `sig` |

The whole header value is percent-encoded, following the same convention as the master-key path
(`type%3Daad%26ver%3D1.0%26sig%3D{jwt}` — verified against the Cosmos REST docs and independent
REST implementations). A JWT is base64url, so encoding it is a no-op in practice; encoding the
`type=...&ver=...&sig=...` prefix is what matters.

## Key API semantics (must-read before coding)

1. **No signing, no resource link, no date participation.** The `authorization` value ignores
   verb, resource type, resource link and date entirely — exactly like the resource-token case.
2. **`x-ms-date` and `x-ms-version` are still sent unchanged.** The current
   `x-ms-version: 2018-12-31` already satisfies Entra ID data-plane auth (it is the version whose
   published API description lists the Entra security scheme). If a future feature requires a
   newer version, bumping it is orthogonal to this change.
3. **Tokens expire (~1 hour).** The Entra token endpoint returns `expires_in` (seconds). The SDK
   must cache the token and refresh it before expiry — a 5-minute safety margin matches the Azure
   SDKs.
4. **Acquiring a token is an async HTTP call.** Client-credentials flow:
   `POST https://{authority}/{tenant_id}/oauth2/v2.0/token` with an
   `application/x-www-form-urlencoded` body
   `grant_type=client_credentials&client_id=..&client_secret=..&scope=https://cosmos.azure.com/.default`.
   The response JSON carries `access_token`, `token_type` and `expires_in`. Errors look like
   `{"error":"invalid_client","error_description":"..."}`.
5. **RBAC, not keys.** The Entra identity needs a *data-plane* role assignment
   (e.g. `Cosmos DB Built-in Data Contributor`, or a custom role with the required
   `dataActions`) at the account scope. Control-plane `Reader`/`Contributor` roles are **not**
   sufficient — with no data-plane assignment every request returns 401/403. This is account
   setup, not SDK code, but the README must say it.
6. **Async acquisition does not fit `Account.authorization`.** Today the signature is
   `Utilities.Verb.t -> resource -> Utilities.Ms_time.t -> string -> string` — synchronous and
   infallible (`src/cosmos/databases_core.ml:5-12`). The resource-token plan explicitly deferred
   AAD for this reason ("`Make_account` is the seam ... though that additionally requires async
   token acquisition"). Making `authorization` IO- and error-aware is the core change.

## Current state

- `Databases_intf.Credential.t` = `Master_key | Resource_token | Resource_token_provider`
  (`src/cosmos/databases_intf.ml:1-6`).
- `Databases_core.Account` is the injected seam; `Auth_credential (C)` and `Auth (Keys)`
  implement it synchronously (`src/cosmos/databases_core.ml:14-46`).
- `Make_account (IO) (Http) (Account)` builds all operations; `headers`/`json_headers` are pure
  functions called from ~19 sites (`src/cosmos/databases_core.ml:155-171` and call sites).
- Backends expose `Database (Auth_key)`, `Database_as (Credentials)`,
  `credentials_of_token`, `credentials_of_token_provider`
  (`src/cosmos_lwt/databases.ml:69-86`, `src/cosmos_eio/databases.ml:155-172`).
- Mocks exist for exactly what is needed: `Mock_io` (synchronous `IO`),
  `Mock_http` (expectation queue matching on method + host + path, header and body assertions),
  `Mock_response` (`test/core/`). The token endpoint is just another host:path, so acquisition,
  caching and refresh are fully testable without network or a real tenant.

## Architecture decisions

- **Two credential flavours, one header.** Cheap layer: `Credential.Aad_token` /
  `Aad_token_provider` for callers who obtain tokens themselves (`az account get-access-token
  --resource https://cosmos.azure.com`, managed identity, a corporate token broker). This needs
  no signature churn and can merge alone. Full layer: SDK-managed client-credentials acquisition
  with caching and refresh, which is what forces the async `Account` refactor below.
- **Extend the existing seam instead of adding a parallel one.** `Account` gains
  `type 'a io` and `authorization` returns `(string, cosmos_error) result io`. `Make_account`
  keeps its shape; `Auth_credential`/`Auth` take `(IO)` and wrap their results in
  `IO.return (Ok …)`. Public functors `Make`, `Make_credential`, `Database`, `Database_as` keep
  their signatures.
- **Token acquisition reuses the injected `Http` client**, so `Auth_aad` is testable with
  `Mock_http` and needs no new dependency. The token POST bypasses Cosmos headers,
  `with_throttle_retry` and `?timeout` (it is not a Cosmos request); a fixed internal timeout is
  applied via `IO.with_timeout`.
- **`cosmos_error` gains one constructor, `Auth_error of string`**, for credential-acquisition
  failures (non-2xx token responses, malformed token JSON). Transport failure still maps to
  `Connection_error`. A data-plane 401/403 (e.g. missing RBAC role) stays
  `Azure_error (401|403, _)` — it is a service answer, not an acquisition failure.
- **Single-flight refresh where the IO permits it.** The cache stores the in-flight
  acquisition `'a IO.t`; under Lwt that is a shared promise (true single-flight), under the Eio
  thunk encoding (`type 'a t = unit -> 'a`) each forcing re-runs — redundant-but-correct
  duplicate token requests on concurrent cold start. Accepted, documented.
- **Injected clock.** The Aad config carries `now : unit -> float` (default
  `Unix.gettimeofday` in the backends) so refresh tests are deterministic.

---

## Phase 1 — The AAD authorizer

**File:** `src/cosmos/utility.ml` (append after `authorization_token_using_resource_token`)

```ocaml
let authorization_token_using_aad_token access_token =
  Uri.pct_encode ~component:`Userinfo
    ("type=aad&ver=1.0&sig=" ^ access_token)
  |> string_replace "%3D" "%3d" |> string_replace "%2B" "%2b"
  |> string_replace "%2F" "%2f"
```

Same normalisation convention as the existing two authorizers.

---

## Phase 2 — Caller-supplied tokens (no signature churn)

**File:** `src/cosmos/databases_intf.ml`

```ocaml
module Credential = struct
  type t =
    | Master_key of string
    | Resource_token of string
    | Resource_token_provider of (unit -> string)
    | Aad_token of string
        (* an Entra access token obtained out-of-band, e.g. Azure CLI *)
    | Aad_token_provider of (unit -> string)
        (* caller keeps a refreshed cache; the getter stays synchronous,
           e.g. reading a ref that a background fiber renews *)
end
```

Two new match arms in `Auth_credential.authorization` produce
`Utility.authorization_token_using_aad_token token`.

**Backends** (`src/cosmos_lwt/databases.ml{i}`, `src/cosmos_eio/databases.ml{i}`):

```ocaml
val credentials_of_aad_token : endpoint:string -> string -> (module Credentials)
(** [credentials_of_aad_token ~endpoint token] wraps an Entra access token
    (e.g. [az account get-access-token --resource https://cosmos.azure.com])
    as credentials suitable for [Database_as]. *)

val credentials_of_aad_token_provider :
  endpoint:string -> (unit -> string) -> (module Credentials)
(** For callers running their own refresh loop (managed identity, broker). *)
```

This phase alone makes AAD accounts reachable; Phases 3-4 add SDK-managed acquisition.

---

## Phase 3 — Make `Account` asynchronous and fallible

**File:** `src/cosmos/databases_core.ml`

3a. **Reorder the file top.** `module type Account` (lines 5-12) must see `cosmos_error`, so
`Response_headers`, `batch_validation_error` and `cosmos_error` (currently lines 48-136) move
above it. `cosmos_error` gains `Auth_error of string` here.

3b. **New signature:**

```ocaml
module type Account = sig
  type 'a io
  type resource = Dbs | Colls | Docs | Users | Permissions | Offers | Pkranges

  val authorization :
    Utilities.Verb.t -> resource -> Utilities.Ms_time.t -> string ->
    (string, cosmos_error) result io

  val endpoint : string
end
```

3c. **Existing implementations wrap in `IO.return (Ok …)`:**

```ocaml
module Auth_credential
    (IO : Databases_intf.IO)
    (C : Databases_intf.Credentials) :
    Account with type 'a io = 'a IO.t = struct
  (* string_of_resource unchanged *)
  let authorization verb resource date db_name =
    IO.return
      (Ok
         (match C.credential with
         | Databases_intf.Credential.Master_key key -> … as today …
         | Databases_intf.Credential.Resource_token t -> …
         | Databases_intf.Credential.Resource_token_provider f -> …
         | Databases_intf.Credential.Aad_token t
         | Databases_intf.Credential.Aad_token_provider … -> aad authorizer …))
end

module Auth (IO : Databases_intf.IO) (Keys : Databases_intf.Auth_key) =
  Auth_credential (IO) (struct … end)
```

3d. **`Make_account` picks up the constraint and a result-binding operator:**

```ocaml
module Make_account
    (IO : Databases_intf.IO)
    (Http : Databases_intf.Http_client with type 'a io := 'a IO.t)
    (Account : Account with type 'a io := 'a IO.t) =
struct
  let ( let* ) = IO.bind

  let ( let** ) m f =
    IO.bind m (function
      | Ok v -> f v
      | Error e -> IO.return (Error e))

  let headers resource verb db_name =
    let ms_date = Utilities.Ms_time.create_now () in
    let* authz = Account.authorization verb resource ms_date db_name in
    IO.return
      (Result.map
         (fun authz ->
           Cohttp.Header.init ()
           |> Cohttp.Header.add'… (* as today: authorization, x-ms-version,
                                    x-ms-date *))
         authz)

  let json_headers resource verb db_name =
    let** h = headers resource verb db_name in
    IO.return (Ok (Cohttp.Header.add h "content_type" "application/json"))
  …
```

`let**` keeps each of the ~19 call sites a one-line edit, e.g.

```ocaml
let list_databases ?timeout () =
  let uri = make_uri "dbs" in
  let** hdrs = headers Account.Dbs Utilities.Verb.Get "" in
  let* response = Http.get ~headers:hdrs uri |> wrap_timeout timeout in
  …
```

Sites that post-process headers (`apply_a_im_to_header_if_some`, `apply_start_from`,
`apply_scope`, `x-ms-documentdb-expiry-seconds`, query/changefeed headers) bind first with
`let**`, then apply the existing combinators to the unwrapped `Cohttp.Header.t`.

3e. **Shims (public signatures unchanged):**

```ocaml
module Make_credential (IO) (Http …) (C : Databases_intf.Credentials) =
  Make_account (IO) (Http) (Auth_credential (IO) (C))

module Make (IO) (Http …) (Auth_key : Databases_intf.Auth_key) =
  Make_account (IO) (Http) (Auth (IO) (Auth_key))
```

Eio note: `cosmos_eio` uses `type 'a t = unit -> 'a` lazy thunks with the environment captured in
refs by `with_env` (`src/cosmos_eio/databases.ml:15-50`). Token acquisition inside a forced thunk
runs under the ambient `switch`/`net` — no Eio plumbing change is needed, because acquisition only
ever happens while serving a request that is already running under `with_env`.

---

## Phase 4 — SDK-managed client credentials (`type=aad` with refresh)

**File:** `src/cosmos/databases_intf.ml`

```ocaml
module type Aad_client = sig
  val endpoint : string        (* Cosmos account endpoint *)
  val tenant_id : string
  val client_id : string
  val client_secret : string
  val scope : string           (* default https://cosmos.azure.com/.default *)
  val authority_host : string  (* default https://login.microsoftonline.com *)
  val now : unit -> float      (* clock; injected for tests *)
end
```

**File:** `src/cosmos/databases_core.ml`

```ocaml
module Auth_aad
    (IO : Databases_intf.IO)
    (Http : Databases_intf.Http_client with type 'a io := 'a IO.t)
    (A : Databases_intf.Aad_client) :
    Account with type 'a io = 'a IO.t = struct
  type resource = Dbs | Colls | Docs | Users | Permissions | Offers | Pkranges

  let token_uri =
    Uri.of_string
      (Printf.sprintf "%s/%s/oauth2/v2.0/token" A.authority_host A.tenant_id)

  let token_body =
    Uri.encoded_of_query
      [ ("grant_type", [ "client_credentials" ]);
        ("client_id", [ A.client_id ]);
        ("client_secret", [ A.client_secret ]);
        ("scope", [ A.scope ]) ]

  let token_headers =
    Cohttp.Header.init_with "Content-Type"
      "application/x-www-form-urlencoded"

  let refresh_margin_seconds = 300.
  let token_request_timeout = 30.

  type cache = {
    mutable token : string option;
    mutable expires_at : float;
    mutable in_flight : (string, cosmos_error) result IO.t option;
  }
  let cache = { token = None; expires_at = 0.; in_flight = None }

  let parse_token_response body =
    try
      let open Yojson.Safe.Util in
      let json = Yojson.Safe.from_string body in
      let token = json |> member "access_token" |> to_string in
      let expires_in =
        match json |> member "expires_in" with
        | `Int i | `Intlit _ -> float_of_int (to_int (member "expires_in" json))
        | `String s -> (try float_of_string s with _ -> 3600.)
        | _ -> 3600.
      in
      Ok (token, expires_in)
    with _ -> Error (Auth_error ("malformed token response: " ^ body))

  let acquire () : (string, cosmos_error) result IO.t =
    (* single-flight: reuse in_flight if set; clear it once resolved *)
    let* response =
      IO.with_timeout token_request_timeout
        (Http.post ~headers:token_headers ~body:token_body token_uri)
    in
    match response with
    | None -> IO.return (Error Timeout_error)
    | Some (Error Connection_refused) -> IO.return (Error Connection_error)
    | Some (Error (Other_error exn)) ->
        IO.return (Error (Auth_error (Printexc.to_string exn)))
    | Some (Ok (resp, body)) ->
        let code =
          resp |> Cohttp.Response.status |> Cohttp.Code.code_of_status in
        if code / 100 = 2 then
          match parse_token_response body with
          | Ok (token, expires_in) ->
              cache.token <- Some token;
              cache.expires_at <- A.now () +. expires_in;
              IO.return (Ok token)
          | Error e -> IO.return (Error e)
        else
          IO.return
            (Error
               (Auth_error
                  (Printf.sprintf "token endpoint returned %d: %s" code body)))

  let get_token () =
    match cache.token with
    | Some t when A.now () +. refresh_margin_seconds < cache.expires_at ->
        IO.return (Ok t)
    | _ -> acquire ()   (* via cache.in_flight for Lwt single-flight *)

  let authorization _verb _resource _date _db_name =
    let* res = get_token () in
    IO.return
      (Result.map
         (fun tok -> Utility.authorization_token_using_aad_token tok)
         res)

  let endpoint = A.endpoint
end

module Make_aad
    (IO : Databases_intf.IO)
    (Http : Databases_intf.Http_client with type 'a io := 'a IO.t)
    (A : Databases_intf.Aad_client) =
  Make_account (IO) (Http) (Auth_aad (IO) (Http) (A))
```

Notes for the implementer:
- `expires_in` is an int in practice but is occasionally serialised as a string; parse both.
- Keep the `in_flight` slot clearing simple: set it before awaiting, clear in a `finally`/match
  after resolution.
- Optional follow-up (record, don't build): on a data-plane `Azure_error (401, _)` under Aad,
  invalidate `cache.token` and retry once — covers externally-revoked tokens.

**Backends** (`src/cosmos_lwt/databases.ml{i}`, `src/cosmos_eio/databases.ml{i}`):

```ocaml
module type Aad = sig
  val endpoint : string
  val tenant_id : string
  val client_id : string
  val client_secret : string
end

module Database_aad (A : Aad) : S =
  Cosmos.Databases_core.Make_aad (Lwt_io) (Lwt_http)
    (struct
      let endpoint = A.endpoint
      let tenant_id = A.tenant_id
      let client_id = A.client_id
      let client_secret = A.client_secret
      let scope = "https://cosmos.azure.com/.default"
      let authority_host = "https://login.microsoftonline.com"
      let now = Unix.gettimeofday
    end)

val aad_client :
  ?scope:string -> ?authority_host:string -> ?now:(unit -> float) ->
  endpoint:string -> tenant_id:string -> client_id:string -> client_secret:string ->
  unit -> (module Cosmos.Databases_intf.Aad_client)
(** For sovereign clouds / custom scope / injected clock; Database_aad covers the common case. *)
```

Usage:

```ocaml
module Sp = struct
  let endpoint = "https://acct.documents.azure.com"
  let tenant_id = "…"
  let client_id = "…"
  let client_secret = "…"
end
module Db = Database_aad (Sp)

let%lwt res = Db.Collection.Document.get ~partition_key db coll doc_id
(* first request acquires+ caches the token; later requests reuse it *)
```

---

## Phase 5 — `cosmos_error` propagation

`Auth_error of string` is added to `cosmos_error` in `databases_core.ml`; every explicit
re-export and exhaustive match must add the constructor:

- `src/cosmos_lwt/databases.ml` and `src/cosmos_lwt/databases.mli`
  (`type cosmos_error = Cosmos.Databases_core.cosmos_error = | …`)
- `src/cosmos_eio/databases.ml` and `src/cosmos_eio/databases.mli`
- `src/cosmos_lwt/database_intf.ml`, `src/cosmos_eio/database_intf.ml` if they restate it
- any `match … with` on `cosmos_error` in `test/` (grep for `Azure_error (`)

---

## Phase 6 — Tests

**New file:** `test/core/aad_auth_tests.ml`, registered in `test/lwt/test.ml` and
`test/eio/test.ml` next to `mock tests` (`Quick`, HTTP-free, no `should_run ()` guard). Built on
`Mock_io` + `Mock_http` + `Mock_response`; the token endpoint is an ordinary expectation —
`POST` to host `login.microsoftonline.com`, path `/{tenant}/oauth2/v2.0/token`.

**Header shape (pure, no IO):**

| Test | Assertion |
|------|-----------|
| `aad_header_shape` | `authorization = pct_encode("type=aad&ver=1.0&sig=" ^ jwt)` with the `%3d/%2b/%2f` lowercasing |
| `aad_header_ignores_verb_and_path` | identical header across `Get`/`Post` and different resource links |
| `aad_request_keeps_ms_headers` | `x-ms-date` and `x-ms-version: 2018-12-31` still present |
| `master_key_header_unchanged` | regression: `Database (Auth_key)` byte-identical output |

**Caller-supplied tokens:**

| Test | Assertion |
|------|-----------|
| `aad_token_reaches_the_wire` | `Database_as` + `credentials_of_aad_token` + mock 200 → `Ok`; recorded header contains the jwt |
| `aad_token_provider_called_per_request` | provider `ref` counter fires once per request; rotated value appears in the second header |

**SDK-managed acquisition (`Make_aad` on `Mock_io`/`Mock_http`, injected `now` ref):**

| Test | Assertion |
|------|-----------|
| `first_request_acquires_token` | request → token POST → data request carrying `type%3Daad…sig%3D<issued jwt>` |
| `token_cached_across_requests` | two document GETs consume exactly one token expectation (`Mock_http.verify` proves no extra) |
| `token_refreshes_after_expiry` | `expires_in: 1`, advance `now` past the margin → second GET issues a second token POST |
| `token_endpoint_http_error` | token endpoint 401 → `Error (Auth_error _)`; the data request is never issued (recorded count = 1) |
| `token_endpoint_malformed_body` | 200 with `{}` → `Error (Auth_error _)` |
| `token_endpoint_connection_refused` | mock `Connection_refused` → `Error Connection_error` |
| `non_aad_credentials_unaffected` | master-key and resource-token suites still pass unchanged |

**Live integration (optional, `Slow`, env-gated):** a real tenant + service principal +
RBAC-assigned Cosmos account, ideally one with `disableLocalAuth`. Env vars
`COSMOS_AAD_ENDPOINT` / `COSMOS_AAD_TENANT_ID` / `COSMOS_AAD_CLIENT_ID` /
`COSMOS_AAD_CLIENT_SECRET`; skip when unset. Manual recipe to keep in the file header:

```
az ad sp create-for-rbac --name cosmos-ocaml-sdk --skip-assignment
az cosmosdb sql role assignment create \
  --account-name <acct> --resource-group <rg> \
  --scope "/" --principal-id <sp-object-id> \
  --role-definition-name "Cosmos DB Built-in Data Contributor"
```

---

## Phase 7 — Docs

- `README.md`: Entra ID section — `Database_aad` (client credentials), `Database_as` +
  `credentials_of_aad_token*` (bring-your-own), and the RBAC requirement ("assign a data-plane
  role such as Cosmos DB Built-in Data Contributor; control-plane roles do not apply").
- `docs/API_IMPROVEMENTS.md`: flip the Authentication rows/coverage bar, mark item 4 in
  Phase 1 implemented, record the plan link (as this file does for its predecessors).
- `.windsurfrules`: if it still names `Make`/`Auth` as the implementation point, update it to
  `Make_account` + the `Account` seam (the resource-token change already noted this).

---

## Backward compatibility checklist

| Surface | Status |
|---------|--------|
| `Databases_intf.Auth_key`, `Credentials` | unchanged |
| `Credential.t` | two added constructors — breaks exhaustive `match` on it (only `Auth_credential` matches today) |
| `Databases_core.Account` | **breaking for external implementations**: gains `type 'a io`, `authorization` returns `(string, cosmos_error) result io`. The seam was added last iteration; only `Auth_credential`/`Auth`/`Auth_aad` implement it |
| `Databases_core.Auth`, `Auth_credential` | gain a leading `(IO)` parameter (internal functors) |
| `Make`, `Make_credential`, `Make_account` entry points | same parameter shapes; `Make_account`'s third argument gains the `with type 'a io := 'a IO.t` constraint |
| `Database`, `Database_as` in both backends | unchanged |
| `cosmos_error` | new `Auth_error` constructor — breaks exhaustive matches (re-export sites listed in Phase 5) |
| Headers for existing credentials | byte-identical — covered by regression test |
| `x-ms-version` / `x-ms-date` | unchanged |

## Explicitly out of scope (first iteration)

- **Other Entra flows**: managed identity, Azure CLI/PowerShell credential, interactive/device
  code, client certificates, federated credentials. `Aad_token(_provider)` already covers them
  via bring-your-own; `cosmos_azure_identity`-style helpers can come later.
- **401-forced refresh & retry** (token revoked before `expires_in`): noted in Phase 4.
- **Resource-level RBAC scoping helpers** (e.g. `cosmosdb sql role definition` management) —
  control plane, not data plane.
- **Connection pooling / client abstraction** — unchanged by this plan (improvement 2).

## References

- [Access control on Cosmos DB resources](https://learn.microsoft.com/en-us/rest/api/cosmos-db/access-control-on-cosmosdb-resources) — `type=aad` header shape
- [Configure role-based access control with Microsoft Entra ID](https://learn.microsoft.com/en-us/azure/cosmos-db/nosql/security/how-to-grant-data-plane-role-based-access) — RBAC roles and `dataActions`
- [OAuth 2.0 client credentials flow](https://learn.microsoft.com/en-us/entra/identity-platform/v2-oauth2-client-creds-grant-flow) — token endpoint
- [Disable key-based authorization](https://learn.microsoft.com/en-us/azure/cosmos-db/nosql/how-to-disable-key-based-authentication) — the account configuration this unlocks
- [`RESOURCE_TOKEN_AUTH_PLAN.md`](RESOURCE_TOKEN_AUTH_PLAN.md) — the `Make_account`/`Credential` seam this builds on

# Implementation Plan: Resource Token Authentication ("connect as a user")

## Overview

The SDK can **mint** resource tokens (`User` + `Permission` modules) but cannot **use** them. There
is no way to open a connection that acts as a Cosmos *user* with the restricted rights granted by a
permission — the only supported credential is the account master key. This plan closes that gap in a
fully backward-compatible way.

**REST API reference:**
https://learn.microsoft.com/en-us/rest/api/cosmos-db/access-control-on-cosmosdb-resources

| Credential | `authorization` header | Signing |
|------------|------------------------|---------|
| Master key | `type=master&ver=1.0&sig={hmac-sha256}` | HMAC-SHA256 over verb/resource-type/resource-link/date |
| Resource token | `type=resource&ver=1&sig={hash}` | None — the service already signed it; send `_token` verbatim |

## Current state — why it is impossible today

1. `Databases_intf.Auth_key` only exposes `master_key` and `endpoint`
   (`src/cosmos/databases_intf.ml:1-4`). There is no other credential shape.
2. `Databases_core.Make` takes an `Auth_key` and builds `Auth (Auth_key)` internally
   (`src/cosmos/databases_core.ml:122-129`). The `Account` module type exists, but there is **no
   seam to inject a different implementation of it**.
3. `Auth.authorization` unconditionally calls `Utility.authorization_token_using_master_key`
   (`src/cosmos/databases_core.ml:25-31`), which hard-codes `let master_token = "master"`
   (`src/cosmos/utility.ml:15-39`).
4. Both backends only expose `Database (Auth_key)`
   (`src/cosmos_lwt/databases.ml:66-67`, `src/cosmos_eio/databases.ml:152-153`).

Smuggling a resource token in as `master_key` does not work either: the signer starts with
`Base64.decode_exn master_key`, and a `_token` value (`type=resource&ver=1&sig=...`) is not valid
base64, so every request raises `Invalid_argument "Malformed input"`.

The token itself is already reachable — `permission._token` is mapped in
`src/cosmos/json_converter.atd:119` and `test/core/permission_tests.ml` already destructures it —
so only the *consuming* side is missing.

## Key API semantics (must-read before coding)

1. **No signing.** For a resource token the header value is the `_token` string percent-encoded;
   verb, resource type, resource link and date play no part. (Verified against the Python SDK's
   `__get_authorization_token_using_resource_token`, which returns the token verbatim and lets the
   transport URL-encode the whole header.)
2. **`x-ms-date` and `x-ms-version` are still required** on every request, exactly as today.
3. **Tokens expire.** Default validity is 1 hour, max 5 hours via `x-ms-documentdb-expiry-seconds`
   on `Permission` create/get/replace — a header the SDK does not send today (zero hits for
   `expiry` under `src/`). A new token is issued on every `GET`/`POST`/`PUT` of the permission.
4. **A resource token is scoped to one resource.** Master-key-only operations
   (`list_databases`, `User.*`, `Permission.*`, `Offer.*`) return `401`/`403` under a resource
   token. `OFFERS_PLAN.md:31` already records this for offers.
5. **`type=resource&ver=1`** — note `ver=1`, not `ver=1.0` as in the master-key token. The SDK never
   constructs this string; it comes from the service inside `_token`.
6. Because the token is opaque and pre-signed, the existing `Account` module type does **not** need
   to change: `authorization` simply ignores its arguments in the resource-token case.

## Architecture decisions

- Add a **credential variant type**, keep `Auth_key` untouched, and re-express `Auth` in terms of
  the new implementation. Existing user code keeps compiling and produces byte-identical headers.
- Introduce `Make_account` (parameterised by `Account`) as the real functor body and turn the
  current `Make` into a thin shim. This is the seam that also unlocks Entra ID (`type=aad`) later.
- `Credential.t` is a type with operations, so it lives in a **module with `type t`**, per project
  convention.
- No new `cosmos_error` variants — auth failures are already `Azure_error (401 | 403, _)`.
- No new generic helpers in `databases_core.ml`; the new authorizer lives in `utility.ml` next to
  the master-key one and reuses its `%3d`/`%2b`/`%2f` lowercasing convention.
- Backends get a **sibling functor**, not a changed one: `Database_as` alongside `Database`.

---

## Phase 1 — Credential abstraction

**File:** `src/cosmos/databases_intf.ml` (prepend, before `Auth_key`)

```ocaml
module Credential = struct
  type t =
    | Master_key of string
    | Resource_token of string
    | Resource_token_provider of (unit -> string)
        (* resource tokens expire after 1-5h; a provider lets callers refresh
           without re-instantiating the functor *)
end

module type Auth_key = sig
  (* unchanged *)
  val master_key : string
  val endpoint : string
end

module type Credentials = sig
  val credential : Credential.t
  val endpoint : string
end
```

`Auth_key` is deliberately left in place — it is referenced by both backend `.mli` files, the
README, `cosmos_runner`, and every test entry point.

---

## Phase 2 — The resource-token authorizer

**File:** `src/cosmos/utility.ml` (append after
`authorization_token_using_master_key`)

```ocaml
let authorization_token_using_resource_token token =
  Uri.pct_encode ~component:`Userinfo token
  |> string_replace "%3D" "%3d" |> string_replace "%2B" "%2b"
  |> string_replace "%2F" "%2f"
```

Same normalisation as the master-key path, so both credentials produce headers in one consistent
encoding.

---

## Phase 3 — Split the `Account` implementation

**File:** `src/cosmos/databases_core.ml` (lines ~14-34)

```ocaml
module Auth_credential (C : Databases_intf.Credentials) : Account = struct
  type resource = Dbs | Colls | Docs | Users | Permissions | Offers

  let string_of_resource = function
    | Dbs -> "dbs"
    | Colls -> "colls"
    | Docs -> "docs"
    | Users -> "users"
    | Permissions -> "permissions"
    | Offers -> "offers"

  let authorization verb resource date db_name =
    match C.credential with
    | Databases_intf.Credential.Master_key key ->
        Utility.authorization_token_using_master_key
          (Utilities.Verb.string_of_verb verb)
          (string_of_resource resource) db_name
          (Utilities.Ms_time.x_ms_date date) key
    | Databases_intf.Credential.Resource_token token ->
        Utility.authorization_token_using_resource_token token
    | Databases_intf.Credential.Resource_token_provider f ->
        Utility.authorization_token_using_resource_token (f ())

  let endpoint = C.endpoint
end

(* preserved for backward compatibility - now a thin shim *)
module Auth (Keys : Databases_intf.Auth_key) : Account =
  Auth_credential (struct
    let credential = Databases_intf.Credential.Master_key Keys.master_key
    let endpoint = Keys.endpoint
  end)
```

---

## Phase 4 — Make the core functor accept an `Account`

**File:** `src/cosmos/databases_core.ml` (line ~122)

Rename the existing functor and add two shims. **The functor body is moved verbatim** — the only
edit inside it is deleting `module Account = Auth (Auth_key)`, since `Account` now arrives as a
parameter.

```ocaml
module Make_account
    (IO : Databases_intf.IO)
    (Http : Databases_intf.Http_client with type 'a io := 'a IO.t)
    (Account : Account) =
struct
  let ( let* ) = IO.bind
  (* ... existing body unchanged ... *)
end

module Make_credential
    (IO : Databases_intf.IO)
    (Http : Databases_intf.Http_client with type 'a io := 'a IO.t)
    (C : Databases_intf.Credentials) =
  Make_account (IO) (Http) (Auth_credential (C))

module Make
    (IO : Databases_intf.IO)
    (Http : Databases_intf.Http_client with type 'a io := 'a IO.t)
    (Auth_key : Databases_intf.Auth_key) =
  Make_account (IO) (Http) (Auth (Auth_key))
```

`Make` keeps its exact signature, so `Mock_test_runner`, `cosmos_lwt`, `cosmos_eio` and
`cosmos_runner` are unaffected.

Note: `.windsurfrules` documents `Make(IO)(Http)(Auth_key)` as the home of all database operations
— update that rule to name `Make_account` as the implementation and `Make` / `Make_credential` as
the entry points.

---

## Phase 5 — Backend entry points

**Files:** `src/cosmos_lwt/databases.ml` + `.mli`, `src/cosmos_eio/databases.ml` + `.mli`

```ocaml
module type Auth_key = Cosmos.Databases_intf.Auth_key       (* unchanged *)
module type Credentials = Cosmos.Databases_intf.Credentials
module Credential = Cosmos.Databases_intf.Credential

module Database (Auth : Auth_key) =                          (* unchanged *)
  Cosmos.Databases_core.Make (Lwt_io) (Lwt_http) (Auth)

module Database_as (C : Credentials) =
  Cosmos.Databases_core.Make_credential (Lwt_io) (Lwt_http) (C)
```

In the `.mli`, `Database_as` needs the same `sig ... end` body as `Database`. That signature is
~430 lines duplicated per backend today, so factor it into a named module type first:

```ocaml
module type S = sig ... end        (* the current body of Database's signature *)
module Database (Auth_key : Auth_key) : S
module Database_as (C : Credentials) : S
```

This is a pure refactor of the interface — `Database`'s exposed API is unchanged.

Add a first-class-module helper so callers do not hand-write a struct per token:

```ocaml
val credentials_of_token : endpoint:string -> string -> (module Credentials)
(** [credentials_of_token ~endpoint token] wraps a permission's [_token] as
    credentials suitable for [Database_as]. *)

val credentials_of_token_provider :
  endpoint:string -> (unit -> string) -> (module Credentials)
```

Usage — the flow that should have existed all along:

```ocaml
module Admin = Database (MyMasterKey)

let as_user () =
  let%lwt res = Admin.Permission.get ~dbname ~user_name ~permission_name () in
  match res with
  | Error e -> Lwt.return (Error e)
  | Ok (_, perm) ->
      let (module C) = credentials_of_token ~endpoint perm.token in
      let module As_user = Database_as (C) in
      As_user.Collection.Document.get ~partition_key dbname coll_name doc_id
```

---

## Phase 6 — Token lifetime control (`x-ms-documentdb-expiry-seconds`)

**File:** `src/cosmos/databases_core.ml`, `Permission` module

Without this header the SDK is stuck with the 1-hour default, which makes resource tokens hard to
use in practice. Additive optional argument, wired with the existing helper:

```ocaml
let create ?timeout ?expiry_seconds ~dbname ~user_name ~coll_name
    permission_mode ~permission_name =
  ...
  let hdrs =
    json_headers resource Utilities.Verb.Post
      (Printf.sprintf "dbs/%s/users/%s" dbname user_name)
    |> Utilities.apply_to_header_if_some "x-ms-documentdb-expiry-seconds"
         string_of_int expiry_seconds
  in
```

Same for `Permission.get` and `Permission.replace` (a new token is issued on each of those).
Valid range is 1..18000; values above 18000 are rejected by the service with `400`.

---

## Phase 7 — Tests

**New file:** `test/core/resource_token_tests.ml` (add to the `test_core` library — the `dune`
stanza needs no change since it globs modules; only the runners register the test list).

Built on the existing `Mock_http` / `Mock_response` harness, mirroring
`mock_tests.ml:225-259` which already asserts on the master-key header.

**Header shape (pure, no IO):**

| Test | Assertion |
|------|-----------|
| `resource_token_header_is_pct_encoded_token` | `authorization = Uri.pct_encode ~component:`Userinfo token` with `%3d/%2b/%2f` lowercased |
| `resource_token_header_has_no_master_type` | header contains `type%3dresource`, never `type%3dmaster` |
| `resource_token_header_ignores_verb_and_path` | header identical across `Get`/`Post` and different resource links |
| `resource_token_request_keeps_ms_headers` | `x-ms-date` and `x-ms-version: 2018-12-31` still present |
| `master_key_header_unchanged` | regression: `Database (Auth_key)` output is byte-identical to today |

**Behaviour through the mock HTTP client:**

| Test | Assertion |
|------|-----------|
| `document_get_with_resource_token` | `Database_as` + mock 200 → `Ok (200, body)`; proves the token reaches the wire |
| `document_get_forbidden` | mock 403 → `Error (Azure_error (403, _))` (token does not cover the resource) |
| `document_get_unauthorized` | mock 401 → `Error (Azure_error (401, _))` (expired token) |
| `document_get_timeout` | `~timeout:0.0` → `Error Timeout_error` |
| `document_get_connection_refused` | mock `Connection_refused` → `Error Connection_error` |
| `token_provider_called_per_request` | `Resource_token_provider` backed by a `ref` counter is invoked once per request, and a rotated token appears in the second request's header |

**`Permission` expiry:**

| Test | Assertion |
|------|-----------|
| `permission_create_sends_expiry_header` | `?expiry_seconds:3600` → `x-ms-documentdb-expiry-seconds: 3600` |
| `permission_create_omits_expiry_header` | header absent when the argument is omitted |

**Fixture:** extend `test/core/mock_auth.ml` with a resource-token credential (keeping
`Mock_auth.Auth` as-is):

```ocaml
let sample_resource_token =
  "type=resource&ver=1&sig=m32/kQcHRQ4dSIiRZUFPRA==;token-body=="

module Resource_token_auth : Cosmos.Databases_intf.Credentials = struct
  let credential =
    Cosmos.Databases_intf.Credential.Resource_token sample_resource_token

  let endpoint = "mock-account.documents.azure.com"
end
```

Register the new list in `test/lwt/test.ml` and `test/eio/test.ml` next to `mock tests`
(these are `Quick`, HTTP-free tests, so they run unconditionally — no `should_run ()` guard).

**Integration test (optional, `Slow`, master key required):** create db → collection → user →
permission → read the document with the returned token → assert the same read is rejected `403`
for a document outside the permission's scope, and that `Offer.list` through the token fails.

---

## Backward compatibility checklist

| Surface | Status |
|---------|--------|
| `Databases_intf.Auth_key` | unchanged |
| `Databases_core.Make` signature | unchanged (now a shim over `Make_account`) |
| `Databases_core.Auth` | unchanged signature (now a shim over `Auth_credential`) |
| `Utility.authorization_token_using_master_key` | unchanged |
| `Database (Auth_key)` in both backends | unchanged; header bytes identical (regression test above) |
| Backend `.mli` | `Database`'s signature body extracted to `module type S` — same API |
| `Permission.*` | new optional `?expiry_seconds`, existing call sites unaffected |
| README / `cosmos_runner` / all tests | compile untouched |

## Explicitly out of scope (first iteration)

- **Compile-time separation of master-key-only operations.** Splitting the module signature so that
  `Offer.*` / `User.*` / `Permission.*` are unavailable under a resource token would be a breaking
  change. Keep it a documented runtime behaviour (401/403), matching the existing "Requires
  master-key authentication" doc comments in the `.mli` files.
- **Entra ID / AAD (`type=aad`) bearer tokens.** `Make_account` is the seam that makes this
  possible later, but AAD needs asynchronous token acquisition and refresh, which does not fit
  `Account.authorization : ... -> string`.
- **Automatic token refresh / permission caching** (a `resource_tokens` map keyed by resource path,
  as the Python SDK has). `Resource_token_provider` lets callers implement this themselves.
- **Token broker helpers** (the "resource token broker" pattern for mobile clients).

## References

- [Access control on Cosmos DB resources](https://learn.microsoft.com/en-us/rest/api/cosmos-db/access-control-on-cosmosdb-resources)
- [Operations on Permissions](https://learn.microsoft.com/en-us/rest/api/cosmos-db/permissions)
- [Get a Permission](https://learn.microsoft.com/en-us/rest/api/cosmos-db/get-a-permission) — `_token` shape and `x-ms-documentdb-expiry-seconds`
- [Secure access to data in Azure Cosmos DB](https://learn.microsoft.com/en-us/azure/cosmos-db/secure-access-to-data) — resource token model

# Implementation Plan: Offers (Throughput Management) for Azure Cosmos DB

## Overview

Offers are account-level resources that describe the provisioned throughput (RU/s) of a
database or a collection. Supporting them unlocks programmatic throughput reads, scaling
(manual and autoscale), and cost monitoring — currently 0% covered by this SDK.

**REST API reference:** https://learn.microsoft.com/en-us/rest/api/cosmos-db/offers

| Operation | HTTP | Path | Success |
|-----------|------|------|---------|
| List offers | `GET` | `/offers` | 200 |
| Get offer | `GET` | `/offers/{offer_rid}` | 200 |
| Replace offer | `PUT` | `/offers/{offer_rid}` | 200 |
| Query offers | `POST` | `/offers` | 200 |

Offers cannot be created or deleted directly — they are created/removed by the service when a
database or collection is created/deleted.

## Key API semantics (must-read before coding)

1. **Resource type is `offers`** — a new variant is required in `Account.resource`.
2. **The authorization `ResourceLink` for a single offer is the offer `_rid` *lowercased*.**
   This is unique to offers; every other resource uses the path verbatim. Getting this wrong
   yields `401 Unauthorized`. The URI path itself uses the `_rid` with original casing.
3. **List and query use an empty `ResourceLink`** (like `list_databases`).
4. **Replace requires the complete offer document**, not a patch: `id`, `_rid`, `_self`,
   `offerVersion`, `offerType`, `resource`, `offerResourceId` must be echoed back with the
   modified `content`. Therefore `replace` takes a previously fetched offer value.
5. **Offers require master-key auth.** Resource tokens cannot read offers — document this.
6. **Finding the offer for a container** requires the container's `_rid`:
   `SELECT * FROM c WHERE c.offerResourceId = @rid`. The `collection` ATD type already exposes
   `rid`, so `Collection.get` gives the input for that query.
7. **Manual vs autoscale throughput:**
   - Manual: `content.offerThroughput = 400`
   - Autoscale: `content.offerAutopilotSettings.maxThroughput = 4000`
   - Switching mode requires a migration header:
     `x-ms-cosmos-migrate-offer-to-autopilot: true` or
     `x-ms-cosmos-migrate-offer-to-manual-throughput: true`

Example offer document (V2):

```json
{
  "id": "GpFA",
  "_rid": "GpFA",
  "_self": "offers/GpFA/",
  "_etag": "\"00000000-0000-0000-0000-000000000000\"",
  "_ts": 1700000000,
  "offerVersion": "V2",
  "offerType": "Invalid",
  "resource": "dbs/1KsSAA==/colls/1KsSAKxWxxo=/",
  "offerResourceId": "1KsSAKxWxxo=",
  "content": { "offerThroughput": 400, "offerIsRUPerMinuteThroughputEnabled": false }
}
```

## Architecture decisions

- Follow the existing **functor pattern**: new `Offer` module inside `Make (IO) (Http) (Auth_key)`
  in `src/cosmos/databases_core.ml`, as a **top-level sibling of `User` and `Permission`**
  (offers are account-scoped, not collection-scoped — do **not** nest under `Collection`).
- **ATD for all JSON** (`src/cosmos/json_converter.atd`) — no hand-rolled JSON; offer payloads
  have a fixed shape.
- Throughput mode is a type with operations, so it lives in a **nested module with `type t`**
  (`Offer.Throughput`), per project convention.
- Reuse existing helpers only: `make_uri`, `headers`/`json_headers`, `add_header`,
  `Utilities.apply_to_header_if_some`, `handle_response`, `result_or_error_with_result`,
  `with_throttle_retry`. No new generic helpers.
- No new `cosmos_error` variants are needed; a missing offer maps to `Ok (code, None)` in the
  convenience lookups and `Azure_error 404` for direct `get`.

---

## Phase 1 — Add the `Offers` resource type

**File:** `src/cosmos/databases_core.ml` (lines ~5–33)

Both the `Account` module type and the `Auth` implementation enumerate the resource variants,
so both must change:

```ocaml
module type Account = sig
  type resource = Dbs | Colls | Docs | Users | Permissions | Offers
  ...
end

module Auth (Keys : Databases_intf.Auth_key) : Account = struct
  type resource = Dbs | Colls | Docs | Users | Permissions | Offers

  let string_of_resource = function
    | Dbs -> "dbs"
    ...
    | Offers -> "offers"
```

`dune build` afterwards to confirm no other exhaustive match on `resource` exists (there should
be none outside this file).

---

## Phase 2 — ATD types

**File:** `src/cosmos/json_converter.atd` (append after `list_permissions`)

```ocaml
type offer_autopilot_settings = {
  max_throughput <json name="maxThroughput">: int;
}

type offer_content = {
  ?offer_throughput <json name="offerThroughput">: int option;
  ?offer_is_ru_per_minute_throughput_enabled
    <json name="offerIsRUPerMinuteThroughputEnabled">: bool option;
  ?offer_autopilot_settings <json name="offerAutopilotSettings">: offer_autopilot_settings option;
}

type offer = {
  id: string;
  offer_version <json name="offerVersion">: string;
  ?offer_type <json name="offerType">: string option;
  content: offer_content;
  resource: string;
  offer_resource_id <json name="offerResourceId">: string;
  rid <json name="_rid">: string;
  ts <json name="_ts">: int;
  self <json name="_self">: string;
  etag <json name="_etag">: string;
}

type list_offers = {
  rid <json name="_rid">: string;
  offers <json name="Offers">: offer list;
  count <json name="_count">: int;
}
```

Notes:
- `offer_type` is optional because it is absent on some accounts / V1 offers.
- All `content` fields are optional so the same type covers manual, RU/m and autoscale offers.
- `string_of_offer` is reused as the replace body, so no separate `replace_offer` type is needed.
- Regenerate with `dune build` (rules in `src/cosmos/dune` already run atdgen on this file).

---

## Phase 3 — Core `Offer` module

**File:** `src/cosmos/databases_core.ml`, inside `Make`, after the `Permission` module.

```ocaml
module Offer = struct
  let resource = Account.Offers
  let headers = headers resource
  let path_of_offers = "offers"
  let path_of_offer offer_rid = Printf.sprintf "offers/%s" offer_rid

  (* Offers are the only resource whose auth ResourceLink is the rid lowercased *)
  let auth_path_of_offer offer_rid = String.lowercase_ascii offer_rid

  module Throughput = struct
    type t = Manual of int | Autoscale of { max_throughput : int }

    val to_content : t -> Json_converter_t.offer_content
    val of_content : Json_converter_t.offer_content -> t option
    val string_of : t -> string   (* for logging / test assertions *)
  end

  val list :
    ?timeout:float ->
    unit ->
    (int * Json_converter_t.list_offers, cosmos_error) result io

  val get :
    ?timeout:float ->
    string ->                                  (* offer_rid *)
    (int * Json_converter_t.offer, cosmos_error) result io

  val query :
    ?max_item_count:int ->
    ?continuation:string ->
    ?timeout:float ->
    Json_converter_t.query ->
    (int * Response_headers.t * Json_converter_t.list_offers, cosmos_error) result io

  val replace :
    ?migrate:[ `To_autoscale | `To_manual ] ->
    ?timeout:float ->
    Json_converter_t.offer ->                  (* offer as previously fetched *)
    Throughput.t ->
    (int * Json_converter_t.offer, cosmos_error) result io

  (* Convenience lookups built on query *)
  val get_for_collection :
    ?timeout:float ->
    string -> string ->                        (* dbname, coll_name *)
    (int * Json_converter_t.offer option, cosmos_error) result io

  val get_for_database :
    ?timeout:float ->
    string ->                                  (* dbname, shared throughput *)
    (int * Json_converter_t.offer option, cosmos_error) result io

  val get_throughput :
    ?timeout:float ->
    string -> string ->
    (int * Throughput.t option, cosmos_error) result io

  val set_throughput :
    ?migrate:[ `To_autoscale | `To_manual ] ->
    ?timeout:float ->
    string -> string ->
    Throughput.t ->
    (int * Json_converter_t.offer, cosmos_error) result io
end
```

### Implementation details per function

| Function | Verb | Path | Auth ResourceLink | Body | Expected |
|----------|------|------|-------------------|------|----------|
| `list` | Get | `offers` | `""` | — | 200 |
| `get` | Get | `offers/{rid}` | `lowercase rid` | — | 200 |
| `query` | Post | `offers` | `""` | `string_of_query` | 200 |
| `replace` | Put | `offers/{rid}` | `lowercase rid` | `string_of_offer` | 200 |

- `query` sets `x-ms-documentdb-isquery: true` and `content-type: application/query+json`,
  and pipes `x-ms-max-item-count` / `x-ms-continuation` through
  `Utilities.apply_to_header_if_some` — mirror `Collection.Document.query`.
- `replace` builds the body from the input offer with `content = Throughput.to_content t`,
  and pipes the optional migration header:

```ocaml
let hdrs =
  json_headers resource Utilities.Verb.Put (auth_path_of_offer offer.rid)
  |> Utilities.apply_to_header_if_some "x-ms-cosmos-migrate-offer-to-autopilot"
       string_of_bool (match migrate with Some `To_autoscale -> Some true | _ -> None)
  |> Utilities.apply_to_header_if_some "x-ms-cosmos-migrate-offer-to-manual-throughput"
       string_of_bool (match migrate with Some `To_manual -> Some true | _ -> None)
```

- `replace` and `set_throughput` are throughput-changing writes and are rate-limited by the
  service, so route the request through `with_throttle_retry ~max_retries:3` (same as
  `Document.delete`).
- `get_for_collection` = `Collection.get dbname coll_name` → take `rid` → `query` with
  `SELECT * FROM c WHERE c.offerResourceId = @rid`, parameters `[{ name = "@rid"; value }]`
  → return the head of `offers` (or `None`).
- `get_for_database` = `Database.get dbname` (top-level `get`) → `_rid` → same query.
- `set_throughput dbname coll_name t` = `get_for_collection` → `replace`; when no offer exists
  (serverless account) return `Error (Azure_error (404, headers))` from the underlying lookup
  rather than inventing an error variant.
- `Throughput.of_content` returns `Autoscale` when `offer_autopilot_settings` is present,
  otherwise `Manual` when `offer_throughput` is present, otherwise `None`. Use `Option.fold`,
  not nested `match`.

---

## Phase 3b — Provisioned throughput at collection creation (prerequisite for deterministic CI)

**File:** `src/cosmos/databases_core.ml`, `Collection.create` / `Collection.create_if_not_exists`

Today `Collection.create` sends no throughput header, so the service picks the account default
and the live offer tests cannot assert a known starting value. Add an optional argument mapped to
the `x-ms-offer-throughput` header:

```ocaml
val create :
  ?indexing_policy:Json_converter_t.indexing_policy option ->
  ?offer_throughput:int ->
  partition_key:Json_converter_t.create_partition_key ->
  ?timeout:float ->
  string -> string ->
  (int * Json_converter_t.collection option, cosmos_error) result io
```

```ocaml
let hdrs =
  json_headers Account.Colls Utilities.Verb.Post ("dbs/" ^ dbname)
  |> Utilities.apply_to_header_if_some "x-ms-offer-throughput" string_of_int offer_throughput
```

Thread the same optional argument through `create_if_not_exists` and both backend `.mli` files.
Additive and backwards compatible — existing callers keep the account default.

---

## Phase 4 — Expose in both backends

**Files:** `src/cosmos_lwt/databases.mli`, `src/cosmos_eio/databases.mli`

Add an `Offer : sig ... end` block after `Permission` in each, mirroring the core signature with
`Lwt.t` / plain results respectively, and `Cosmos.Json_converter_t.offer` for the payload types.
`databases.ml` needs no change in either backend — both just instantiate the functor.

Include doc comments in the same style as the existing entries, e.g.:

```ocaml
val get_throughput :
  ?timeout:float ->
  string ->
  string ->
  (int * Offer.Throughput.t option, cosmos_error) result Lwt.t
(** [get_throughput dbname coll_name] returns the provisioned throughput of the
    collection, or [None] when the account is serverless. Requires master-key auth. *)
```

---

## Phase 5 — Tests

### 5a. Mock (offline, always run) — `test/core/mock_tests.ml`

These are the highest-value tests because the auth-path rule is the easy thing to get wrong.
Use the existing `Mock_http.expect` / `Mock_response` helpers and `Mock_db`.

| Test | Asserts |
|------|---------|
| `mock_offer_get_auth_resource_path` | authorization header is computed with the **lowercased** rid while the URI keeps the original casing |
| `mock_offer_list_auth_resource_path` | list signs with an empty ResourceLink |
| `mock_offer_query_headers` | `x-ms-documentdb-isquery` and `application/query+json` are set |
| `mock_offer_replace_body_round_trip` | replace body is valid JSON, keeps `_rid`/`_self`/`offerVersion`, and carries the new `offerThroughput` |
| `mock_offer_replace_autoscale_body` | autoscale replace emits `offerAutopilotSettings.maxThroughput` and no `offerThroughput` |
| `mock_offer_migrate_header` | `?migrate:`To_autoscale` sets the migration header; omitted otherwise |
| `mock_offer_throughput_of_content` | pure `Throughput.of_content` mapping for manual / autoscale / empty |

Add a `Mock_response.offer` (and `list_offers`) helper alongside the existing response builders
in `test/core/mock_response.ml{,i}`.

Register each test in the `tests` list at the bottom of `mock_tests.ml`.

### 5b. Live integration tests against the real account — `test/core/offer_tests.ml` (new)

These run against the live Cosmos account in CI. The `test` job in
`.github/workflows/main.yml` already exports `AZURE_COSMOS_KEY` (secret) and
`AZURE_COSMOS_ENDPOINT` (variable) before `dune runtest`, so `Test_common_core.should_run ()`
is `true` in CI and the offer tests execute for real. **No workflow change is required** — the
same two env vars gate every live test suite.

Follow the `Make (Cfg) (IO) (D)` functor shape used by `users_tests.ml` / `batch_tests.ml` and
expose a `tests` list of `(name, unit -> unit io)`.

#### Test resources

Use **prefix-scoped, offer-test-only** resources, because dune may run the lwt and eio test
binaries concurrently against the same account:

```ocaml
let dbname = Cfg.prefix ^ "offer_database"
let coll_name = "offerCollection"
```

Do **not** reuse the databases from `integration_tests.ml` / `batch_tests.ml`: changing their
throughput mid-run would make those suites flaky.

#### Test sequence

| # | Step | Assertion |
|---|------|-----------|
| 1 | `create_if_not_exists dbname` | 200 or 201 |
| 2 | `Collection.create_if_not_exists ~offer_throughput:400 ~partition_key dbname coll_name` | 200 or 201 (needs Phase 3b) |
| 3 | `Offer.list ()` | 200 and body parses; no assertion on count |
| 4 | `Offer.get_for_collection dbname coll_name` | 200; remember the offer |
| 5 | `Offer.get offer.rid` | 200 and same `_rid` — the live check of the lowercase auth path |
| 6 | `Offer.get_throughput dbname coll_name` | `Manual 400` |
| 7 | `Offer.set_throughput dbname coll_name (Manual 500)` | 200, then `get_throughput` = `Manual 500` |
| 8 | restore `set_throughput ... (Manual 400)` | 200 |
| 9 | teardown `Collection.delete` then `delete dbname` | 204 |

#### Rules that keep the live run green

1. **Tolerate accounts without offers.** Serverless accounts and shared-throughput databases
   expose no per-collection offer. If step 4 yields `None`, steps 5–8 must pass trivially
   (assert `unit` and return) instead of failing. Steps 1–4 still exercise the real HTTP path.
2. **Only use legal throughput values.** Manual minimum is **400 RU/s**, in multiples of 100.
   Use 400 → 500 → 400; never go below 400 and never test large decreases.
3. **Always restore and always tear down**, including on assertion failure, so a red run cannot
   leave the account at elevated RU/s or block the next run.
4. **Retry the offer-mutating calls.** A live account may answer a throughput change with `429`,
   `423 Locked` (another offer operation in flight) or `449 Retry with`. `Offer.replace` already
   retries `429` through `with_throttle_retry`; the test adds a small bounded retry:

   ```ocaml
   let rec with_offer_retry attempts f =
     let* r = f () in
     match r with
     | Error (Azure_error ((423 | 449 | 429), _)) when attempts > 0 ->
         let* () = IO.sleep 2.0 in
         with_offer_retry (attempts - 1) f
     | r -> IO.return r
   ```

   The eio test shim's `IO.sleep` in `test/eio/test.ml` is a no-op, so this loop spins without
   delay there — keep `attempts` small (5) and rely on the SDK-level retry, which uses the real
   Eio clock.
5. **Do not assert on `Offer.list` contents** beyond the collection's own offer: the CI account
   also holds offers from other suites and concurrent branches.
6. **Autoscale migration is mock-only.** Migrating an offer is slow and not cleanly reversible;
   `?migrate` stays covered by the mock tests, with a manual verification note.
7. **`get_for_database` asserts "no error"**, not a value — the SDK cannot create a
   shared-throughput database, so a database-level offer normally does not exist.

#### Wiring

Extend `module type DB` in `test/core/test_io_intf.ml` with the `Offer` sub-signature (including
`Offer.Throughput`), then register the functor in both runners exactly like `Batch`:

`test/lwt/test.ml`
```ocaml
module Offers = Test_core.Offer_tests.Make (Lwt_config) (Lwt_test_io) (D)

let offer_tests =
  if Test_core.Test_common_core.should_run () then wrap_async_tests `Slow Offers.tests else []

(* inside Alcotest_lwt.run *)
("offer test", offer_tests);
```

`test/eio/test.ml`
```ocaml
module Offers = Test_core.Offer_tests.Make (Eio_config) (Eio_test_io) (D)

let offer_tests =
  if Test_core.Test_common_core.should_run () then wrap_async_tests `Slow Offers.tests else []

(* inside Alcotest.run *)
("offer test", offer_tests);
```

With the `should_run` gate, a local `dune runtest` without `env.sh` sourced still passes (mock
tests only), while CI runs the full live suite.

---

## Phase 6 — Documentation

- `API_IMPROVEMENTS.md`: move **Offers** out of "Major Missing Features", add an
  `Offers` row to the implemented table, bump the coverage figures, and tick the
  "Offers Management" Phase 1 item.
- `README.md`: short throughput example (`get_throughput` / `set_throughput`).
- Note explicitly that offers require master-key auth and are unavailable on serverless
  accounts.

---

## Build & verification

```sh
dune build @fmt --auto-promote @ocaml-index
dune runtest
```

Both must be clean. `dune runtest` executes the mock tests unconditionally; the live tests
require `AZURE_COSMOS_KEY` and `AZURE_COSMOS_ENDPOINT` (source `env.sh` locally).

In CI the `test` job in `.github/workflows/main.yml` already supplies them:

```yaml
      - name: Run tests
        env:
          AZURE_COSMOS_KEY: ${{secrets.AZURE_COSMOS_KEY}}
          AZURE_COSMOS_ENDPOINT: ${{vars.AZURE_COSMOS_ENDPOINT}}
        run: opam exec -- dune runtest --instrument-with bisect_ppx --force
```

So the offer tests run against the real account with no workflow change. To confirm they are
actually executing rather than silently skipped, check the CI log for the `offer test` group in
both the `Main tests` (lwt) and `Main tests (Eio)` runs.

---

## Implementation order / checklist

- [ ] 1. `Offers` variant in `Account` module type + `Auth` impl (`databases_core.ml`)
- [ ] 2. ATD types `offer_autopilot_settings`, `offer_content`, `offer`, `list_offers`
- [ ] 3. `Offer.Throughput` module + unit-testable `to_content` / `of_content`
- [ ] 4. `Offer.list` and `Offer.get` (+ lowercase auth-path helper)
- [ ] 5. Mock tests for the auth path of `get` / `list` — **land before going further**
- [ ] 6. `Offer.query` + `get_for_collection` / `get_for_database`
- [ ] 7. `Offer.replace` (with `?migrate`, wrapped in `with_throttle_retry`)
- [ ] 8. `get_throughput` / `set_throughput` convenience wrappers
- [ ] 9. Remaining mock tests (bodies, headers)
- [ ] 10. `?offer_throughput` on `Collection.create` / `create_if_not_exists` (Phase 3b)
- [ ] 11. `test/core/test_io_intf.ml` DB signature extension
- [ ] 12. `test/core/offer_tests.ml` — live tests with prefix-scoped resources, retry on 423/449/429, restore + teardown
- [ ] 13. Wiring in `test/lwt/test.ml` and `test/eio/test.ml` behind `should_run`
- [ ] 14. `.mli` exposure for lwt and eio, with doc comments
- [ ] 15. Docs update (`API_IMPROVEMENTS.md`, `README.md`)

## Acceptance criteria

- `dune build @fmt --auto-promote @ocaml-index` and `dune runtest` both pass.
- `Offer` is reachable from both `Cosmos_lwt.Databases.Database(Auth)` and
  `Cosmos_eio.Databases.Database(Auth)` with identical shapes.
- No duplicated logic between backends; no new helpers duplicating `databases_core.ml` ones.
- Reading and changing manual throughput works end-to-end against a real account.
- Autoscale offers deserialise without error even if migration is not exercised live.
- The CI `test` job passes with the live account: the offer tests actually **run** (not skipped)
  because `AZURE_COSMOS_KEY` / `AZURE_COSMOS_ENDPOINT` are provided by the workflow, in both the
  lwt and eio suites.
- After the run the account is unchanged: throughput restored to 400 and the offer test database
  deleted.
- The pipeline passes twice in a row — tests are idempotent and tolerate leftovers from an
  aborted run via `create_if_not_exists`.

## Risks / open questions

| Risk | Mitigation |
|------|------------|
| Lowercased-rid auth rule is easy to regress | Dedicated mock test asserting the signed ResourceLink |
| Replace requires echoing unknown/undocumented fields | Round-trip the fetched offer through the ATD type; verify against a live account early. If the service rejects the round-trip, fall back to keeping the raw response body and splicing only `content` |
| `offerVersion` V1 accounts have a different shape (no `content`) | Optional fields in ATD; treat V1 as `Throughput.of_content = None` |
| Serverless accounts have no offers | Convenience lookups return `None`; live tests treat `None` as a pass instead of failing |
| Live throughput change rejected with 423/449 while another offer operation runs | Bounded test-level retry on 423/449/429 on top of the SDK 429 retry |
| lwt and eio suites run concurrently against the same CI account | Offer tests use their own `Cfg.prefix`-scoped database and collection |
| A failing test leaves the account at raised RU/s (cost) | Explicit restore step plus teardown that runs even on failure |
| Autoscale migration headers are not covered by tests without a live account | Mock test asserts the header is emitted; live migration left as manual verification |

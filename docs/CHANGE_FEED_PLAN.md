# Implementation Plan: Change Feed for Azure Cosmos DB

## Overview

The change feed is a persistent, ordered record of changes to a container. It is the basis for
CDC pipelines, event-driven architectures, cache invalidation and materialised views — listed as
gap **3. Real-Time Features** and Phase 2 item **6. Change Feed** in
[`API_IMPROVEMENTS.md`](API_IMPROVEMENTS.md), with the API sketch in improvement **8. Change Feed
Processor**.

Today the SDK exposes the change feed only as a side effect of `Collection.Document.list ?a_im`:

- `?a_im:true` sets `A-IM: Incremental feed`, and the caller must pass the previous response
  `etag` back through `?if_none_match` by hand
- `304 Not Modified` — the normal "no changes" answer — is returned as
  `Error (Azure_error (304, _))`, so the happy path looks like a failure
  (see `change_feed_with_partition_key_test` in `test/core/integration_tests.ml`, which asserts
  exactly that)
- there is no way to start from *now* or from a *point in time*, no way to enumerate the physical
  partitions of a container, and no polling/checkpointing loop

This plan adds a first-class **pull-model** change feed, plus the partition-key-range resource
needed to fan out across physical partitions, and a bounded polling helper. A lease-based
processor (push model) is explicitly deferred and sketched at the end.

**REST references**

- [List Documents (ReadFeed / incremental changes)](https://learn.microsoft.com/en-us/rest/api/cosmos-db/list-documents)
- [Common request headers](https://learn.microsoft.com/en-us/rest/api/cosmos-db/common-cosmosdb-rest-request-headers)
- [Get Partition Key Ranges](https://learn.microsoft.com/en-us/rest/api/cosmos-db/get-partition-key-ranges)
- [Change feed in Azure Cosmos DB](https://learn.microsoft.com/en-us/azure/cosmos-db/change-feed)

| Operation | HTTP | Path | Success |
|-----------|------|------|---------|
| Read change feed | `GET` | `/dbs/{db}/colls/{coll}/docs` + `A-IM` | 200 (changes) / 304 (none) |
| List partition key ranges | `GET` | `/dbs/{db}/colls/{coll}/pkranges` | 200 |

## Key API semantics (must-read before coding)

1. **The feed is a `GET` on `docs` with `A-IM: Incremental feed`.** Same path, same resource type
   (`docs`) and same auth ResourceLink (`dbs/{db}/colls/{coll}`) as `Document.list`.
2. **Start position is expressed through three mutually exclusive headers:**

   | Start position | Wire header |
   |----------------|-------------|
   | Beginning of retention | *no header* |
   | Now (only future changes) | `If-None-Match: *` |
   | A point in time | `If-Modified-Since: <RFC 1123 date>` |
   | Resume from a checkpoint | `If-None-Match: <etag>` |

   `If-Modified-Since` is **ignored when `If-None-Match` is present**, so the four cases must be a
   single closed variant, never independent optional arguments.
3. **The continuation token is the response `etag`**, not `x-ms-continuation`. `etag` is the LSN of
   the last document returned and is what goes into the next `If-None-Match`. It is present on
   both `200` and `304` responses, so a `304` still advances nothing but must still be surfaced to
   the caller as a valid checkpoint.
4. **`x-ms-continuation` means "this drain is not finished"** — more pages exist for the *current*
   set of changes (when `x-ms-max-item-count` truncated the page). It is a separate concept from
   the `etag` checkpoint; both must be exposed.
5. **`304 Not Modified` is success, not an error.** This is the single most important behavioural
   decision in this plan: `Change_feed.read` returns `Ok (304, headers, None)`.
6. **`410 Gone` means the physical partition was split or merged.** The saved
   `partition_key_range_id` is no longer valid and the caller must re-read `pkranges` and resume
   the child ranges. Detecting this needs the response sub-status, which `Response_headers` does
   not currently capture.
7. **Scoping:** a feed read can be scoped to the whole container, to a single
   `x-ms-documentdb-partitionkey`, or to one `x-ms-documentdb-partitionkeyrangeid`. Range scoping
   is what enables parallel consumption; the two scopes are mutually exclusive in practice and
   should be a variant, not two optional arguments.
8. **Ordering is guaranteed only within a partition key**, never across the container. Any helper
   that fans out across ranges must not promise global ordering.
9. **Retention:** with `Beginning`, the feed starts at the oldest retained change, which is *not*
   necessarily container creation on accounts with a finite backup window. Do not document
   "from creation".
10. **All-versions-and-deletes mode** (`A-IM: Full-Fidelity Feed` +
    `x-ms-cosmos-changefeed-wire-format-version: 2021-09-15`) needs change feed retention
    (continuous backups) on the container and returns a *different body shape* — each item is
    wrapped in an envelope with `_metadata` (`operationType`, `crts`, `lsn`, `previousImageLSN`)
    plus `current`/`previous` images. Out of scope for the first iteration — see "Out of scope"
    below.
11. **The `A-IM` value is matched case-insensitively by the service.** The REST reference documents
    `Incremental feed`; every official SDK sends `Incremental Feed` (capital F). This SDK already
    sends `Incremental feed` and the live `change_feed_with_partition_key_test` passes with it, so
    keep that exact string — it is the verified-working value.
12. **API version:** this SDK hard-codes `x-ms-version: 2018-12-31` in `headers`, which is the
    newest version listed in the REST reference's *Supported REST API Versions* table and is
    sufficient for everything in this plan (change feed was introduced in `2016-07-11`, `pkranges`
    is available from `2016-07-11`). Note for future work that the table is stale: the official
    Python and JS SDKs both send `x-ms-version: 2020-07-15`, an undocumented but service-accepted
    version. Nothing here requires bumping it.

## Architecture decisions

- New `Change_feed` module inside `Collection`, **as a sibling of `Document` and `Batch`**, placed
  after `Document` so it can reuse `Document.convert_to_list_result` and
  `Document.list_result_meta_data`. The feed is collection-scoped, so it does not belong at the
  top level next to `Offer`.
- New `Partition_key_range` module inside `Collection` (also collection-scoped), backed by ATD
  types — the `pkranges` payload has a fixed shape.
- `Start_from`, `Mode` and `Scope` are types with more than one operation, so each is a **nested
  module with `type t`** per the project convention, not top-level variants.
- `Document.list ?a_im ?if_none_match` is **left exactly as it is**. It stays source-compatible and
  its `Error (Azure_error (304, _))` behaviour is asserted by an existing test that this plan must
  not weaken. `Change_feed.read` is the new, correct surface; the old path gets a doc comment
  pointing at it.
- Reuse existing helpers only: `make_uri`, `headers`, `add_header`,
  `Utilities.apply_to_header_if_some`, `handle_response`, `with_throttle_retry`,
  `Utilities.Ms_time.{create,x_ms_date}` (already emits RFC 1123, so no new date formatter).
- **`?timeout` is composed *inside* `with_throttle_retry`, per attempt.** See Phase 1b: the shared
  helper gains an optional `?timeout` that wraps each attempt with the existing `wrap_timeout` and
  fails fast with `Timeout_error` (a timeout is not retried). Do **not** copy the
  `Document.create` pattern of awaiting the request and *then* running
  `IO.with_timeout t (IO.return ())` — that measures nothing and cannot fire.
- **429 retry exhaustion currently returns `Timeout_error`**, losing the throttle cause and
  `x-ms-retry-after-ms` (improvement 6 in [`API_IMPROVEMENTS.md`](API_IMPROVEMENTS.md),
  `BATCH_REVIEW_FINDINGS.md` #3). This plan **accepts** that behaviour rather than fixing it, so
  `Change_feed` inherits it: a throttled-out feed read is indistinguishable from a timeout. Fixing
  it is a separate, cross-cutting change to `with_throttle_retry` and its callers' error mapping;
  do not fold it in here. The doc comment on `read` must state it.
- **No new `cosmos_error` variant.** `410` surfaces as `Azure_error (410, headers)`; the caller
  distinguishes a split via a new `Response_headers.x_ms_substatus` accessor and a
  `Change_feed.is_partition_split` predicate. Adding a variant would be a breaking change for
  every existing exhaustive `match` on `cosmos_error` in user code and in
  `test/core/integration_tests.ml`.
- The polling helper is **bounded and driven by a caller predicate**. `Databases_intf.IO` has
  `sleep` but no cancellation primitive, so an unbounded `subscribe`/`cancel` pair cannot be
  implemented portably across Lwt and Eio. `fold` therefore stops when the caller says stop.

---

## Phase 1 — `Pkranges` resource type and `x-ms-substatus`

**File:** `src/cosmos/databases_core.ml` (`Account` module type at ~5–12, `Auth_credential` at
~14–40, `Response_headers` at ~47–120)

Since resource-token authentication landed, the resource variants are enumerated in exactly **two**
places: the `Account` module type and `Auth_credential`. `Auth (Keys : Auth_key)` is now a shim
(`Auth_credential` applied to a `Master_key` struct) and needs **no change**:

```ocaml
module type Account = sig
  type resource = Dbs | Colls | Docs | Users | Permissions | Offers | Pkranges
  ...
end

module Auth_credential (C : Databases_intf.Credentials) : Account = struct
  type resource = Dbs | Colls | Docs | Users | Permissions | Offers | Pkranges

  let string_of_resource = function
    | Dbs -> "dbs"
    ...
    | Pkranges -> "pkranges"
```

Note that `string_of_resource` is only consumed on the `Master_key` branch of `authorization`; under
a resource token the pre-signed `_token` is used verbatim, so `Pkranges` needs no token-side work.

Extend `Response_headers` with the sub-status, needed to tell a partition split (`410` +
`1002`/`1007`) from any other `410`:

```ocaml
type t = { ...; x_ms_substatus : string option }

let update t = function
  | "x-ms-substatus", value -> { t with x_ms_substatus = Some value }
  ...

let x_ms_substatus t = t.x_ms_substatus
```

Purely additive: `empty` gains one `None`, and both backend `.mli` files gain one accessor.

---

## Phase 1b — per-attempt `?timeout` in `with_throttle_retry`

**File:** `src/cosmos/databases_core.ml` (~line 205)

`with_throttle_retry ~max_retries f` currently takes `f : unit -> (resp * body, Http.error) result
io` and has no timeout path, which is why `Document.create` ends with a post-hoc
`IO.with_timeout t (IO.return ())` that can never fire. `Change_feed.read` must not inherit that,
so wrap each attempt with the existing `wrap_timeout`:

```ocaml
let with_throttle_retry ?timeout ~max_retries f =
  let rec retry_loop attempt () =
    IO.catch
      (fun () ->
        let* result = f () |> wrap_timeout timeout in
        match result with
        | None -> timeout_error            (* a timeout is never retried *)
        | Some (Ok (resp, body)) -> (* existing 429 / success handling *)
        | Some (Error Http.Connection_refused) -> (* existing retry *)
        | Some (Error (Http.Other_error exn)) -> raise exn)
      (* existing exception handler *)
  in
  retry_loop max_retries ()
```

Rules:

- The argument is **optional and defaults to `None`**, so all existing call sites
  (`Document.create` `~max_retries:10`, `Document.delete` `~max_retries:3`, `Batch`) keep compiling
  and keep their current behaviour.
- `timeout` bounds **one attempt**, not the whole retry sequence — the total wall clock is still
  `max_retries` attempts plus the throttle sleeps. Say so in the doc comment; do not pretend it is
  a deadline.
- Do **not** refactor `Document.create`'s dead `IO.with_timeout` in this plan; it is unrelated
  cleanup and touching it changes an already-tested path.

---

## Phase 2 — ATD types for partition key ranges

**File:** `src/cosmos/json_converter.atd` (append after `list_offers`)

```ocaml
type partition_key_range = {
  id: string;
  min_inclusive <json name="minInclusive">: string;
  max_exclusive <json name="maxExclusive">: string;
  rid <json name="_rid">: string;
  ts <json name="_ts">: int;
  self <json name="_self">: string;
  etag <json name="_etag">: string;
}

type list_partition_key_ranges = {
  rid <json name="_rid">: string;
  partition_key_ranges <json name="PartitionKeyRanges">: partition_key_range list;
  count <json name="_count">: int;
}
```

Notes:

- Real responses carry extra internal fields (`ridPrefix`, `throughputFraction`, `status`,
  `parents`, `ownedArchivalPkRangeIds`, …). atdgen ignores unknown fields by default, so only the
  documented ones are modelled. **Do not** model `parents`: it is undocumented and its absence on
  unsplit ranges would force an option for no benefit.
- The change feed itself needs **no** new ATD type — the `200` body is the same
  `{_rid, Documents, _count}` shape `Document.convert_to_list_result` already parses.
- Regenerate with `dune build` (the atdgen rules in `src/cosmos/dune` already cover this file).

---

## Phase 3 — `Collection.Partition_key_range`

**File:** `src/cosmos/databases_core.ml`, inside `Collection`, after `Document`.

```ocaml
module Partition_key_range = struct
  val list :
    ?max_item_count:int ->
    ?continuation:string ->
    ?timeout:float ->
    string ->  (* dbname *)
    string ->  (* coll_name *)
    (int * Response_headers.t * Json_converter_t.list_partition_key_ranges,
     cosmos_error) result io

  (* Convenience: just the range ids, in feed-partition order *)
  val ids : ?timeout:float -> string -> string -> (int * string list, cosmos_error) result io
end
```

| Detail | Value |
|--------|-------|
| Verb / path | `Get` `/dbs/{db}/colls/{coll}/pkranges` (add `path_of_pkranges` next to `path_of_docs`, **with the same leading `/`** — `make_uri` takes it verbatim, while the auth link must not have one) |
| Resource type | `Account.Pkranges` |
| Auth ResourceLink | `dbs/{db}/colls/{coll}` — the *parent collection*, not the pkranges path |
| Expected | 200 → `result_or_error_with_result 200` |

Pipe `x-ms-max-item-count` / `x-ms-continuation` through `Utilities.apply_to_header_if_some`, and
return `Response_headers.t` so a caller can page a container with hundreds of ranges.

---

## Phase 4 — `Collection.Change_feed`

**File:** `src/cosmos/databases_core.ml`, inside `Collection`, after `Partition_key_range`.

```ocaml
module Change_feed = struct
  module Mode = struct
    type t = Latest_version
    (* Full_fidelity is deliberately absent in iteration 1 — see "Out of scope".
       A one-constructor variant plus ?mode is dead surface today; it exists so that
       adding Full_fidelity later is additive rather than a signature change. Keep it. *)

    val string_of : t -> string   (* Latest_version -> "Incremental feed" *)
  end

  module Start_from = struct
    type t =
      | Beginning
      | Now
      | Point_in_time of float          (* Unix epoch seconds *)
      | Continuation of string          (* etag from a previous page *)

    val string_of : t -> string          (* for logging and test assertions *)
  end

  module Scope = struct
    type t =
      | Container
      | Partition_key of string
      | Partition_key_range of string   (* Partition_key_range.list -> id *)

    val string_of : t -> string
  end

  type page = {
    rid : string;               (* _rid of the collection, straight from list_result *)
    documents : (string * Document.list_result_meta_data option) list;
    count : int;
    continuation : string;      (* etag: feed this back as Start_from.Continuation *)
    has_more_pages : bool;      (* x-ms-continuation present: drain again immediately *)
    session_token : string option;
  }

  type drain_result = {
    pages : page list;          (* only 200 pages, in order; may be [] *)
    checkpoint : string;        (* etag to resume from: the last one seen,
                                   including the etag of a terminal 304 *)
    caught_up : bool;           (* true if the loop ended on a 304,
                                   false if it ended at max_pages *)
  }

  val read :
    ?mode:Mode.t ->
    ?start_from:Start_from.t ->            (* default Beginning *)
    ?scope:Scope.t ->                      (* default Container *)
    ?max_item_count:int ->
    ?session_token:string ->
    ?timeout:float ->
    string ->                              (* dbname *)
    string ->                              (* coll_name *)
    (int * Response_headers.t * page option, cosmos_error) result io
  (* Ok (200, hdrs, Some page) — changes; Ok (304, hdrs, None) — no changes *)

  val is_partition_split : cosmos_error -> bool

  (* Drain everything currently available for one scope, then stop *)
  val drain :
    ?mode:Mode.t ->
    ?start_from:Start_from.t ->
    ?scope:Scope.t ->
    ?max_item_count:int ->
    ?max_pages:int ->                      (* default 100, guards runaway loops *)
    ?timeout:float ->
    string -> string ->
    (drain_result, cosmos_error) result io

  (* Bounded polling loop: caller owns the checkpoint and the stop condition *)
  val fold :
    ?mode:Mode.t ->
    ?start_from:Start_from.t ->
    ?scope:Scope.t ->
    ?max_item_count:int ->
    ?poll_interval:float ->                (* default 1.0s, applied only after a 304 *)
    ?max_polls:int ->                      (* default 1, i.e. a single drain *)
    ?timeout:float ->
    string -> string ->
    init:'acc ->
    f:('acc -> page -> ('acc, string) result io) ->
    ('acc * string, cosmos_error) result io   (* final accumulator + last checkpoint *)
end
```

### `read` implementation details

```ocaml
let path = path_of_docs dbname coll_name in
let hdrs =
  headers Account.Docs Utilities.Verb.Get (Printf.sprintf "dbs/%s/colls/%s" dbname coll_name)
  |> add_header "A-IM" (Mode.string_of mode)
  |> Utilities.apply_to_header_if_some "x-ms-max-item-count" string_of_int max_item_count
  |> Utilities.apply_to_header_if_some "x-ms-session-token" Fun.id session_token
  |> apply_start_from start_from
  |> apply_scope scope
```

- `apply_start_from` is the only place the three mutually exclusive headers are set, and it is a
  single `match` over the closed variant:

  ```ocaml
  let apply_start_from = function
    | Start_from.Beginning -> Fun.id
    | Start_from.Now -> add_header "If-None-Match" "*"
    | Start_from.Continuation etag -> add_header "If-None-Match" etag
    | Start_from.Point_in_time time ->
        add_header "If-Modified-Since"
          (Utilities.Ms_time.x_ms_date (Utilities.Ms_time.create time))
  ```

  `Ms_time.x_ms_date` already emits `"Fri, 08 Apr 2015 03:52:31 GMT"` — RFC 1123, exactly what
  `If-Modified-Since` wants. No new formatter, no new dependency.
- `apply_scope` maps `Partition_key pk` to `x-ms-documentdb-partitionkey`
  (`string_of_partition_key pk`, so it is JSON-array-quoted like everywhere else) and
  `Partition_key_range id` to `x-ms-documentdb-partitionkeyrangeid`.
- **Do not** use `json_headers`: this is a `GET` with no body, and `Document.list`'s use of
  `json_headers` is an existing wart worth not copying.
- Route through `with_throttle_retry ?timeout ~max_retries:10` (a feed read on a hot container is
  the most likely operation in this SDK to be throttled) using the per-attempt `?timeout` added in
  Phase 1b, then classify:

  | Status | Result |
  |--------|--------|
  | 200 | `Ok (200, hdrs, Some page)` with `continuation = etag` |
  | 304 | `Ok (304, hdrs, None)` |
  | other | `Error (Azure_error (code, hdrs))` |

- `continuation` is `Response_headers.etag hdrs`. A `200` or `304` without an `etag` is a protocol
  violation; return `Error (Azure_error (code, hdrs))` rather than inventing an empty token that
  would silently restart the feed from the beginning on the next call.
- `has_more_pages = Option.is_some (Response_headers.x_ms_continuation hdrs)`.
- Reuse `Document.convert_to_list_result` for the body and map its `rid`/`documents`/`count` into
  `page`; do not re-parse the JSON. On a `304` the body is empty, so `convert_to_list_result` is
  never called on that path — `read` returns `None` before touching the body.
- `is_partition_split = function Azure_error (410, h) -> (match Response_headers.x_ms_substatus h
  with Some ("1002" | "1007") -> true | _ -> false) | _ -> false`.

### `drain` and `fold`

- `drain` loops on `read` while `has_more_pages` is true, threading `Start_from.Continuation
  page.continuation`, and stops on the first `304` or at `max_pages`. It never sleeps.
  It returns a **record, not `(int * page list)`** — a status code is meaningless for a loop that
  mixes `200`s with a terminal `304`, and callers need the checkpoint and the
  "is there more work" flag more than they need a code. `caught_up = false` means the caller should
  call `drain` again from `checkpoint`. A `drain` that immediately sees a `304` returns
  `{ pages = []; checkpoint = <etag of the 304>; caught_up = true }`.
- `fold`'s returned checkpoint is `drain_result.checkpoint` of the last completed drain, so a
  poll cycle that saw no changes still advances nothing but stays resumable.
- `fold` calls `drain` up to `max_polls` times, sleeping `poll_interval` via `IO.sleep` **only
  after a `304`** (sleeping between full pages would throttle a healthy backlog for no reason).
  The callback returns `('acc, string) result io`: `Error reason` stops the fold cleanly and the
  last successful checkpoint is still returned, so a consumer that fails to process a page does
  not lose its place.
- Both default to a *bounded* number of iterations. A daemon-style consumer composes `fold` in its
  own loop — the SDK does not own the application's main loop, and `IO` has no cancellation
  primitive to make an unbounded `subscribe` safe.
- Neither helper spawns concurrent work. Cross-partition parallelism is the caller's choice:
  `Partition_key_range.ids` + `IO.parallel_map` over `Scope.Partition_key_range`, with one
  checkpoint per range. Document that pattern in the `README`; do not hide it behind a helper that
  would imply global ordering.

---

## Phase 5 — Expose in both backends

**Files:** `src/cosmos_lwt/databases.mli`, `src/cosmos_eio/databases.mli`

Add `Partition_key_range : sig ... end` and `Change_feed : sig ... end` blocks inside
`Collection`, after `Document`, mirroring the core signature with `Lwt.t` / plain results.
`databases.ml` needs no change in either backend — both only instantiate the functor.

Also add the `Response_headers.x_ms_substatus` accessor to both.

Doc comments in the existing style, and one on the legacy path:

```ocaml
val read :
  ?mode:Change_feed.Mode.t ->
  ?start_from:Change_feed.Start_from.t ->
  ...
(** [read dbname coll_name] reads one page of the change feed. Returns
    [Ok (304, headers, None)] when there are no changes since [start_from] —
    this is the normal idle answer, not an error. Persist [page.continuation]
    (the response ETag) and pass it back as [Start_from.Continuation] to resume. *)
```

```ocaml
val list : ?a_im:bool -> ... 
(** ... [?a_im] requests a raw incremental feed and reports "no changes" as
    [Error (Azure_error (304, _))]. Prefer {!Change_feed.read}. *)
```

---

## Phase 6 — Tests

### 6a. Mock tests (offline, always run) — `test/core/mock_tests.ml`

The wire-header mapping and the 304 classification are the things that break silently, so these
are the highest-value tests. Use the existing `Mock_http.expect` / `Mock_response` / `Mock_db`
helpers and register each test in the `tests` list at the bottom of the file.

| Test | Asserts |
|------|---------|
| `mock_change_feed_auth_resource_path` | authorization is computed with verb `get`, resource `docs`, link `dbs/mydb/colls/mycoll` (reuses `compute_expected_auth`) |
| `mock_change_feed_a_im_header` | `A-IM: Incremental feed` is always sent |
| `mock_change_feed_start_beginning` | **no** `If-None-Match` and **no** `If-Modified-Since` |
| `mock_change_feed_start_now` | `If-None-Match: *` |
| `mock_change_feed_start_point_in_time` | `If-Modified-Since` is RFC 1123 and `If-None-Match` is absent |
| `mock_change_feed_start_continuation` | `If-None-Match: <etag>` verbatim, including quotes |
| `mock_change_feed_scope_partition_key` | `x-ms-documentdb-partitionkey: ["pk"]`, no range header |
| `mock_change_feed_scope_range` | `x-ms-documentdb-partitionkeyrangeid: 0`, no partition-key header |
| `mock_change_feed_304_is_ok` | `Ok (304, _, None)` — **the regression test for the core design decision** |
| `mock_change_feed_page_continuation` | `page.continuation` equals the response `etag`, not `x-ms-continuation` |
| `mock_change_feed_has_more_pages` | `has_more_pages` follows presence of `x-ms-continuation` |
| `mock_change_feed_drain_stops_on_304` | two 200 pages then a 304 → `pages` length 2, `caught_up = true`, `checkpoint` = the 304's etag, and the second request carries the first page's etag |
| `mock_change_feed_drain_respects_max_pages` | `~max_pages:1` issues exactly one request and returns `caught_up = false` |
| `mock_change_feed_drain_immediate_304` | a lone 304 → `pages = []`, `caught_up = true`, `checkpoint` non-empty |
| `mock_change_feed_timeout_not_retried` | a timing-out attempt returns `Error Timeout_error` after exactly one request (Phase 1b: timeouts are not retried) |
| `mock_change_feed_fold_checkpoint` | callback sees pages in order; returned checkpoint is the last etag |
| `mock_change_feed_fold_callback_error_keeps_checkpoint` | `Error _` from `f` stops the fold and still returns the last good checkpoint |
| `mock_change_feed_throttle_retry` | `Mock_response.throttled_response` then 200 → one retry, `Ok` |
| `mock_change_feed_split_detection` | 410 + `x-ms-substatus: 1002` → `is_partition_split` is true; 410 without substatus → false |
| `mock_pkranges_auth_resource_path` | verb `get`, resource `pkranges`, link `dbs/mydb/colls/mycoll` |
| `mock_pkranges_parses_ids` | `Partition_key_range.ids` returns `["0"; "1"]` in order |

New helpers in `test/core/mock_response.ml{,i}`:

```ocaml
val change_feed_response : ?etag:string -> ?continuation:string -> (string * string) list -> Cohttp.Response.t * string
val not_modified_response : etag:string -> Cohttp.Response.t * string          (* 304, empty body *)
val partition_split_response : unit -> Cohttp.Response.t * string              (* 410 + substatus 1002 *)
val list_partition_key_ranges_response : (string * string * string) list -> string
```

`change_feed_response` reuses `list_documents_response` for the body and only adds the `etag` /
`x-ms-continuation` headers — the feed body shape is identical to a read feed.

### 6b. Live integration tests — `test/core/change_feed_tests.ml` (new)

Follow the `Make (Cfg) (IO) (D)` functor shape used by `offer_tests.ml` and expose a
`tests : (string * (unit -> unit io)) list`. Prefix-scoped, change-feed-only resources, because
dune may run the lwt and eio binaries concurrently against the same account:

```ocaml
let dbname = Cfg.prefix ^ "change_feed_database"
let coll_name = "changeFeedCollection"
```

Do **not** reuse the databases from `integration_tests.ml`: writes there would inject
non-deterministic changes into this feed, and this suite's writes would perturb theirs.

#### Test sequence

| # | Step | Assertion |
|---|------|-----------|
| 1 | `create_if_not_exists dbname`, `Collection.create_if_not_exists ~offer_throughput:400 ~partition_key` | 200 or 201 |
| 2 | `Change_feed.read ~start_from:Now` | `Ok (304, _, None)` on an empty container, or `Ok (200, ...)`; capture the checkpoint either way |
| 3 | create 3 documents in one partition | 201 each |
| 4 | `read ~start_from:(Continuation ckpt)` | `Ok (200, _, Some page)`, `page.count = 3`, ids match |
| 5 | `read ~start_from:(Continuation page.continuation)` | `Ok (304, _, None)` — feed is caught up |
| 6 | replace one document, then read from the same checkpoint | 200 with that document present (an update appears as a change) |
| 7 | delete one document **and** replace a second (the sentinel) in the same step, then read from the same checkpoint | the sentinel id **is** present (proves the feed advanced past the delete) **and** the deleted id is absent — latest-version mode does not surface deletes. The sentinel is what makes this assertion falsifiable; see rule 6 |
| 8 | `Partition_key_range.ids dbname coll_name` | 200, non-empty |
| 9 | `read ~scope:(Partition_key_range id)` for each id, summed | equals the container-scoped count |
| 10 | `read ~scope:(Partition_key "a Last name")` | only that partition's changes |
| 11 | teardown of this collection: `Collection.delete` | 204 |

Steps 12–15 need a container whose whole retention window belongs to this run, so they run against
a **second, freshly created collection** in the same database (`changeFeedPagingCollection`), each
with its own `with_teardown`:

| # | Step | Assertion |
|---|------|-----------|
| 12 | create `changeFeedPagingCollection`, write exactly 3 documents in one partition | 201 each |
| 13 | `read ~start_from:Beginning ~max_item_count:1` | `has_more_pages = true`, `count = 1` |
| 14 | `drain ~start_from:Beginning ~max_item_count:1` | `List.length pages >= 2`, `caught_up = true`, total documents = 3 exactly, id-set equality |
| 15 | `fold ~start_from:Beginning ~max_polls:1 ~init:0 ~f:(count)` | accumulator = 3; checkpoint non-empty |
| 16 | teardown: `Collection.delete` of the paging collection, then `delete dbname` | 204 |

#### Rules that keep the live run green

1. **Treat `304` as an expected outcome only where it is the *asserted* outcome.** Step 5 asserts
   `304` positively. Steps 4/6/7, which read after a write, must **retry and then fail** on a
   persistent `304` — they must never accept it as a pass. See rule 4 and the anti-patterns in
   Phase 6c.
2. **Never assert an exact count from `Beginning` without a fresh collection.** This is why steps
   12–15 use their own `changeFeedPagingCollection`, created and deleted inside the test body via
   the `offer_tests.ml` `with_teardown` pattern, instead of reusing the collection that steps 2–10
   have already written to, replaced in and deleted from. Do not merge the two collections back
   into one — the counts in steps 14–15 are exact and would drift with every earlier step.
3. **Do not assert on `_lsn`/etag values or their ordering as strings.** The token is opaque;
   assert only that it round-trips.
4. **Allow eventual visibility.** A change can take a moment to appear in the feed. Wrap the
   read-after-write steps in a small bounded retry, mirroring `with_offer_retry`:

   ```ocaml
   let rec until_changes attempts f =
     let* r = f () in
     match r with
     | Ok (304, _, _) when attempts > 0 ->
         let* () = IO.sleep 1.0 in
         until_changes (attempts - 1) f
     | Ok (304, _, _) -> Alcotest.fail "change never appeared in the feed"
     | r -> IO.return r
   ```

   Note the final `Ok (304, ...)` case: exhausting the retries is a **failure**, not a pass.
   `IO.sleep` is a no-op in the current eio test shim, which makes this retry useless under eio —
   Phase 6c fixes that shim, and it must be fixed *before* this test is trusted.
5. **Always tear down**, including on assertion failure, via the `with_teardown` wrapper — a red
   run must not leave a collection billing RU/s.
6. **Never assert only an absence.** "The deleted id is not in the page" is satisfied by an empty
   page, a lagging feed, or a broken parser. Every absence assertion must be paired with a
   positive sentinel in the *same* page — step 7 replaces a second document precisely so the page
   is known to be non-empty and current before the absence is checked.
7. **Do not test partition splits.** Forcing a split requires a large RU/s change and minutes of
   waiting; `is_partition_split` stays mock-only, with a manual verification note.
8. **Keep the existing `change_feed_with_partition_key_test`** in `integration_tests.ml` unchanged.
   It is now a regression test for the legacy `?a_im` behaviour.

#### Wiring

Extend `module type DB` in `test/core/test_io_intf.ml` with `Collection.Partition_key_range` and
`Collection.Change_feed` (including the three nested `Mode` / `Start_from` / `Scope` signatures and
the `page` and `drain_result` records), then register the functor in both runners exactly like
`Offers`:

`test/lwt/test.ml`
```ocaml
module Change_feed = Test_core.Change_feed_tests.Make (Lwt_config) (Lwt_test_io) (D)

let change_feed_tests =
  if Test_core.Test_common_core.should_run () then wrap_async_tests `Slow Change_feed.tests else []

(* inside Alcotest_lwt.run *)
("change feed test", change_feed_tests);
```

`test/eio/test.ml`
```ocaml
module Change_feed = Test_core.Change_feed_tests.Make (Eio_config) (Eio_test_io) (D)

let change_feed_tests =
  if Test_core.Test_common_core.should_run () then wrap_async_tests `Slow Change_feed.tests else []

(* inside Alcotest.run *)
("change feed test", change_feed_tests);
```

`test/core/change_feed_tests.ml` needs no `dune` edit — `test/core/dune` is a whole-directory
library stanza.

With the `should_run ()` gate, a local `dune runtest` without `env.sh` sourced still passes (mock
tests only), while CI runs the live suite — **provided the guarantees in Phase 6c are in place**.

---

### 6c. Guaranteeing the lwt and eio suites are really live (not mocks)

The `change feed test` groups in `test/lwt/test.ml` and `test/eio/test.ml` **must** exercise a real
Azure account over real HTTP. This repository currently has four structural ways for that to look
green while testing nothing, all of which this phase closes. Treat every item here as a
requirement, not a suggestion.

#### Hazard 1 — silent skip when credentials are missing

`should_run ()` returns `false` when `AZURE_COSMOS_KEY` / `AZURE_COSMOS_ENDPOINT` are unset, and
every live group then registers as `[]`. Alcotest reports success for an empty group, so the suite
goes **green with zero live coverage**. Worse, `MyAuthKeys.getenv` maps a missing variable to `""`,
so a half-configured environment builds a client with an empty master key instead of failing loudly.
This is the most likely way for a live suite to quietly become decorative — e.g. a fork PR, a
rotated secret, or a renamed workflow variable.

Fix — make "live required" explicit and enforced, in `test/core/test_common_core.ml`:

```ocaml
let live_required_env = "COSMOS_REQUIRE_LIVE_TESTS"
let live_required () = Sys.getenv_opt live_required_env = Some "1"

(* Registered unconditionally, so it runs even when every live group is empty *)
let live_wiring_test ~suite ~registered () =
  if live_required () then begin
    Alcotest.(check bool)
      (suite ^ ": AZURE_COSMOS_KEY and AZURE_COSMOS_ENDPOINT must be set")
      true (should_run ());
    Alcotest.(check bool)
      (suite ^ ": live test cases must be registered")
      true (registered > 0)
  end
```

Register it in **both** runners in a group that is always non-empty, and set
`COSMOS_REQUIRE_LIVE_TESTS: "1"` next to the existing `AZURE_COSMOS_KEY` / `AZURE_COSMOS_ENDPOINT`
entries in the `test` job of `.github/workflows/main.yml`:

```ocaml
( "live wiring",
  wrap_sync_tests `Quick
    [ ( "change feed live tests registered",
        Test_core.Test_common_core.live_wiring_test ~suite:"change feed"
          ~registered:(List.length change_feed_tests) ) ] );
```

Effect: locally `dune runtest` still passes with mocks only; in CI a missing or broken credential
turns the build **red** instead of silently skipping. Apply the same guard to the existing live
groups while touching this file.

#### Hazard 2 — the eio test shim is a no-op fake

`Eio_test_io` in `test/eio/test.ml` is `type 'a t = unit -> 'a` with:

```ocaml
let sleep secs () = ignore secs                  (* comment claims it delegates to the clock; it does not *)
let with_timeout t cmd () = ignore t; Some (cmd ())
let parallel_map f xs () = List.map (fun x -> (f x) ()) xs
```

So under eio: test-level sleeps do not sleep, timeouts never fire, and "parallel" fan-out is
sequential. The SDK's own `Cosmos_eio.Databases.IO` is real (real `Eio.Time.sleep`,
`Eio.Time.with_timeout`, `Eio.Fiber.List.map` — see `src/cosmos_eio/databases.ml`), so the HTTP
calls in eio tests *are* real; it is only the test harness's orchestration that is fake. That is
enough to hollow out this plan's eventual-visibility retry (rule 4) and the range fan-out test.

Fix the shim as part of this work — capture the clock inside `Eio_main.run` and delegate:

```ocaml
let clock_ref : Eio.Time.clock option ref = ref None   (* set inside Eio_main.run, before Alcotest.run *)
let get_clock () = match !clock_ref with Some c -> c | None -> failwith "clock not set"

module Eio_test_io = struct
  ...
  let sleep secs () = Eio.Time.sleep (get_clock ()) secs
  let with_timeout t cmd () =
    match Eio.Time.with_timeout (get_clock ()) t (fun () -> Ok (cmd ())) with
    | Ok x -> Some x
    | Error `Timeout -> None
  let parallel_map f xs () = Eio.Fiber.List.map ~max_fibers:10 (fun x -> (f x) ()) xs
end
```

Until this lands, the eio change feed suite is strictly weaker than the lwt one and must not be
described as equivalent. Also delete the misleading `sleep` comment.

#### Hazard 3 — the live functor is structurally mockable

`Change_feed_tests.Make` takes `(Cfg) (IO) (D : DB)`. `Mock_db` in `test/core/mock_test_runner.ml`
satisfies the same `DB` signature, so `Change_feed_tests.Make (Cfg) (Mock_io) (Mock_db)` would
compile and yield a fully green `change feed test` group that never opens a socket. **This is
prohibited.** Rules:

- `Change_feed_tests.Make` is applied to `D = Database (MyAuthKeys)` only, in `test/lwt/test.ml` and
  `test/eio/test.ml`.
- All mock coverage lives in `mock_tests.ml`, in the `mock tests` group, with test names prefixed
  `mock_`. No mock ever appears in a group named `... test (live)`.
- Name the live groups `"change feed test (live)"` in both runners so a CI log cannot be misread.

#### Hazard 4 — assertions that cannot fail

A live test that asserts nothing falsifiable is worse than no test. Banned patterns, all of which
would pass against a stub:

| Anti-pattern | Why it is wrong |
|--------------|-----------------|
| `match r with Ok _ -> () \| Error _ -> ()` | accepts every outcome |
| accepting `304` as a pass after writing documents | the empty feed is exactly the bug being tested for |
| `Alcotest.(check bool) "count" true (count >= 0)` | vacuously true |
| wrapping the whole body in `IO.catch (fun _ -> IO.return ())` | swallows failures; `with_teardown` must re-raise, as `offer_tests.ml` does |
| asserting only on status codes, never on payload | a 200 with an empty `Documents` array would pass |
| asserting an id is *absent* with no positive sentinel in the same page | an empty or stale page satisfies it — see rule 6 and step 7 |

Required instead: exact document counts on a freshly created collection (steps 4, 9, 14, 15), and
id-set equality — not just cardinality — in steps 4 and 14.

#### Proof-of-liveness assertion

Add one assertion no mock or stub in this repo can satisfy, and call it from the first `read` in the
suite. `x-ms-activity-id` is server-generated and `x-ms-request-charge` is non-zero for any real
feed read; neither is produced by `Mock_response`'s builders:

```ocaml
let check_served_by_azure headers =
  (match Response_headers.x_ms_activity_id headers with
   | Some id -> Alcotest.(check bool) "server activity id present" true (String.length id > 0)
   | None -> Alcotest.fail "no x-ms-activity-id: response did not come from Azure");
  match Response_headers.x_ms_request_charge headers with
  | Some charge ->
      Alcotest.(check bool) "request charge > 0" true (float_of_string charge > 0.)
  | None -> Alcotest.fail "no x-ms-request-charge: response did not come from Azure"
```

#### Negative control

Add one live test that asserts a real *failure* reaches us: `read` against a non-existent
collection must return `Error (Azure_error (404, _))`. A stubbed or short-circuited harness cannot
produce that 404, so this test fails loudly if the suite ever stops talking to Azure. Keep it in the
live group, not the mock group.

#### How to verify the suites are live (do this, do not assume)

1. `dune runtest` **without** `env.sh` sourced → mock tests pass, live groups skipped, and with
   `COSMOS_REQUIRE_LIVE_TESTS=1` exported the run turns red. Both behaviours must be observed.
2. `source env.sh && dune runtest --verbose` → the `change feed test (live)` group lists every case
   in **both** `Main tests` and `Main tests (Eio)`, with non-zero durations. A live feed read takes
   milliseconds of wall clock, not microseconds; a suspiciously instant group is a fake.
3. Temporarily point `AZURE_COSMOS_ENDPOINT` at an unreachable host → the live change feed tests
   must **fail**. If they still pass, they are not doing I/O. This is the single most valuable check
   in this list and takes one minute.
4. In the CI log, confirm the case count of `change feed test (live)` is identical between the lwt
   and eio runs. A divergence means one runner silently registered `[]`.
5. Confirm the RU charge assertion actually ran by breaking it once (assert `> 1e9`) and watching
   both suites fail.

---

## Phase 7 — Documentation

- `API_IMPROVEMENTS.md`: move **Change Feed** out of "Major Missing Features"; add a
  `Change Feed (pull model)` row to the implemented table; update gap **3. Real-Time Features** to
  note that only conflict resolution and the push-model processor remain; tick Phase 2 item 6 and
  rewrite improvement 8 as "implemented (pull model); processor outstanding"; bump the coverage
  bars.
- `README.md`: a short consumer example — `read ~start_from:Now`, persist `page.continuation`,
  resume with `Start_from.Continuation`, and the `Partition_key_range.ids` + `IO.parallel_map`
  fan-out pattern.
- Document explicitly: `304` is normal; ordering holds only within a partition key; deletes are
  invisible in latest-version mode (use a soft-delete flag + TTL, the standard Cosmos workaround);
  the checkpoint is the caller's responsibility.

---

## Build & verification

```sh
dune build @fmt --auto-promote @ocaml-index
dune runtest
```

Both must be clean. `dune runtest` executes the mock tests unconditionally; the live tests need
`AZURE_COSMOS_KEY` and `AZURE_COSMOS_ENDPOINT` (source `env.sh` locally). The `test` job in
`.github/workflows/main.yml` already exports both.

**One workflow change is required** (correcting the earlier assumption): add
`COSMOS_REQUIRE_LIVE_TESTS: "1"` to that job's `env` block, so that a missing credential fails the
build instead of silently skipping the live groups:

```yaml
      - name: Run tests
        env:
          AZURE_COSMOS_KEY: ${{secrets.AZURE_COSMOS_KEY}}
          AZURE_COSMOS_ENDPOINT: ${{vars.AZURE_COSMOS_ENDPOINT}}
          COSMOS_REQUIRE_LIVE_TESTS: "1"
        run: opam exec -- dune runtest --instrument-with bisect_ppx --force
```

Do not consider this feature done until the Phase 6c verification steps have been run — in
particular, the unreachable-endpoint check that proves the lwt **and** eio change feed tests fail
when Azure is unreachable.

---

## Implementation order / checklist

- [ ] 1. `Pkranges` variant in the `Account` module type + `Auth_credential` impl
       (`databases_core.ml`); `Auth` is a shim and needs no change
- [ ] 2. `Response_headers.x_ms_substatus` (record field, `update` case, accessor, both `.mli`s)
- [ ] 2b. `with_throttle_retry ?timeout` (Phase 1b) — per-attempt `wrap_timeout`, timeouts not
        retried, all existing call sites unchanged. `dune runtest` must stay green **before** any
        change feed code is written
- [ ] 3. ATD `partition_key_range` / `list_partition_key_ranges`; `dune build` to regenerate
- [ ] 4. `Collection.Partition_key_range.list` / `ids` + `path_of_pkranges`
- [ ] 5. Mock tests for the pkranges auth path and parsing — **land before going further**
- [ ] 6. `Change_feed.Mode` / `Start_from` / `Scope` modules with `string_of`
- [ ] 7. `Change_feed.read` (header mapping, throttle retry, 200/304 classification)
- [ ] 8. Mock tests for every start-position/scope header **and the 304-is-`Ok` case** — the
       highest-value tests in this plan; land them before the loops
- [ ] 9. `is_partition_split` + 410/substatus mock test
- [ ] 10. `Change_feed.drain` returning `drain_result` (+ `max_pages` guard, `caught_up`,
        `checkpoint`) and its mock tests, incl. the immediate-304 case
- [ ] 11. `Change_feed.fold` (+ `poll_interval`, `max_polls`, callback-error path) and its tests
- [ ] 12. `Mock_response` helpers (`change_feed_response`, `not_modified_response`,
        `partition_split_response`, `list_partition_key_ranges_response`)
- [ ] 13. Both backend `.mli` files: `Partition_key_range`, `Change_feed`, `x_ms_substatus`,
        doc comments, legacy `?a_im` note
- [ ] 14. `test/core/test_io_intf.ml` `DB` signature extension
- [ ] 15. **Fix the eio test shim** (`sleep`, `with_timeout`, `parallel_map`) — Phase 6c hazard 2.
        Prerequisite for the eio live tests being meaningful; do this *before* writing them
- [ ] 16. `Test_common_core.live_required` / `live_wiring_test` + `COSMOS_REQUIRE_LIVE_TESTS: "1"`
        in `.github/workflows/main.yml` — Phase 6c hazard 1
- [ ] 17. `test/core/change_feed_tests.ml` — steps 1–11 on the main collection, steps 12–16 on a
        second freshly created `changeFeedPagingCollection`, incl. `check_served_by_azure`, the
        step-7 sentinel and the 404 negative control + registration as
        `"change feed test (live)"` in `test/lwt/test.ml` and `test/eio/test.ml`, applied to
        `D = Database (MyAuthKeys)` only
- [ ] 18. Run the five verification steps in Phase 6c, including the unreachable-endpoint check
- [ ] 19. `API_IMPROVEMENTS.md` and `README.md` updates

---

## Explicitly out of scope (first iteration)

| Deferred | Why |
|----------|-----|
| **All-versions-and-deletes mode** (`A-IM: Full-Fidelity Feed`, `x-ms-cosmos-changefeed-wire-format-version: 2021-09-15`) | Three reasons, in order of weight: (a) it needs change feed retention / continuous backups configured on the container, which `create_collection` cannot express today (no `changeFeedPolicy` in the ATD type) and which the CI account is not guaranteed to have — so it could not be covered by a live test; (b) the response body is a different shape (`_metadata` envelope with `operationType`/`crts`/`lsn`, plus `current`/`previous` images), so it needs its own ATD types and parse path rather than `Document.convert_to_list_result`; (c) the semantics differ enough (deletes, TTL expirations, previous images) that folding it into the same `page` record would produce a misleading type. It does **not** require a newer `x-ms-version`: the .NET SDK's full-fidelity mode adds only the `A-IM` and wire-format-version headers and leaves `x-ms-version` alone. (It also forces gateway mode, because full-fidelity split handling lives in the compute gateway — a non-issue here, since this SDK only ever talks to the gateway endpoint.) `Mode.t` is a variant precisely so this is additive later. |
| **Lease-based processor / `subscribe` + `cancel`** | Requires durable leases in a second collection, load balancing across consumers, renewal timers and a cancellation primitive that `Databases_intf.IO` does not expose. Sketched below. |
| **`FeedRange`-based scoping (EPK ranges)** | Undocumented in the REST reference; `partitionkeyrangeid` covers the fan-out use case. |
| **Automatic split recovery inside `drain`/`fold`** | Would silently change the set of ranges a caller is checkpointing. `is_partition_split` makes it detectable; recovery stays the caller's decision until the processor exists. |
| **Typed change envelopes** | Blocked on improvement 3 (strongly typed documents); `page.documents` stays `(string * meta option) list`, consistent with `Document.list`. |
| **Conflict feed** (`/conflicts`) | A separate resource with its own semantics. |

### Sketch of the deferred processor (do not build yet)

```ocaml
module Processor : sig
  type lease = { range_id : string; continuation : string; owner : string; ts : int }

  val run :
    lease_dbname:string -> lease_coll_name:string ->
    owner:string ->
    poll_interval:float ->
    max_polls:int ->                         (* still bounded until IO gains cancellation *)
    on_change:(page -> (unit, string) result io) ->
    string -> string ->
    (unit, cosmos_error) result io
end
```

Leases are ordinary documents (`id = range_id`, partition key `/id`) updated with `If-Match` for
optimistic ownership — which is why this can be built entirely on top of Phases 1–5 and the
existing `Document` module once a cancellation story exists.

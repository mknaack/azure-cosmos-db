# Transactional Batch Branch — Code Review Findings

Review of the `transactional-batch` branch against `main`. Items below are grouped
into what has already been addressed and what remains outstanding.

## Resolved

| # | Finding | Resolution |
|---|---------|------------|
| 1 | Patch `value` serialized as a quoted string, producing malformed JSON in the batch request body | `construct_batch_request_body` now emits `value` with `add_field` (raw JSON) instead of `add_string_field`. Regression test `mock_batch_patch_body_valid_json` added. |
| 2 | `execute` raised `failwith` on validation errors instead of returning a result | Added top-level `batch_validation_error` type and a `Batch_validation_error` `cosmos_error` variant; `execute` now returns `Error (Batch_validation_error _)`. Tests `mock_batch_empty_returns_validation_error` and `mock_batch_too_many_returns_validation_error` added. |
| 4 | Unused generated ATD types (`batch_request`, `batch_operation_result`, `batch_response`) | Removed the dead types from `json_converter.atd`. |
| 5 | Batch header building used a manual `Cohttp.Header.add` ladder | Rewrote with the `add_header` pipe style; promoted `add_header` to functor scope so it is shared. |
| 6 | Inlined path string `"dbs/" ^ dbname ^ "/colls/" ^ coll_name` | Replaced with the `path_of_collection` helper. |

## Outstanding

### 3. `with_throttle_retry` mislabels exhausted throttling as `Timeout_error`

**Location:** `src/cosmos/databases_core.ml`, `with_throttle_retry` retry loop.

When a request is throttled (HTTP 429) and all retries are exhausted, the loop
returns `timeout_error`. This conflates two distinct failure modes: a genuine
client-side timeout versus sustained server-side throttling. Callers that match
on `Timeout_error` cannot distinguish them.

**Suggested fix:** on retry exhaustion, surface the throttle response as
`Azure_error (429, headers)` (or a dedicated `Throttled` variant) rather than
`Timeout_error`, so the error reflects the real cause and preserves the response
headers (including `x-ms-retry-after-ms`).

### 7. Batch `execute` is not wrapped in `with_throttle_retry`

**Location:** `src/cosmos/databases_core.ml`, `Batch.execute`.

Unlike `Document.create` / `delete`, a throttled batch (429) fails immediately
instead of retrying. This is inconsistent with the rest of the SDK.

**Suggested fix:** route the batch POST through `with_throttle_retry` so 429 and
transient connection errors are retried with the same policy as other write
operations. Decide intentionally whether atomic batches should be retried.

### 8. Canonical type name in `Batch` module

**Location:** `src/cosmos/databases_core.ml`, `Batch` module.

Project convention is for the primary type of a module to be named `t`. The
`Batch` module exposes `operation` as its primary type. This reads well, but is
a deviation from the convention used elsewhere. Minor — flag for consistency
only; no behavior impact.

### 9. `atomic` header semantics need verification

**Location:** `src/cosmos/databases_core.ml`, `Batch.execute` header construction.

The implementation sets `x-ms-cosmos-batch-atomic`. Cosmos transactional batch
also uses `x-ms-cosmos-batch-continue-on-error` for non-atomic / continue-on-error
behavior. It is not confirmed that `atomic = false` alone yields the partial-success
semantics assumed by the `non_atomic_partial` test.

**Suggested action:** verify against the Cosmos REST documentation and add the
`continue-on-error` header if required for the intended non-atomic behavior.

### 10. OCaml lower bound raised 4.08 → 4.14

**Location:** `dune-project` and the CI matrix.

The minimum supported OCaml version was raised from 4.08 to 4.14, which is a
compatibility reduction for downstream users.

**Suggested action:** confirm the bump is actually required (e.g. forced by a
dependency) rather than incidental, and document it as a breaking constraint in
the changelog / release notes.

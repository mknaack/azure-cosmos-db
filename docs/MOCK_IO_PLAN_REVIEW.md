# Mock IO Plan — Code Review (Pass 3)

## Overview

The changes introduce a synchronous mock layer (`Mock_io`, `Mock_http`, `Mock_response`,
`Mock_auth`, `Mock_test_runner`) for unit-testing `databases_core.ml` without a live Cosmos
endpoint, wired into both `eio` and `lwt` test runners.

---

## What Was Fixed Since Pass 2

- **`mock_auth.ml` endpoint** — scheme stripped; endpoint is now the bare hostname
  `"mock-account.documents.azure.com"`. Test expectations updated to match.
- **`with_mock` exception/leak fix** — restructured to `Fun.protect ~finally:uninstall`,
  with `verify ()` called inside the protected body on the success path only.
- **`document_response` template** — refactored: `attachments_field` built separately,
  trailing-quote ambiguity removed.

---

## Remaining Issues

### 1. Broad `with _exn ->` catch in `test_mock_http_verify_unconsumed` — LOW

**File:** `test/core/mock_tests.ml:111-113`

```ocaml
  with _exn ->
    (* Expected: with_mock raised due to unconsumed expectations *)
    ()
```

The rename to `_exn` is cosmetic. The catch is still indiscriminate — `Out_of_memory`,
`Stack_overflow`, and any unrelated exception from inside the mock are all silently swallowed.

`Alcotest.fail` raises `Alcotest_engine.V1.Core.Check_error` (aliased as
`Alcotest.Check_error`), which is what `verify ()` raises when expectations are unconsumed.
The fix is to catch only that:

```ocaml
  with Alcotest.Check_error _ -> ()
```

Note: `Alcotest.Test_error` is only raised by `Alcotest.run` itself — it is **not** the right
exception to catch here.

---

### 2. `Mock_test_runner` has no `.mli` — LOW

Unchanged across all three passes. `mock_test_runner.ml` exposes `Mock_db`, `Mock_config`,
`Mock_test_io`, and `Mock_http_impl` with no interface file. An `.mli` would document the
public surface and prevent tests from accidentally depending on internal implementation details
as the module grows.

---

## Minor / Style

- **`_speed` discarded in test wiring.** Both `test/eio/test.ml` and `test/lwt/test.ml`
  discard the speed tag from `Mock_tests.tests`. Since all tests are `` `Quick `` today this is
  harmless, but passing speed through `wrap_sync_tests` and using it directly would be
  consistent with `wrap_async_tests`.

- **`Mock_auth.Auth` double-nesting.** The module is accessed as `Mock_auth.Auth`, which reads
  redundantly. Naming the top-level module `Mock_auth_key` conforming directly to
  `Databases_intf.Auth_key` at the file level would be cleaner.

- **`string_contains` uses `Str.regexp_string`.** Pulling in `str` for a literal substring
  check is heavier than necessary. A hand-rolled check or `String.split_on_char` scan would
  avoid the regex engine dependency entirely.

---

## Summary

| Status | Severity | Issue |
|---|---|---|
| ✅ Fixed | High | `post`/`put` bodies recorded and matched |
| ✅ Fixed | Medium | `with_mock` calls `verify()` on the success path; `uninstall` always runs |
| ✅ Fixed | Medium | `match_uri` compares host + path |
| ✅ Fixed | High | `mock_auth.ml` endpoint bare hostname; test expectations corrected |
| ✅ Fixed | Low | `with_timeout` limitation documented |
| ✅ Fixed | Low | `document_response` template trailing-quote ambiguity |
| 🟡 Remaining | Low | `with _exn ->` catch still indiscriminate — should be `Alcotest.Check_error _` |
| 🟡 Remaining | Low | `Mock_test_runner` has no `.mli` |
| Cosmetic | — | `_speed` discarded; `Mock_auth.Auth` double-nesting; `Str` for substring search |

---
description: Implement feature with automatic self-review
---

When the user asks to "implement" or "add" a feature, follow this complete workflow before presenting any code.

All code standards (build commands, style rules, module structure) are defined in `.windsurfrules` — consult it during implementation.

## Phase 0: Classify the Change

Determine scope before writing any code:

- **Small** — a single function or minor addition (≤ ~20 lines, no new types or modules). Use the Fast Path below.
- **Large** — a new module, new type with operations, or multiple related functions. Use the Incremental Loop below.

---

## Fast Path (small changes only)

1. Implement the change
2. Write or update the test
3. `dune build @fmt --auto-promote @ocaml-index` — fix if it fails
4. `dune runtest` — fix if it fails
5. Quick structure check: naming convention, no inline path strings, no duplicated helper
6. Present

---

## Incremental Loop (large changes)

### Step A: Decompose

Before writing any code, decompose the feature into the smallest independently testable units. Example for a new module:
- Unit 1: core type(s) and their basic constructors
- Unit 2: serialisation / conversion functions
- Unit 3: HTTP operation(s)
- Unit 4: `.mli` public surface

State the decomposition explicitly, then proceed unit by unit.

### Step B: Per-Unit Loop (repeat for each unit)

**B1 — Implement the unit**
Check `databases_core.ml` for existing helpers before writing new logic:
- `apply_to_header_if_some`, `add_header`, `handle_response`, `make_uri`, `header_path_of_path`
- `Utilities.take_first` for chunk processing
- `with_throttle_retry` for 429/retry handling
- Path-building helpers (`path_of_doc`, `path_of_docs`, `path_of_collection`, `path_of_collections`)

**B2 — Write the test for this unit**
- At least one happy path and one error path (404, 429, timeout, or connection refused)
- Unit tests in `test/core/`, integration tests in `test/cosmos/` or backend dir
- Add to the relevant `dune` stanza

**B3 — Build and test**
- `dune build @fmt --auto-promote @ocaml-index` — fix if it fails, do not continue
- `dune runtest` — fix if it fails, do not continue

**B4 — Structure review (OCaml module design persona)**
*Narrow focus: structure and duplication only.*
- [ ] Any new type with more than one operation? → lift into a module with `type t`
- [ ] Any logic duplicated between backends? → move to `src/cosmos/`
- [ ] Any new helper identical in structure to an existing one? → remove and reuse
- [ ] Any inline path string? → use `path_of_*` helper
- [ ] Naming: `string_of_*` / `*_of_string` for conversions?

If issues found: fix → repeat B3.

### Step C: Cross-Unit Refactor

*After all units pass, adopt the role of an OCaml refactoring expert. Look across all new code together.*
- [ ] Any duplication across units that can be extracted into a shared helper?
- [ ] Any module boundary that should be split or merged now that the full shape is visible?
- [ ] Is the `.mli` surface minimal — hiding internal helpers, exposing only what callers need?

Build and test again after any changes.

### Step D: API Consistency Review

*Adopt the role of an API design reviewer. Review only the public interface.*
- [ ] Argument order: optional args first, then `dbname`, `coll_name`, resource id, `?timeout` last
- [ ] Data responses: `(int * 'a, cosmos_error) result IO.t`; no-body responses: `(int, cosmos_error) result IO.t`
- [ ] New `cosmos_error` variants defined at the top of `databases_core.ml`, not inline
- [ ] `Response_headers.t` exposed where the caller may need continuation tokens, ETags, or request-charge

### Step E: Test Quality Review

*Adopt the role of a testing expert. Review all tests written across units.*
- [ ] Every operation has at least one error path test
- [ ] Tests assert on specific return values, not just success/failure
- [ ] No test was weakened or deleted

### Step F: Final Build & Test

- `dune build @fmt --auto-promote @ocaml-index`
- `dune runtest`

Both must pass before presenting.

---

## Presentation

1. Summary of implementation
2. Files changed
3. Test coverage added
4. Notable trade-offs or decisions

Do not present until all applicable phases are complete and green.

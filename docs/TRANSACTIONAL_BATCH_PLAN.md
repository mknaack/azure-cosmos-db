# Implementation Plan: Transactional Batch Support for Azure Cosmos DB

## Overview

Transactional batch allows grouping multiple document operations (Create, Upsert, Read, Delete, Replace, Patch) with the same partition key into a single atomic request. All operations succeed or fail together.

**Azure Cosmos DB REST API Reference**: https://learn.microsoft.com/en-us/rest/api/cosmos-db/transactional-batch

## Architecture Decisions

- Follow existing **functor pattern** (`Make (IO) (Http) (Auth)` in `databases_core.ml`)
- Use **ATD for JSON serialization** (consistent with existing code)
- Same **testing pattern**: `test/core/batch_tests.ml` + wrappers in `test/lwt/` and `test/eio/`
- Atomic batches are all-or-nothing; non-atomic allows partial success
- **Error handling**: Return per-operation status codes; caller determines success/failure (matches actual Cosmos API behavior)
- **Builder pattern**: Optional helper module for ergonomic batch construction

---

## Phase 1: Data Types & JSON Serialization

**File**: `src/cosmos/json_converter.atd`

Add batch operation types. **Note**: ATD uses different syntax for variants and `abstract` for raw JSON strings.

```ocaml
type batch_operation_type = [ Create | Upsert | Read | Delete | Replace | Patch ]

type batch_operation = {
  operationType: batch_operation_type;
  partitionKey <json name="partitionKey">: string;  (* Required per operation *)
  ?ifMatch <json name="ifMatch">: string option;
  ?ifNoneMatch <json name="ifNoneMatch">: string option;
  ?id: string option;  (* Required for Read/Delete/Patch *)
  ?resourceBody <json name="resourceBody">: string option;  (* JSON string for Create/Upsert/Replace *)
  ?from <json name="from">: string option;  (* For Patch: source path *)
  ?value <json name="value">: string option;  (* For Patch: new value *)
}

type batch_request = batch_operation list

type batch_operation_result = {
  statusCode <json name="statusCode">: int;
  requestCharge <json name="requestCharge">: float;
  ?eTag <json name="eTag">: string option;
  ?resourceBody <json name="resourceBody">: string option;  (* JSON string *)
}

type batch_response = batch_operation_result list
```

---

## Phase 2: Core Implementation

**File**: `src/cosmos/databases_core.ml` (inside the `Make` functor, within `Collection` module)

Add new `Batch` module with simplified error handling:

```ocaml
module Collection = struct
  (* ... existing code ... *)
  
  module Batch = struct
    type operation = 
      | Create of { if_match: string option; if_none_match: string option; body: string }
      | Upsert of { if_match: string option; if_none_match: string option; body: string }
      | Read of { id: string; if_match: string option; if_none_match: string option }
      | Delete of { id: string; if_match: string option; if_none_match: string option }
      | Replace of { id: string; if_match: string option; if_none_match: string option; body: string }
      | Patch of { id: string; if_match: string option; patch_op: patch_operation }
    
    and patch_operation =
      | Add of { path: string; value: string }      (* Add field if not exists *)
      | Set of { path: string; value: string }    (* Set/replace field *)
      | Replace of { path: string; value: string } (* Synonym for Set *)
      | Remove of { path: string }                (* Remove field *)
      | Increment of { path: string; value: int } (* Atomically increment *)

    type operation_result = {
      status_code: int;           (* 200/201 = success, 4xx/5xx = failure *)
      request_charge: float;
      etag: string option;        (* Only for successful operations *)
      resource_body: string option; (* JSON string, only for Read/Create/Upsert success *)
    }

    type batch_result = {
      outcomes: operation_result list;  (* Same order as input operations *)
      total_request_charge: float;
    }
    
    type validation_error = 
      | Too_many_operations of int  (* Exceeds 100 operation limit *)
      | Mixed_patch_operations      (* Patch mixed with Create/Delete/Replace *)
      | Empty_batch
    
    (* Optional validation before sending *)
    val validate : operation list -> (unit, validation_error) result

    val execute :
      ?timeout:float ->
      ?atomic:bool ->
      ?validate:bool ->           (* Validate before sending, default: true *)
      partition_key:string ->
      dbname:string ->
      coll_name:string ->
      operation list ->
      (batch_result, cosmos_error) result io
      
    (* Check if an operation succeeded based on status code *)
    val is_success : operation_result -> bool
  end
end
```

### Implementation Details

| Aspect | Value |
|--------|-------|
| **Endpoint** | `POST /dbs/{db}/colls/{coll}/docs` |
| **Method** | POST |

### Headers

| Header | Value |
|--------|-------|
| `x-ms-cosmos-is-batch-request` | `True` |
| `x-ms-documentdb-partitionkey` | `["{partition_key}"]` |
| `x-ms-cosmos-batch-atomic` | `true` or `false` (optional) |
| `content-type` | `application/json` |

### Request Body Format

**Note**: Each operation must include its own `partitionKey` field serialized as a JSON array string.

```json
[
  {
    "operationType": "Create",
    "partitionKey": "[\"testPk\"]",
    "ifMatch": "",
    "ifNoneMatch": "",
    "resourceBody": "{\"id\": \"doc1\", \"name\": \"Microsoft\", \"pk\": \"testPk\"}"
  },
  {
    "operationType": "Read",
    "partitionKey": "[\"testPk\"]",
    "id": "doc2",
    "ifMatch": "",
    "ifNoneMatch": ""
  },
  {
    "operationType": "Patch",
    "partitionKey": "[\"testPk\"]",
    "id": "doc3",
    "ifMatch": "\"0000-0000\"",
    "from": "",
    "value": "{\"path\": \"/count\", \"op\": \"incr\", \"value\": 1}"
  }
]
```

### Response Parsing

HTTP 200 response with body containing array of operation results:

```json
[
  {
    "statusCode": 201,
    "requestCharge": 7.05,
    "eTag": "\"00000000-0000-0000-6df8-18bb188801db\"",
    "resourceBody": {
      "id": "doc1",
      "name": "Microsoft",
      "pk": "testPk",
      "_rid": "...",
      "_self": "...",
      "_etag": "...",
      "_ts": 1737679027
    }
  }
]
```

---

## Phase 3: Lwt & Eio Interface Updates

### Files to Update

- `src/cosmos_lwt/databases.mli`
- `src/cosmos_eio/databases.mli`

Add to `Collection` module signature:

```ocaml
module Batch : sig
  type operation =
    | Create of { if_match: string option; if_none_match: string option; body: string }
    | Upsert of { if_match: string option; if_none_match: string option; body: string }
    | Read of { id: string; if_match: string option; if_none_match: string option }
    | Delete of { id: string; if_match: string option; if_none_match: string option }
    | Replace of { id: string; if_match: string option; if_none_match: string option; body: string }
    | Patch of { id: string; if_match: string option; patch_op: patch_operation }
  
  and patch_operation =
    | Add of { path: string; value: string }
    | Set of { path: string; value: string }
    | Replace of { path: string; value: string }
    | Remove of { path: string }
    | Increment of { path: string; value: int }

  type operation_result = {
    status_code: int;
    request_charge: float;
    etag: string option;
    resource_body: string option;
  }

  type batch_result = {
    outcomes: operation_result list;
    total_request_charge: float;
  }
  
  type validation_error = 
    | Too_many_operations of int
    | Mixed_patch_operations
    | Empty_batch
  
  val validate : operation list -> (unit, validation_error) result
  val is_success : operation_result -> bool

  val execute :
    ?timeout:float ->
    ?atomic:bool ->
    ?validate:bool ->
    partition_key:string ->
    dbname:string ->
    coll_name:string ->
    operation list ->
    (batch_result, cosmos_error) result io
end

module Batch_builder : sig
  type t
  
  val empty : t
  val add_create : ?if_match:string -> ?if_none_match:string -> body:string -> t -> t
  val add_upsert : ?if_match:string -> ?if_none_match:string -> body:string -> t -> t
  val add_read : ?if_match:string -> ?if_none_match:string -> id:string -> t -> t
  val add_delete : ?if_match:string -> ?if_none_match:string -> id:string -> t -> t
  val add_replace : ?if_match:string -> ?if_none_match:string -> id:string -> body:string -> t -> t
  val add_patch : ?if_match:string -> id:string -> patch_op:Batch.patch_operation -> t -> t
  val to_operations : t -> Batch.operation list
  val length : t -> int
end
```

**Note**: Implementation files need no changes if using the existing functor pattern. The builder module is an optional ergonomic addition.

---

## Phase 4: Test Infrastructure

### New File: `test/core/batch_tests.ml`

Create functorized tests following `integration_tests.ml` pattern:

```ocaml
open Test_common_core
open Cosmos.Databases_core

module Make (Cfg : Test_io_intf.Config) (IO : Test_io_intf.IO) (D : Test_io_intf.DB with type 'a io := 'a IO.t) = struct
  let ( let* ) = IO.bind
  let dbname = dbname_partition Cfg.prefix
  let coll_name = collection_name_partition

  let create_doc_json id name pk = 
    Printf.sprintf {|{"id": "%s", "name": "%s", "pk": "%s"}|} id name pk

  let create_batch_test () =
    let partition_key = "test-pk" in
    let ops = [
      Batch.Create { if_match = None; if_none_match = None; body = create_doc_json "doc1" "Alice" partition_key };
      Batch.Create { if_match = None; if_none_match = None; body = create_doc_json "doc2" "Bob" partition_key };
    ] in
    let* result = D.Collection.Batch.execute ~partition_key dbname coll_name ops in
    match result with
    | Ok { outcomes; _ } ->
        List.iteri (fun i r ->
          Alcotest.(check bool) (Printf.sprintf "Operation %d succeeded" i) true (Batch.is_success r)
        ) outcomes;
        IO.return ()
    | Error _ -> Alcotest.fail "Batch should succeed"

  let atomic_batch_rollback_test () =
    (* First create a doc, then batch create with same ID + another new doc.
       Atomic batch should fail entirely. *)
    IO.return ()

  let mixed_operations_test () =
    (* Test Create + Read + Replace + Delete in single batch *)
    IO.return ()
    
  let empty_batch_test () =
    (* Empty batch should return validation error or Azure 400 *)
    let* result = D.Collection.Batch.execute ~partition_key:"pk" dbname coll_name [] in
    match result with
    | Error _ -> IO.return ()  (* Expected *)
    | Ok _ -> Alcotest.fail "Empty batch should fail"
    
  let max_operations_test () =
    (* Test that 100 ops succeeds, 101 fails with validation error *)
    let ops = List.init 101 (fun i -> 
      Batch.Create { if_match = None; if_none_match = None; 
        body = create_doc_json (string_of_int i) "name" "pk" }
    ) in
    match Batch.validate ops with
    | Error (Too_many_operations 101) -> IO.return ()
    | _ -> Alcotest.fail "Should fail with Too_many_operations"
    
  let batch_builder_test () =
    (* Test using Batch_builder module *)
    let open Collection.Batch_builder in
    let ops =
      empty
      |> add_create ~body:(create_doc_json "b1" "Builder" "pk")
      |> add_read ~id:"existing-doc"
      |> to_operations
    in
    Alcotest.(check int) "Builder has 2 operations" 2 (length ops - 1 + 1);
    IO.return ()

  let tests = [
    "create batch", create_batch_test;
    "atomic rollback", atomic_batch_rollback_test;
    "mixed operations", mixed_operations_test;
    "empty batch", empty_batch_test;
    "max operations limit", max_operations_test;
    "batch builder", batch_builder_test;
  ]
end
```

### Update: `test/core/test_io_intf.ml`

Add `Batch` submodule to `DB.Collection` signature.

### Update: `test/lwt/test.ml`

```ocaml
module Batch = Test_core.Batch_tests.Make (Lwt_config) (Lwt_test_io) (D)

(* Add to test list *)
let batch_tests =
  if Test_core.Test_common_core.should_run () then
    wrap_async_tests `Slow Batch.tests
  else []
```

### Update: `test/eio/test.ml`

Same pattern as Lwt tests.

---

## Phase 5: Testing Strategy

| Test Case | Description |
|-----------|-------------|
| Create multiple docs | Verify 200 response with 201 status codes for each |
| Upsert existing + new | Test atomic behavior with mixed exists/not-exists |
| Read operations | Include reads in batch, verify resourceBody returned |
| Delete operations | Create then delete in same batch |
| Replace operations | Test conditional replace with ETags |
| Patch operations | Test Add/Set/Remove/Increment patch ops |
| Atomic failure | Ensure all-or-nothing when `~atomic:true` |
| Non-atomic partial | Verify partial success when `~atomic:false` |
| Invalid partition key | Verify error handling for wrong PK |
| Timeout handling | Test `~timeout` parameter |
| Mixed operation types | Test Create + Read + Replace + Delete in one batch |
| Empty batch | Verify proper error for empty operation list |
| Max operations (100) | Test 100 ops succeed, 101 fail |
| Builder pattern | Test Batch_builder ergonomics |
| Validation disabled | Test `~validate:false` bypasses local validation |
| Patch isolation | Verify error when mixing Patch with Create/Delete |

---

## File Changes Summary

| File | Action | Est. Lines |
|------|--------|------------|
| `src/cosmos/json_converter.atd` | Add batch types (fixed syntax) | +30 |
| `src/cosmos/databases_core.ml` | Add `Batch` + `Batch_builder` modules | +120 |
| `src/cosmos_lwt/databases.mli` | Expose `Batch` and `Batch_builder` | +35 |
| `src/cosmos_eio/databases.mli` | Expose `Batch` and `Batch_builder` | +35 |
| `test/core/test_io_intf.ml` | Add `Batch` to `DB.Collection` | +30 |
| `test/core/batch_tests.ml` | **New** batch test functor | +180 |
| `test/lwt/test.ml` | Wire up batch tests | +5 |
| `test/eio/test.ml` | Wire up batch tests | +5 |

**No changes needed**: `dune-project`, `src/cosmos/dune`, package dependencies

---

## Estimated Effort

| Task | Time |
|------|------|
| Core implementation (types + execute + validation) | ~3 hours |
| Builder module (optional but recommended) | ~1 hour |
| Testing (unit + integration + edge cases) | ~2.5 hours |
| Documentation & refinement | ~1 hour |
| **Total** | **~7.5 hours** |

---

## Usage Example

### Basic Usage

```ocaml
open Cosmos_lwt.Databases

module D = Database (MyAuth)

let run_batch () =
  let operations = [
    Collection.Batch.Create {
      if_match = None;
      if_none_match = None;
      body = {|{"id": "user1", "name": "Alice", "pk": "users"}|}
    };
    Collection.Batch.Create {
      if_match = None;
      if_none_match = None;
      body = {|{"id": "user2", "name": "Bob", "pk": "users"}|}
    };
    Collection.Batch.Read {
      id = "existing-doc";
      if_match = None;
      if_none_match = None;
    };
  ] in
  
  let%lwt result = 
    D.Collection.Batch.execute 
      ~partition_key:"users" 
      ~atomic:true 
      "mydb" "mycoll" 
      operations 
  in
  
  match result with
  | Ok { outcomes; total_request_charge } -> 
      List.iteri (fun i r -> 
        if Collection.Batch.is_success r then
          Printf.printf "Op %d: success (charge: %.2f)\n" i r.request_charge
        else
          Printf.printf "Op %d: failed with %d\n" i r.status_code
      ) outcomes;
      Printf.printf "Total charge: %.2f\n" total_request_charge
  | Error e -> handle_error e
```

### Using the Builder Pattern

```ocaml
let run_batch_with_builder () =
  let open D.Collection.Batch_builder in
  
  let ops =
    empty
    |> add_create ~body:{|{"id": "doc1", "value": 100}|}
    |> add_patch ~id:"counter-doc" 
         ~patch_op:(Increment { path = "/count"; value = 1 })
    |> add_read ~id:"doc1"
    |> to_operations
  in
  
  let%lwt result = D.Collection.Batch.execute ~partition_key:"pk" "db" "coll" ops in
  (* ... *)
  Lwt.return ()
```

---

## Batch Limitations & Constraints

Document these clearly for users:

| Constraint | Value | Notes |
|------------|-------|-------|
| **Max operations** | 100 per batch | Exceeding this returns `Too_many_operations` error |
| **Max payload** | 2 MB | Larger payloads will be rejected by Cosmos |
| **Partition key** | Must be consistent | All operations must target same partition key value |
| **Patch isolation** | No mixing | Patch cannot be combined with Create/Delete/Replace in same batch |
| **Idempotent creates** | Use `if_none_match:"*"` | Prevents duplicates on retry |
| **ETag conditions** | Use `if_match` | For optimistic concurrency in Replace/Delete |
| **Atomic behavior** | All-or-nothing | When `~atomic:true`, any failure rolls back entire batch |

---

## Critical Implementation Notes

### 1. Partition Key Serialization

The `partitionKey` field in each operation must be a JSON array string:
```ocaml
let format_partition_key pk = Printf.sprintf "[%S]" pk
(* "[\"testPk\"]" *)
```

### 2. Response Handling

Always returns HTTP 200 for valid batch requests. Check individual `status_code`:
- `200` / `201` = Success
- `400` = Bad request (e.g., malformed body)
- `404` = Document not found (Read/Delete/Replace/Patch)
- `409` = Conflict (Create with duplicate ID)
- `412` = Precondition failed (ETag mismatch)

### 3. Error Type Simplification

The original plan had a complex `batch_error` type. **Simplified approach**:
- `Ok batch_result` = Batch request succeeded (check individual status codes)
- `Error cosmos_error` = Network/auth/timeout failure (same as other operations)

This matches the actual Cosmos DB REST API behavior where the batch itself always returns 200, and per-operation failures are indicated via status codes.

### 4. ATD Type Fixes

Original plan had incorrect ATD syntax. Key fixes:
- Use `[ Foo | Bar ]` for polymorphic variants (not OCaml-style `[ | Foo | Bar ]`)
- Use `string` for JSON bodies, not `json` (undefined in ATD)
- Add `<json name="...">` annotations for camelCase fields
- Include `partitionKey` as required field per operation


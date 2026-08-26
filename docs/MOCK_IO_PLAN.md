# Mock IO Implementation Plan for azure-cosmos-db

## Goal
Enable more tests to run without environment variables by providing mock implementations of the `IO` and `Http_client` modules. This will allow testing business logic, error handling, JSON parsing, and header construction without requiring a live Cosmos DB connection.

## Current Architecture

### Module Hierarchy
```
Databases_intf.IO          - Interface: type 'a t, return, bind, catch, sleep, with_timeout, parallel_map
Databases_intf.Http_client - Interface: get, post, put, delete with http_error type
Databases_core.Make        - Functor: Make(IO)(Http)(Auth_key) -> Database operations
```

### Existing Implementations
| Backend | IO Module | Http Module | Type `'a t` |
|---------|-----------|-------------|-------------|
| Lwt     | `Lwt_io`  | `Lwt_http`  | `'a Lwt.t`  |
| Eio     | `Eio_io`  | `Eio_http`  | `unit -> 'a` (thunk) |

### Current Test Structure
- Tests are organized as functors: `Make(Cfg : Config)(IO : IO)(D : DB)`
- Tests only run when `AZURE_COSMOS_KEY` and `AZURE_COSMOS_ENDPOINT` are set
- Pure utility tests (e.g., `Test_utilities`) run without credentials

## Proposed Mock Implementation

### 0. Integration with Existing Utilities

Your recent refactoring moved `apply_to_header_if_some` to `Utilities`. The mock tests should verify headers are constructed correctly using these shared utilities:

```ocaml
(* Test that headers are correctly built using Utilities functions *)
let verify_headers_test () =
  let base = Cohttp.Header.init () in
  let with_upsert = 
    Utilities.apply_to_header_if_some "x-ms-documentdb-is-upsert" 
      string_of_bool (Some true) base 
  in
  Alcotest.(check (option string))
    "Upsert header present" 
    (Some "true") 
    (Cohttp.Header.get with_upsert "x-ms-documentdb-is-upsert")
```

### 1. Mock IO Module (`test/core/mock_io.ml`)

A synchronous, immediate IO implementation using `type 'a t = 'a`:

```ocaml
type +'a t = 'a

let return x = x
let bind x f = f x
let catch f handler = try f () with exn -> handler exn
let sleep _secs = ()  (* immediate - no delay *)

let with_timeout t cmd = 
  (* Configurable: can simulate timeout if t <= 0 *)
  if t <= 0.0 then None else Some cmd

let parallel_map f xs = List.map f xs

(* For tests that need to verify timeout behavior *)
let enable_timeouts = ref false
let with_timeouts_enabled f = 
  let old = !enable_timeouts in
  enable_timeouts := true;
  Fun.protect ~finally:(fun () -> enable_timeouts := old) f
```

Benefits:
- Synchronous execution makes tests deterministic and fast
- No need for `Lwt_main.run` or `Eio_main.run` wrappers
- Stack traces are clear when tests fail
- Can simulate timeouts when needed via `with_timeouts_enabled`

### 2. Mock HTTP Client (`test/core/mock_http.ml`)

A configurable mock that records requests and returns programmed responses:

```ocaml
type 'a io = 'a Mock_io.t  (* = 'a *)

type expectation = {
  method : [`Get | `Post | `Put | `Delete];
  uri : Uri.t;
  expected_headers : (string * string) list;
  response : (Cohttp.Response.t * string, http_error) result;
}

type t = {
  mutable expectations : expectation Queue.t;
  mutable recorded : (request * response) list;
}

val create : unit -> t
val expect : t -> expectation -> unit
val verify : t -> unit  (* assert all expectations consumed *)
val get : t -> headers:Cohttp.Header.t -> Uri.t -> (Cohttp.Response.t * string, http_error) result io
```

### 3. Response Builder (`test/core/mock_response.ml`)

Helper functions to construct valid Cosmos DB responses:

```ocaml
val make_response : ?status:int -> ?headers:Cohttp.Header.t -> string -> Cohttp.Response.t * string

val database_response : id:string -> _rid:string -> unit -> string
val collection_response : id:string -> _rid:string -> partition_key:string -> unit -> string
val document_response : id:string -> _rid:string -> json:string -> unit -> string
val list_databases_response : (string * string) list -> string  (* [(id, _rid), ...] *)

val error_response : code:int -> message:string -> Cohttp.Response.t * string
```

### 4. Leveraging Existing Test Infrastructure

The project already has excellent functor-based test infrastructure in `test_io_intf.ml`. The mock should plug into this cleanly:

```ocaml
(* test/core/mock_test_io.ml - implements Test_io_intf.IO *)
module Mock_test_io : Test_io_intf.IO with type 'a t = 'a = struct
  include Mock_io
  let run x = x  (* Synchronous - just return the value *)
end

(* Lwt test IO - already exists in test/lwt/test.ml, but unify here *)
module Lwt_test_io : Test_io_intf.IO with type 'a t = 'a Lwt.t = struct
  include Lwt_io
  let run = Lwt_main.run
end

(* Eio test IO - already exists in test/eio/test.ml *)
module Eio_test_io : Test_io_intf.IO with type 'a t = unit -> 'a = struct
  include Eio_io
  let run thunk = thunk ()
end
```

**Key insight**: The existing `Test_io_intf.IO` already extends `Databases_intf.IO` with `run`. The mock just needs to provide a synchronous implementation.

Create mock-specific test runners that reuse the existing test functors:

```ocaml
(* test/core/mock_test_runner.ml *)
module Mock_config : Test_io_intf.Config = struct let prefix = "mock" end

module Mock_auth : Cosmos.Databases_intf.Auth_key = struct
  let master_key = "dGhlcXVpY2ticm93bmZveGp1bXBzb3ZlcnRoZWxhenlkb2c="  (* base64 test key *)
  let endpoint = "https://mock-account.documents.azure.com"
end

(* Build the database module with mock IO *)
module Mock_db = Cosmos.Databases_core.Make(Mock_io)(Mock_http)(Mock_auth)

(* Instantiate ALL existing test functors with mock IO *)
module Mock_integration = Integration_tests.Make(Mock_config)(Mock_test_io)(Mock_db)
module Mock_users = Users_tests.Make(Mock_config)(Mock_test_io)(Mock_db)
module Mock_permissions = Permission_tests.Make(Mock_config)(Mock_test_io)(Mock_db)
module Mock_batch = Batch_tests.Make(Mock_config)(Mock_test_io)(Mock_db)
```

This approach **reuses all existing test definitions** - no test code duplication needed.

## Implementation Phases

### Phase 1: Core Mock Infrastructure

Files to create:
1. `test/core/mock_io.ml` / `.mli` - Synchronous IO implementation
2. `test/core/mock_http.ml` / `.mli` - Configurable mock HTTP client
3. `test/core/mock_response.ml` / `.mli` - Response builder utilities
4. `test/core/mock_test_runner.ml` - Shared mock setup

Update:
- `test/core/dune` - Add mock modules to the library

### Phase 2: Port Pure Logic Tests

Identify and port tests that don't require real network:

| Test Category | Examples | Mock Approach |
|--------------|----------|---------------|
| Authorization header | Verify `authorization` header format | Mock HTTP capture request headers |
| Error handling | Timeout, 404, 429 retry | Program specific error responses |
| JSON parsing | Response body parsing | Use realistic JSON fixtures |
| Request construction | Path building, query params | Verify URI in mock expectations |
| Retry logic | Throttle retry with 429 | Return 429 then success |
| Batch validation | Empty batch, too many ops | Direct function testing (no IO needed) |

Specific test cases to mock-enable:
- `create_database_with_partition_key_test` - mock 201 response
- `create_database_if_not_exists_with_partition_key_test` - mock 200/201 sequence
- `list_databases_with_partition_key_test` - mock list response
- Batch validation tests - run without any IO
- Document CRUD flow - mock full lifecycle

### Phase 3: Three-Layer Test Strategy

Structure tests in three distinct layers, each with different trade-offs:

#### Layer 1: Unit Tests (Pure Logic, No IO)

These test pure functions that don't require IO at all. They run instantly and always.

```ocaml
(* test/core/batch_validation_tests.ml *)
let batch_validate_empty () =
  let open Cosmos.Databases_core.Collection.Batch in
  let result = validate [] in
  Alcotest.(check (result unit validation_error)) 
    "Empty batch fails" (Error Empty_batch) result

let batch_validate_too_many () =
  let open Cosmos.Databases_core.Collection.Batch in
  let ops = List.init 101 (fun i -> Create { if_match=None; if_none_match=None; 
    body = Printf.sprintf "{\"id\": \"doc%d\"}" i }) in
  let result = validate ops in
  Alcotest.(check (result unit validation_error))
    "Too many operations fails" (Error (Too_many_operations 101)) result

let batch_validate_mixed_patch () =
  let open Cosmos.Databases_core.Collection.Batch in
  let ops = [
    Create { if_match=None; if_none_match=None; body = "{}" };
    Patch { id="x"; if_match=None; patch_op=Add {path="/a"; value="1"} }
  ] in
  let result = validate ops in
  Alcotest.(check (result unit validation_error))
    "Mixed patch operations fails" (Error Mixed_patch_operations) result
```

**Key insight**: Batch validation is already pure - no mocking needed! Just test it directly.

#### Layer 2: Mock IO Tests (Fast, Deterministic)

These use mock IO to test the full stack without network calls.

```ocaml
(* test/core/mock_database_tests.ml *)
let mock_create_database () =
  let open Mock_http in
  let http = create () in
  expect http
    { method = `Post; 
      uri = Uri.make ~scheme:"https" ~host:"mock-account.documents.azure.com" 
        ~port:443 ~path:"dbs" ();
      expected_headers = [("x-ms-version", "2018-12-31")];
      response = Ok (Mock_response.make_response ~status:201 
        (Mock_response.database_response ~id:"testdb" ~_rid:"abc123" ())) };
  
  let result = Mock_db.create "testdb" in
  Alcotest.(check int) "Status 201" 201 (fst (Result.get_ok result));
  verify http

let mock_create_if_not_exists_exists () =
  let open Mock_http in
  let http = create () in
  (* First call: GET returns 200 (exists) *)
  expect http
    { method = `Get; uri = Uri.make ~path:"dbs/testdb" ();
      expected_headers = [];
      response = Ok (Mock_response.make_response ~status:200
        (Mock_response.database_response ~id:"testdb" ~_rid:"abc123" ())) };
  (* No second call needed - should NOT POST *)
  
  let result = Mock_db.create_if_not_exists "testdb" in
  Alcotest.(check int) "Status 200 (existing)" 200 (fst (Result.get_ok result));
  verify http

let mock_429_retry_test () =
  let open Mock_http in
  let http = create () in
  (* First call returns 429 with retry-after *)
  expect http
    { method = `Post; uri = Uri.make ~path:"/dbs/testdb/colls/testcoll/docs" ();
      expected_headers = [];
      response = Ok (Mock_response.make_response ~status:429 
        ~headers:(Cohttp.Header.of_list [("x-ms-retry-after-ms", "100")]) "") };
  (* Second call succeeds *)
  expect http
    { method = `Post; uri = Uri.make ~path:"/dbs/testdb/colls/testcoll/docs" ();
      expected_headers = [];
      response = Ok (Mock_response.make_response ~status:200 "{}") };
  
  let result = Mock_db.Collection.Document.create ~partition_key:"pk" 
    "testdb" "testcoll" "{}" in
  Alcotest.(check int) "Status 200 after retry" 200 (fst (Result.get_ok result));
  verify http
```

#### Layer 3: Integration Tests (Real Cosmos DB)

These remain unchanged - they test against the actual service when credentials are available.

```ocaml
(* test/lwt/test.ml - unchanged *)
let integration_tests =
  if Test_core.Test_common_core.should_run () then
    wrap_async_tests `Slow Integration.tests
  else []
```

**Test Matrix**:

| Test Type | Speed | Needs Creds | Coverage |
|-----------|-------|-------------|----------|
| Unit | <1ms | No | Pure logic |
| Mock | ~1-10ms | No | IO orchestration |
| Integration | ~100-1000ms | Yes | Real service |

### Phase 4: Test Discovery and CI Integration

Update test runners:

```ocaml
(* test/test.ml - updated entry point *)
let () =
  let mock_tests = Mock_test_runner.all_mock_tests in
  let unit_tests = Test_utilities.tests @ Test_cosmos_utility.tests in
  let integration_tests = 
    if Test_common_core.should_run () 
    then Integration_test.test_partition_key @ Users_test.test @ ...
    else []
  in
  Alcotest.run "azure-cosmos-db tests" 
    (mock_tests @ unit_tests @ integration_tests)
```

Benefits:
- Mock + unit tests run on every CI build
- Integration tests run only with credentials
- Fast feedback loop for developers

## File Structure

```
test/
├── core/
│   ├── dune
│   ├── mock_io.ml              # Synchronous IO
│   ├── mock_io.mli
│   ├── mock_http.ml            # HTTP mock with expectations
│   ├── mock_http.mli
│   ├── mock_response.ml        # Response builders
│   ├── mock_response.mli
│   ├── mock_auth.ml            # Fixed test credentials
│   ├── mock_test_runner.ml     # Shared mock test setup
│   ├── mock_tests.ml           # New mock-based tests
│   └── ...
├── lwt/
│   └── test.ml                 # Uses real Lwt IO
├── eio/
│   └── test.ml                 # Uses real Eio IO
└── test.ml                     # Entry point - runs all applicable tests
```

## What NOT to Mock

Some things are better tested with real implementations or as pure unit tests:

| Don't Mock | Why | Test Instead With |
|------------|-----|-------------------|
| JSON serialization/deserialization | ATD generated code is trusted | Direct tests with sample JSON |
| Authorization token generation | Cryptographic operations need real validation | Property tests with known inputs/outputs |
| `Utilities` functions | Pure logic | Direct unit tests |
| `Verb` / `Ms_time` modules | Pure logic | Direct unit tests (already done in `test_utilities.ml`) |

Focus mock testing on: HTTP orchestration, error handling, retry logic, and header construction.

## Build and Test Commands

Per the project rules:

```bash
# Build and format
dune build @fmt --auto-promote @ocaml-index

# Run all tests (mock + unit always, integration if env vars set)
dune runtest

# Run only quick tests (no integration)
dune runtest -- -q

# Run only mock/unit tests
dune exec test/core/test_mock.exe
```

## Testing the Mock Implementation

Create a simple verification test:

```ocaml
(* test/core/mock_self_test.ml *)
let test_mock_io_bind () =
  let module IO = Mock_io in
  let ( let* ) = IO.bind in
  let* x = IO.return 5 in
  let* y = IO.return 10 in
  Alcotest.(check int) "Bind chains" 15 (x + y)

let test_mock_http_expectation () =
  let http = Mock_http.create () in
  Mock_http.expect http
    { method = `Get; uri = Uri.of_string "https://test.com";
      expected_headers = []; response = Ok (Mock_response.make_response "OK") };
  let result = Mock_http.get http ~headers:(Cohttp.Header.init ()) 
    (Uri.of_string "https://test.com") in
  Alcotest.(check (result (pair response string) http_error))
    "Returns expected" (Ok (response, "OK")) result;
  Mock_http.verify http  (* passes - all expectations consumed *)

let test_mock_timeout_simulation () =
  Mock_io.with_timeouts_enabled (fun () ->
    let result = Mock_io.with_timeout 0.0 (Some "value") in
    Alcotest.(check (option string)) "Timeout returns None" None result
  )
```

## Success Criteria

1. **More tests run without credentials**: Target 60%+ test coverage without env vars
2. **Fast execution**: Mock tests complete in <1 second
3. **Deterministic**: Same mock test always produces same result
4. **Maintainable**: Adding new mock tests requires minimal boilerplate
5. **Complete coverage**: Can test error paths, retry logic, and edge cases

## Future Extensions

### Near-term (Immediate Value)

1. **State-based Mock**: Instead of just expectation-based, a mock that maintains fake database state:
   ```ocaml
   module Stateful_mock = struct
     let create () = ref (StringMap.empty : document StringMap.t)
     let insert db id doc = db := StringMap.add id doc !db
     let get db id = StringMap.find_opt id !db
   end
   ```
   Useful for testing complex multi-operation scenarios.

2. **Header Verification Tests**: Tests that verify the correct headers are sent:
   ```ocaml
   let verify_partition_key_header () =
     let http = Mock_http.create () in
     Mock_http.expect http ~method:`Get ~path:"/dbs/db/colls/coll/docs/doc1"
       ~expected_headers:[("x-ms-documentdb-partitionkey", "[\"value\"]")]
       ~response:(Ok (document_response ())) ();
     let _ = Mock_db.Collection.Document.get ~partition_key:"value" "db" "coll" "doc1" in
     Mock_http.verify http
   ```

### Long-term (Advanced Scenarios)

3. **Record/Replay**: Capture real Cosmos responses during integration tests, save to files, replay in mock tests. This bridges the gap between mock and real data.

4. **Property-based Testing**: Use QCheck to generate random:
   - Batch operations (verify validation)
   - Document IDs and content (verify URL encoding)
   - Error responses (verify error handling)

5. **Failure Injection**: Controlled fault injection for testing resilience:
   ```ocaml
   Mock_http.set_failure_rate http 0.1; (* 10% of requests fail *)
   Mock_http.set_latency http 0.01;    (* 10ms simulated latency *)
   ```

## Summary of Improvements in This Plan

| Aspect | Original Plan | Improved Plan |
|--------|---------------|---------------|
| **Utilities Integration** | Not mentioned | Explicitly leverages your recent `Utilities.apply_to_header_if_some` refactoring |
| **Test Infrastructure** | Generic functor mention | Specific integration with `Test_io_intf.IO` existing pattern |
| **Test Layers** | 2 layers (unit + mock) | 3 layers (unit + mock + integration) with clear separation |
| **Code Reuse** | Suggested new tests | Emphasizes reusing existing test functors with mock IO |
| **Timeout Testing** | Static mock | Configurable timeout simulation via `with_timeouts_enabled` |
| **What NOT to Mock** | Not mentioned | Clear guidance on avoiding mock overuse |
| **Build Commands** | Not mentioned | Includes specific `dune` commands per project rules |
| **Error Scenarios** | Mentioned | Concrete examples (429 retry, create-if-not-exists) |

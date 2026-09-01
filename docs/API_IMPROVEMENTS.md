# Azure Cosmos DB OCaml SDK - API Improvement Suggestions

This document outlines suggested improvements to the Azure Cosmos DB OCaml SDK API, based on comparison with modern Cosmos DB SDKs (.NET, Python, Java, Node.js) and functional programming best practices.

## Feature Coverage Analysis

### Current Implementation vs Official Azure Cosmos API

Based on comprehensive analysis of the codebase against official Azure Cosmos DB REST API documentation, the current implementation provides approximately **65%** of the full API functionality.

#### ✅ **Currently Implemented Features (100% Coverage)**

| Resource | Operations | Status |
|----------|------------|--------|
| **Databases** | List, Create, Get, Delete, Create if not exists | ✅ Complete |
| **Collections** | List, Create, Get, Delete, Create if not exists | ✅ Complete |
| **Users** | List, Create, Get, Replace, Delete | ✅ Complete |
| **Permissions** | List, Create, Get, Replace, Delete (with `?expiry_seconds`) | ✅ Complete — tokens are usable via `Database_as` |
| **Transactional Batch** | Create, Upsert, Read, Replace, Delete, Patch within a partition | ✅ Complete |
| **Offers (throughput)** | List, Get, Query, Replace, get/set throughput (manual + autoscale) | ✅ Complete |
| **Authentication** | Master key, resource token, resource token provider | ✅ Complete (no Entra ID) |

#### ✅ **Documents (98% Coverage)**

| Operation | Implemented | Missing |
|------------|-------------|---------|
| Create (with upsert, indexing) | ✅ | |
| Create Multiple (parallel, chunked) | ✅ | |
| List (with pagination, consistency) | ✅ | |
| Get (with conditional requests) | ✅ | |
| Replace (with conditional updates) | ✅ | |
| Delete (with retry logic) | ✅ | |
| Delete Multiple | ✅ | |
| Query (SQL, cross-partition) | ✅ | |
| Patch (add/set/replace/remove/incr) | ✅ via `Batch` | Standalone `Document.patch` endpoint |

#### ❌ **Major Missing Features (0% Coverage)**

| Resource | Missing Operations | Impact |
|----------|-------------------|--------|
| **Attachments** | Create, Replace, List, Delete | No media/binary support |
| **Stored Procedures** | Create, Replace, List, Delete, **Execute** | No server-side processing |
| **User Defined Functions** | Create, Replace, List, Delete | No custom query functions |
| **Triggers** | Create, Replace, List, Delete | No pre/post processing |
| **Change Feed** | First-class pull model, `pkranges`, checkpointing | Only raw `?a_im` on `Document.list`; `304` surfaces as an error (plan: [`CHANGE_FEED_PLAN.md`](CHANGE_FEED_PLAN.md)) |
| **TTL Management** | All operations | No automatic expiration |
| **Vector Search** | All operations | No AI/ML features |
| **Entra ID (AAD) auth** | `type=aad` bearer tokens with RBAC | Requires async token acquisition |

#### 📊 **Implementation Coverage by Category**

```
Core CRUD Operations:  ████████████████████ 100%
Document Operations:   ████████████████████ 98%
Transactional Batch:   ████████████████████ 100%
Throughput Mgmt:       ████████████████████ 100%
Authentication:        ████████████████░░░░ 80%  (master key + resource token; no Entra ID)
Server-side Logic:     ░░░░░░░░░░░░░░░░░░░░ 0%
Advanced Features:     ░░░░░░░░░░░░░░░░░░░░ 0%
Overall Coverage:      █████████████████░░░ 65%
```

### Critical Gaps Analysis

#### **1. Server-Side Programming (High Impact)**
- **Missing**: Stored procedures, UDFs, triggers
- **Impact**: Cannot execute complex business logic server-side
- **Use Case**: Data validation, complex calculations, multi-document transactions

#### **2. Throughput Management — ✅ Closed**
- **Implemented**: `Offer` module (list/get/query/replace, `get_throughput` / `set_throughput`,
  manual and autoscale, autoscale↔manual migration) plus `?offer_throughput` on
  `Collection.create` / `create_if_not_exists`
- **Remaining**: no offer support for shared-throughput databases created by this SDK
  (`Database.create` cannot provision database-level throughput yet)

#### **3. Real-Time Features (Medium Impact)**
- **Missing**: First-class change feed, conflict resolution
- **Current**: `Document.list ?a_im ?if_none_match` only; `304 Not Modified` (the "no changes"
  answer) is returned as `Error (Azure_error (304, _))`, and there is no `pkranges` support,
  start-position variant or checkpoint loop
- **Impact**: No practical real-time data synchronization capabilities
- **Use Case**: Event-driven architectures, CDC pipelines
- **Plan**: [`CHANGE_FEED_PLAN.md`](CHANGE_FEED_PLAN.md) — see improvement 8 below

#### **4. Media Handling (Low Impact)**
- **Missing**: Attachments
- **Impact**: Cannot work with binary/media files
- **Use Case**: Document storage with images, videos

#### **5. Authentication — ✅ Resource Tokens Closed**
- **Implemented**: `Databases_intf.Credential.t` (`Master_key` | `Resource_token` |
  `Resource_token_provider`), `Databases_intf.Credentials`, `Databases_core.Auth_credential`,
  `Make_account` / `Make_credential`, and `Database_as (C : Credentials)` plus
  `credentials_of_token` / `credentials_of_token_provider` in both backends.
  `Permission.create` / `get` / `replace` accept `?expiry_seconds`
  (`x-ms-documentdb-expiry-seconds`, 1..18000).
- **Remaining**: Entra ID (`type=aad`) bearer tokens with RBAC, which additionally require async
  token acquisition. Master-key-only operations (`list_databases`, `User.*`, `Permission.*`,
  `Offer.*`) still fail with 401/403 under a resource token — documented, not enforced by types.
- **Plan (delivered)**: [`RESOURCE_TOKEN_AUTH_PLAN.md`](RESOURCE_TOKEN_AUTH_PLAN.md) — see
  improvement 13 below

### Feature Implementation Priority

#### **Phase 1: Core Functionality Completion**
1. ✅ **Transactional Batch** - Implemented (`Batch`, `Batch_builder`)
2. ✅ **Offers Management** - Implemented (`Offer`, `Offer.Throughput`)
3. ✅ **Resource Token Authentication** - Implemented (`Database_as`, `Credential.t`)
4. **Standalone Document Patch** - Patch outside a batch (`PATCH /docs/{id}`)
5. **Stored Procedure Execution** - Enable server-side logic

#### **Phase 2: Advanced Features**
6. **Change Feed** - Real-time capabilities ([`CHANGE_FEED_PLAN.md`](CHANGE_FEED_PLAN.md))
7. **UDFs & Triggers** - Complete server-side programming
8. **Attachments** - Media support

#### **Phase 3: Enterprise Features**
9. **TTL Management** - Automatic expiration
10. **Vector Search** - AI/ML integration
11. **Conflict Resolution** - Multi-region writes
12. **Entra ID (AAD) Authentication** - `type=aad` bearer tokens with RBAC

## Current API Summary

The library uses a **functor-based architecture** with:
- `Cosmos.Databases_core.Make` functor parameterized by `IO`, `Http_client`, and `Auth_key`
- Dual backend support: `cosmos_lwt` and `cosmos_eio` for different async models
- Hierarchical module structure: `Database` → `Collection` → `Document` / `Batch` / `Batch_builder`,
  with account-scoped `User`, `Permission` and `Offer` modules alongside `Collection`
- Document operations: `create`, `create_multiple`, `get`, `replace`, `delete`, `delete_multiple`, `list`, `query`
- Transactional batch: `Batch.execute` (`?atomic`, `?should_validate`) with `Batch_builder` for fluent construction
- Throughput: `Offer.list` / `get` / `query` / `replace` plus `get_throughput` / `set_throughput`
  and `Offer.Throughput.t = Manual | Autoscale`; collections can be created with
  `?offer_throughput`
- Authentication: master key or resource token — `Make (IO) (Http) (Auth_key)` remains the
  master-key entry point, while `Make_credential` / `Database_as (C : Credentials)` accept a
  `Credential.t`; `Make_account` exposes the `Account` seam for future schemes (e.g. Entra ID)
- Errors are a single `cosmos_error` variant: `Timeout_error`, `Connection_error`, `Azure_error`, `Batch_validation_error`
- Shared retry/throttle handling via `with_throttle_retry` in `databases_core.ml`
- Test infrastructure: functor-based mocks (`Mock_io`, `Mock_http`, `Mock_response`) allowing HTTP-free unit tests

## Comparison with Modern SDKs

| Aspect | This OCaml SDK | Modern SDKs (.NET/Python/Java) |
|--------|---------------|-------------------------------|
| Terminology | `Collection`, `Document` | `Container`, `Item` (v3+ SDKs) |
| Authentication | Master key, resource token (incl. refreshable provider) | Master key, resource token, Entra ID (`authKeyOrResourceToken`) |
| Entry point | Functor with Auth_key module | Client struct with connection pooling |
| Type safety | Raw JSON strings | Strongly typed generics |
| Query building | Raw SQL strings | LINQ/fluent query builders |
| Streaming | Pagination via continuation tokens | Iterator/stream-based results |
| Retry policy | Centralised in `with_throttle_retry`, fixed parameters | Configurable policies |
| Transactional batch | `Batch` / `Batch_builder`, raw JSON bodies | `TransactionalBatch` with typed items |
| Throughput | `Offer` module, typed `Throughput.t` | `ThroughputProperties` on database/container objects |

## Suggested Improvements

### 1. Modernize Terminology (Breaking Change)

**Rationale:** Azure SDKs v3+ standardized on `Container` instead of `Collection` and `Item` instead of `Document`. This change aligns with the [Azure Cosmos DB .NET SDK v3 migration guide](https://learn.microsoft.com/en-us/azure/cosmos-db/migrate-dotnet-v3).

**Changes:**
- `Collection` → `Container`
- `Document` → `Item`
- `create` → `insert_item`
- `create_multiple` → `bulk_insert`
- `list` → `read_items`
- `get` → `read_item`

#### **Soft Migration Strategy**

**Option 1: New Modern Module (Recommended)**
```ocaml
(* New modern API module *)
module Cosmos_v3 = struct
  module Database = struct
    module Container = struct
      type t = Cosmos.Database.Collection.t (* Alias to existing *)
      
      let insert_item = Cosmos.Database.Document.create
      let bulk_insert = Cosmos.Database.Document.create_multiple
      let read_items = Cosmos.Database.Document.list
      let read_item = Cosmos.Database.Document.get
      let replace_item = Cosmos.Database.Document.replace
      let delete_item = Cosmos.Database.Document.delete
      let query_items = Cosmos.Database.Document.query
      
      (* New modern operations *)
      let upsert_item ?indexing_directive ?partition_key ?timeout 
          dbname container_name content =
        Cosmos.Database.Document.create 
          ~is_upsert:true 
          ?indexing_directive 
          ~partition_key 
          ?timeout 
          dbname container_name content
          
      (* Patch is currently only available inside a transactional batch
         via Collection.Batch.Patch; a standalone endpoint is still missing *)
      let patch_item ~partition_key ?if_match ?timeout 
          dbname container_name doc_id patch_operations =
        failwith "TODO: Implement standalone patch operation"
    end
  end
end

(* Usage - new modern API *)
let%lwt result = Cosmos_v3.Database.Container.insert_item 
  ~partition_key:"user123" 
  "mydb" 
  "users" 
  user_json

(* Legacy API still available *)
let%lwt old_result = Cosmos.Database.Document.create 
  ~partition_key:"user123" 
  "mydb" 
  "users" 
  user_json
```

**Option 2: Module Aliases with Deprecation**
```ocaml
(* Create modern aliases while keeping legacy *)
module Container = Database.Collection
[@@ocaml.deprecated "Use Cosmos_v3.Database.Container instead"]

module Item = Database.Document  
[@@ocaml.deprecated "Use Cosmos_v3.Database.Container operations instead"]

(* Modern convenience functions *)
let insert_item = Database.Document.create
[@@ocaml.deprecated "Use Cosmos_v3.Database.Container.insert_item instead"]

let read_items = Database.Document.list
[@@ocaml.deprecated "Use Cosmos_v3.Database.Container.read_items instead"]
```

**Option 3: Gradual Migration Path**
```ocaml
(* Phase 1: Add new modern module alongside existing *)
module Cosmos_v3 = struct
  (* New modern API implementation *)
end

(* Phase 2: Mark old API as deprecated *)
module Database = struct
  module Collection = struct
    [@@@ocaml.deprecated "Use Cosmos_v3.Database.Container instead"]
    include Cosmos.Database.Collection
  end
  
  module Document = struct
    [@@@ocaml.deprecated "Use Cosmos_v3.Database.Container operations instead"]  
    include Cosmos.Database.Document
  end
end

(* Phase 3: Eventually remove deprecated modules in future major version *)
```

**Migration Benefits:**
- **Zero breaking changes** - existing code continues to work
- **Gradual adoption** - teams can migrate at their own pace
- **Clear migration path** - deprecation warnings guide users
- **Modern API available immediately** - new projects use modern terminology

**Recommended Approach:**
Start with **Option 1** (New Modern Module) to provide the modern API immediately, then add **Option 2** deprecation warnings in a minor release, and finally remove legacy API in a future major version.

---

### 2. Add Client Abstraction

**Problem:** Current functor-based auth creates a new module per connection. No connection reuse or pooling.

**Current Architecture Issues:**
- Each `Database(Auth_key)` functor instantiation creates a new module
- HTTP connections are created per-request via `Cohttp_lwt_unix.Client` and `Cohttp_eio.Client`
- No connection reuse, keep-alive, or pooling mechanisms
- High overhead for frequent operations

**Connection Reuse Analysis:**

#### **Lwt Backend Current Implementation:**
```ocaml
(* Current: New connection per request *)
let get ~headers uri =
  perform_request (fun () -> Cohttp_lwt_unix.Client.get ~headers uri)
```
- Uses `Cohttp_lwt_unix.Client` which creates new TCP connections per request
- No connection pooling or keep-alive
- Each request incurs TCP handshake + TLS negotiation overhead

#### **Eio Backend Current Implementation:**
```ocaml
(* Current: Single client shared globally *)
let client = Cohttp_eio.Client.make ~https:(Some https) net in
(* Stored in global ref, reused across requests *)
```
- Better: Single `Cohttp_eio.Client` instance reused
- But still limited: no connection pooling, no configuration options
- Global state management via refs is fragile

#### **Proposed Connection Pooling Architecture:**

```ocaml
(* Connection pool abstraction *)
module Connection_pool = struct
  type t
  type config = {
    max_connections : int;           (* Pool size *)
    connection_timeout : float;      (* Connection establishment timeout *)
    idle_timeout : float;           (* Close idle connections *)
    max_requests_per_conn : int;     (* Connection reuse limit *)
  }
  
  val create : config -> t
  val get_connection : t -> Cohttp.Client.t Lwt.t  (* or Eio *)
  val release_connection : t -> Cohttp.Client.t -> unit Lwt.t
  val close : t -> unit Lwt.t
end

(* Enhanced client with pooling *)
type client = {
  endpoint : string;
  auth : auth_info;
  connection_pool : Connection_pool.t;
  default_options : options;
}

(* Lwt-specific implementation *)
module Lwt_connection_pool : Connection_pool = struct
  type t = {
    config : config;
    available : Cohttp_lwt_unix.Client.t Queue.t;
    in_use : (Cohttp_lwt_unix.Client.t * float) Hashtbl.t;
    mutex : Lwt_mutex.t;
  }
  
  let get_connection pool =
    Lwt_mutex.with_lock pool.mutex @@ fun () ->
    match Queue.pop pool.available with
    | client -> Lwt.return client
    | exception Queue.Empty ->
        create_new_client pool.config
        
  let release_connection pool client =
    Lwt_mutex.with_lock pool.mutex @@ fun () ->
    if should_reuse client pool.config then
      Queue.push client pool.available
    else
      close_connection client
end

(* Eio-specific implementation *)
module Eio_connection_pool : Connection_pool = struct
  type t = {
    config : config;
    available : Cohttp_eio.Client.t Queue.t;
    in_use : (Cohttp_eio.Client.t * float) Hashtbl.t;
    mutex : Eio.Mutex.t;
    net : Eio.Net.t;
  }
  
  let get_connection pool =
    Eio.Mutex.with_lock pool.mutex @@ fun () ->
    match Queue.pop pool.available with
    | client -> client
    | exception Queue.Empty ->
        Cohttp_eio.Client.make ~https:(pool.https) pool.net
        
  (* Eio can leverage fiber-local storage for better performance *)
  let get_connection_fiber_local pool =
    match Eio.Fiber.get_custom_state pool with
    | Some client -> client
    | None -> 
        let client = get_connection pool in
        Eio.Fiber.set_custom_state pool client;
        client
end
```

#### **Enhanced HTTP Client Modules:**

```ocaml
(* Lwt backend with pooling *)
module Lwt_http_pooled : Http_client = struct
  let perform_request_with_pool pool f =
    Lwt.catch (fun () ->
      let%lwt client = Connection_pool.get_connection pool in
      let%lwt result = f client in
      let%lwt () = Connection_pool.release_connection pool client in
      Lwt.return (Ok result))
    | handle_connection_errors
      
  let get ~headers uri pool =
    perform_request_with_pool pool (fun client ->
      Cohttp_lwt_unix.Client.get ~client ~headers uri)
end

(* Eio backend with pooling *)
module Eio_http_pooled : Http_client = struct
  let perform_request_with_pool pool f =
    Eio.Switch.run @@ fun sw ->
    try
      let client = Connection_pool.get_connection pool in
      let result = f client sw in
      Connection_pool.release_connection pool client;
      Ok result
    with handle_connection_errors
      
  let get ~headers uri pool =
    perform_request_with_pool pool (fun client sw ->
      Cohttp_eio.Client.get ~client ~sw ~headers uri)
end
```

#### **Client Factory API:**

```ocaml
(* Unified client creation *)
module Client = struct
  type t = {
    endpoint : string;
    auth : auth_info;
    pool_config : Connection_pool.config;
    default_options : options;
  }
  
  val create_lwt_client : 
    ?pool_config:Connection_pool.config ->
    endpoint:string -> 
    ?master_key:string -> 
    ?resource_token:string ->
    unit -> 
    t Lwt.t
    
  val create_eio_client : 
    ?pool_config:Connection_pool.config ->
    endpoint:string -> 
    ?master_key:string -> 
    ?resource_token:string ->
    Eio.Net.t -> 
    t
    
  (* Usage *)
  let%lwt client = create_lwt_client 
    ~endpoint:"https://account.documents.azure.com"
    ~master_key:"key"
    ~pool_config:{ max_connections = 10; idle_timeout = 30.0 }
    ()
end
```

#### **Performance Benefits:**

| Metric | Current | With Pooling | Improvement |
|--------|---------|--------------|-------------|
| Connection Setup | Per-request | Reused | 10-100x faster |
| Memory Usage | Unbounded | Bounded | Predictable |
| Concurrent Requests | Limited | Configurable | Scalable |
| TLS Handshakes | Per-request | Once per connection | Major reduction |

#### **Implementation Challenges:**

**Lwt Backend:**
- Connection lifecycle management with `Lwt_mutex`
- Proper cleanup of idle connections
- Handling connection failures and retries
- Thread-safe pool operations

**Eio Backend:**
- Leveraging Eio's structured concurrency
- Fiber-local connection caching
- Integration with Eio's cancellation model
- Resource cleanup with `Eio.Switch`

#### **Migration Strategy:**

```ocaml
(* Backward compatibility *)
module Database_v2 (Auth : Auth_key) = struct
  (* New pooled implementation *)
  module Pooled = Cosmos.Databases_core.Make (Lwt_io_pooled) (Lwt_http_pooled) (Auth)
  
  (* Legacy compatibility *)
  let create ?pool_config () =
    match pool_config with
    | None -> (* Use old implementation *)
        Cosmos.Databases_core.Make (Lwt_io) (Lwt_http) (Auth)
    | Some config -> (* Use new pooled implementation *)
        Cosmos.Databases_core.Make (Lwt_io_pooled) (Lwt_http_pooled) (Auth)
end
```

---

### 3. Strongly Typed Documents

**Problem:** Current API uses raw JSON strings for document content.

**Current:**
```ocaml
val create : 
  partition_key:string ->
  string ->  (* JSON string *)
  (int * collection option, cosmos_error) result io
```

**Suggested:**
```ocaml
type 'a json_serializable = {
  to_json : 'a -> string;
  of_json : string -> 'a option;
}

val insert_item : 
  'a json_serializable ->
  partition_key:string ->
  'a ->
  (item_response, error) result io
```

---

### 4. Request Options Builder Pattern

**Problem:** Functions have many optional parameters creating unwieldy signatures.

**Current:**
```ocaml
val list :
  ?max_item_count:int ->
  ?continuation:string ->
  ?consistency_level:string ->
  ?session_token:string ->
  ?a_im:bool ->
  ?if_none_match:string ->
  ?partition_key_range_id:string ->
  ?timeout:float ->
  string ->
  string ->
  (int * Response_headers.t * list_result, cosmos_error) result io
```

**Suggested:**
```ocaml
module Options = struct
  type t
  
  val default : t
  val with_timeout : float -> t -> t
  val with_consistency_level : consistency_level -> t -> t
  val with_max_item_count : int -> t -> t
  val with_session_token : string -> t -> t
  val with_continuation : string -> t -> t
end

val read_items : 
  ?options:Options.t ->
  container ->
  (items_response, error) result io
```

---

### 5. Streaming Query Results

**Problem:** Current `list_result` loads all documents into memory. No lazy iteration.

**Suggested:**
```ocaml
(* Using OCaml 5.0+ Seq type or custom stream type *)
val query_stream : 
  ?options:query_options ->
  query:string ->
  container ->
  ('a, error) result Seq.t io

(* Example usage *)
let stream = query_stream ~query:"SELECT * FROM c WHERE c.status = 'active'" container in
stream
|> Seq.filter_map (function Ok item -> Some item | Error _ -> None)
|> Seq.take 100
|> List.of_seq
```

---

### 6. Configurable Retry Policies

**Status:** Partially addressed. Retry logic is now centralised in a single shared helper `with_throttle_retry ~max_retries f` instead of being duplicated per operation, but the policy itself is still hardcoded (fixed `max_retries:10`, random back-off) and not configurable by the caller.

**Current (centralised but fixed policy):**
```ocaml
let with_throttle_retry ~max_retries f =
  let rec retry_loop attempt () = (* 429 / connection errors, random back-off *) in
  retry_loop max_retries ()

let do_post () = Http.post ~headers:hdrs ~body:content uri in
let* retry_result = with_throttle_retry ~max_retries:10 do_post in
```

**Known issues:**
- Retry exhaustion on HTTP 429 surfaces as `Timeout_error`, losing the throttle cause and `x-ms-retry-after-ms` header (see `BATCH_REVIEW_FINDINGS.md` #3)
- `Batch.execute` does not route through `with_throttle_retry`, so batches are not retried on 429 (see `BATCH_REVIEW_FINDINGS.md` #7)

**Suggested:**
```ocaml
type retry_policy = 
  | No_retry
  | Fixed_delay of { max_retries : int; delay_ms : int }
  | Exponential_backoff of { 
      max_retries : int; 
      initial_delay_ms : int;
      max_delay_ms : int;
      jitter : bool 
    }
  | Custom of (attempt:int -> error:error -> [ `Retry of float | `Stop ])

val with_retry_policy : retry_policy -> client -> client
```

---

### 7. Type-Safe Query Builder (GADTs)

**Problem:** Raw SQL strings are error-prone. Modern SDKs provide fluent query builders.

The existing `query` type in `json_converter.atd` already serialises to the Cosmos SQL wire format:
```ocaml
type query = {
  query: string;       (* raw SQL, e.g. "SELECT * FROM c WHERE c.age > @age" *)
  parameters: parameter list;   (* [{ name="@age"; value="18" }] *)
}
```
The goal of this improvement is to build a type-safe, composable layer **on top of this**, that compiles down to the existing `query` type rather than replacing it.

---

#### **Implementation Plan**

##### **Step 1 — Expression AST**

Define a typed expression tree that covers the Cosmos SQL subset users actually need.
No GADTs are required in the first iteration — a simple polymorphic variant AST is enough and avoids the complexity overhead.

```ocaml
(* src/cosmos/query_builder.ml *)

(** Scalar values that can appear in expressions *)
type value =
  | Int    of int
  | Float  of float
  | String of string
  | Bool   of bool
  | Null

(** A field path, e.g. ["address"; "city"] → "c.address.city" *)
type field = string list

(** Comparison and logical expressions *)
type expr =
  | Eq      of field * value
  | Neq     of field * value
  | Lt      of field * value
  | Lte     of field * value
  | Gt      of field * value
  | Gte     of field * value
  | In      of field * value list
  | Is_null of field
  | Is_defined of field
  | Starts_with of field * string
  | Contains    of field * string
  | And   of expr * expr
  | Or    of expr * expr
  | Not   of expr
```

This covers the most common Cosmos SQL predicates without requiring GADT machinery.

---

##### **Step 2 — Query Record**

```ocaml
type order_dir = Asc | Desc

type t = {
  alias       : string;               (* "c" by default *)
  projection  : [`Star | `Fields of field list | `Value of field];
  where       : expr option;
  order_by    : (field * order_dir) list;
  offset      : int option;
  limit       : int option;
  distinct    : bool;
}

let default = {
  alias      = "c";
  projection = `Star;
  where      = None;
  order_by   = [];
  offset     = None;
  limit      = None;
  distinct   = false;
}
```

---

##### **Step 3 — Fluent Builder API**

```ocaml
val alias      : string -> t -> t
val select     : [`Star | `Fields of field list | `Value of field] -> t -> t
val where      : expr -> t -> t
val and_where  : expr -> t -> t   (* ANDs with existing where *)
val order_by   : field -> order_dir -> t -> t
val limit      : int -> t -> t
val offset     : int -> t -> t
val distinct   : t -> t

(* Convenience operators *)
val (=~)  : field -> value -> expr   (* Eq *)
val (<~)  : field -> value -> expr   (* Lt *)
val (>~)  : field -> value -> expr   (* Gt *)
val (&&~) : expr  -> expr  -> expr   (* And *)
val (||~) : expr  -> expr  -> expr   (* Or *)
```

Usage example:
```ocaml
open Query_builder

let q =
  default
  |> where (["age"] >~ Int 18 &&~ ["status"] =~ String "active")
  |> order_by ["created_at"] Desc
  |> limit 100
```

---

##### **Step 4 — SQL Emission**

Compile `t` down to the existing `Json_converter_t.query` type. Parameter names are auto-generated (`@p0`, `@p1`, …) to avoid collisions.

```ocaml
val to_cosmos_query : t -> Json_converter_t.query
```

```ocaml
(* Internal implementation sketch *)
let emit_value ~params v =
  let name = Printf.sprintf "@p%d" (List.length !params) in
  let str  = match v with
    | Int i    -> string_of_int i
    | Float f  -> string_of_float f
    | String s -> Printf.sprintf "%S" s
    | Bool b   -> string_of_bool b
    | Null     -> "null"
  in
  params := { Json_converter_t.name; value = str } :: !params;
  name

let rec emit_expr ~alias ~params = function
  | Eq  (f, v) -> Printf.sprintf "%s = %s"  (emit_field alias f) (emit_value ~params v)
  | Lt  (f, v) -> Printf.sprintf "%s < %s"  (emit_field alias f) (emit_value ~params v)
  | In  (f, vs) ->
      let ps = List.map (emit_value ~params) vs in
      Printf.sprintf "%s IN (%s)" (emit_field alias f) (String.concat ", " ps)
  | And (a, b) ->
      Printf.sprintf "(%s AND %s)" (emit_expr ~alias ~params a) (emit_expr ~alias ~params b)
  (* … *)

let to_cosmos_query q =
  let params = ref [] in
  let where_clause = Option.map (fun e ->
    "WHERE " ^ emit_expr ~alias:q.alias ~params e) q.where
  in
  let sql = String.concat " " (List.filter_map Fun.id [
    Some (if q.distinct then "SELECT DISTINCT" else "SELECT");
    Some (emit_projection q.alias q.projection);
    Some ("FROM c");   (* Cosmos always uses the container alias *)
    where_clause;
    emit_order_by q;
    emit_offset_limit q;
  ]) in
  { Json_converter_t.query = sql; parameters = List.rev !params }
```

---

##### **Step 5 — Integration Point**

The builder slots in without touching `databases_core.ml`. The existing `Document.query` function already accepts `Json_converter_t.query`:

```ocaml
(* Before — raw SQL *)
let q : Json_converter_t.query = {
  query = "SELECT * FROM c WHERE c.age > @age AND c.status = @status";
  parameters = [{ name = "@age"; value = "18" };
                { name = "@status"; value = "\"active\"" }];
}

(* After — type-safe builder *)
let q =
  Query_builder.(
    default
    |> where (["age"] >~ Int 18 &&~ ["status"] =~ String "active")
    |> limit 100
    |> to_cosmos_query)

(* Both call the same underlying function *)
let%lwt result = Document.query ~partition_key "mydb" "users" q
```

---

##### **Step 6 — New Files Required**

| File | Purpose |
|------|---------|
| `src/cosmos/query_builder.ml` | AST, builder functions, SQL emitter |
| `src/cosmos/query_builder.mli` | Public interface (hides `emit_*` internals) |
| `test/core/query_builder_tests.ml` | Unit tests: AST → SQL string round-trips |

---

##### **Implementation Order**

1. `query_builder.ml` — `expr` type + `emit_expr` → SQL string (no builder functions yet; validates the core idea with unit tests)
2. `query_builder.mli` — lock down public surface
3. Add fluent builder functions and convenience operators
4. `query_builder_tests.ml` — property-based tests asserting no SQL injection via string values
5. Wire into the eio/lwt `.mli` files to expose `Query_builder.t` alongside raw `query`

##### **Explicitly Out of Scope (first iteration)**

- `JOIN` clauses (Cosmos SQL supports self-joins only; add later)
- `GROUP BY` / `HAVING` (not in Cosmos SQL)
- Sub-queries
- `ARRAY_CONTAINS` and other array functions (add as specific `expr` constructors in a follow-up)

---

### 8. Change Feed Processor

**Problem:** Current `a_im` parameter provides basic change feed access. Modern SDKs have robust processors.

**Status:** Planned in detail — see [`CHANGE_FEED_PLAN.md`](CHANGE_FEED_PLAN.md), which specifies a
pull-model `Change_feed` module (closed start-position variant, `etag` checkpoints, `304` as
`Ok`, `pkranges` enumeration, bounded polling helper). The lease-based push processor sketched
below remains a later step.

**Suggested (push model, later):**
```ocaml
type change = {
  id : string;
  partition_key : string;
  operation : [ `Insert | `Replace | `Delete ];
  previous : item option;
  current : item option;
}

type change_feed_start =
  | Beginning
  | Now
  | From_time of float
  | From_continuation of string

type subscription

val subscribe_to_changes : 
  ?start_from:change_feed_start ->
  ?lease_container:container ->  (* for multiple consumers *)
  on_change:(change -> unit io) -> 
  container -> 
  subscription io

val cancel_subscription : subscription -> unit io
```

---

### 9. Unified Response Type

**Problem:** The `(int * 'a, cosmos_error) result` pattern is verbose and inconsistent.

**Current:**
```ocaml
val get : 
  partition_key:string ->
  string ->
  string ->
  string ->
  (int * string, cosmos_error) result io  (* status code + body *)
```

**Suggested:**
```ocaml
type 'a response = {
  status : int;
  data : 'a;
  request_charge : float;
  activity_id : string;
  session_token : string option;
  etag : string option;
  diagnostics : diagnostics option;
}

type error = {
  status : int;
  code : string;
  message : string;
  request_charge : float;
  sub_status : int option;
}

val read_item : 
  partition_key:string ->
  string ->
  container ->
  (item response, error) result io
```

---

### 10. Bulk Executor with Rate Limiting

**Status:** Partially addressed. `Document.create_multiple` and `Document.delete_multiple` chunk work via `Utilities.take_first` (`?chunk_size`, default 100) and run each chunk in parallel, with per-request 429 retry through `with_throttle_retry`. Still missing: RU/s-aware throttling, per-item retry limits, and structured per-item results.

**Problem:** `create_multiple` lacks RU/s awareness and typed per-item outcomes.

**Suggested:**
```ocaml
module Bulk = struct
  type operation =
    | Insert of { id : string; item : 'a }
    | Upsert of { id : string; item : 'a }
    | Replace of { id : string; etag : string; item : 'a }
    | Delete of { id : string; etag : string option }
  
  type operation_result =
    | Success of { request_charge : float; etag : string }
    | Error of { status : int; message : string }
    | Throttled of { retry_after_ms : int }
  
  type config = {
    max_concurrency : int;           (* default: 10 *)
    target_ru_per_sec : int option; (* auto-throttle *)
    max_retries_per_item : int;     (* default: 3 *)
    continue_on_error : bool;       (* default: true *)
  }
  
  val execute : 
    ?config:config -> 
    'a json_serializable ->
    operation list -> 
    container ->
    operation_result list io
end
```

---

### 11. Transaction Support — ✅ Implemented

**Was:** No native transaction support for multi-item operations within a partition.

**Now:** `Collection.Batch` implements Cosmos transactional batch, with `Collection.Batch_builder` as the fluent construction API.

```ocaml
module Batch : sig
  type operation =
    | Create  of { if_match : string option; if_none_match : string option; body : string }
    | Upsert  of { if_match : string option; if_none_match : string option; body : string }
    | Read    of { id : string; if_match : string option; if_none_match : string option }
    | Delete  of { id : string; if_match : string option; if_none_match : string option }
    | Replace of { id : string; if_match : string option; if_none_match : string option; body : string }
    | Patch   of { id : string; if_match : string option; patch_op : patch_operation }

  and patch_operation =
    | Add         of { path : string; value : string }
    | Set         of { path : string; value : string }
    | ReplacePath of { path : string; value : string }
    | Remove      of { path : string }
    | Increment   of { path : string; value : int }

  type operation_result = {
    status_code : int;
    request_charge : float;
    etag : string option;
    resource_body : string option;
  }

  type batch_result = {
    outcomes : operation_result list;
    total_request_charge : float;
  }

  type validation_error =
    | Too_many_operations of int
    | Mixed_patch_operations
    | Empty_batch

  val validate : operation list -> (unit, validation_error) result

  val execute :
    ?timeout:float ->
    ?atomic:bool ->
    ?should_validate:bool ->
    partition_key:string ->
    string ->            (* dbname *)
    string ->            (* coll_name *)
    operation list ->
    (batch_result, cosmos_error) result io
end
```

Usage:
```ocaml
let ops =
  Batch_builder.empty
  |> Batch_builder.add_create ~body:doc_json
  |> Batch_builder.add_patch ~id:"doc2" ~patch_op:(Batch.Increment { path = "/count"; value = 1 })
  |> Batch_builder.to_operations

let%lwt result = Batch.execute ~partition_key:"user123" "mydb" "users" ops
```

Validation is performed client-side before the request (max 100 operations, non-empty, no mixing of patch and non-patch operations) and returns `Error (Batch_validation_error _)` rather than raising.

**Remaining gaps:**
- No `Transaction` monad / typed item wrapper — operations carry raw JSON strings
- `Batch.execute` is not wrapped in `with_throttle_retry` (no 429 retry)
- Non-atomic semantics rely solely on `x-ms-cosmos-batch-atomic`; `x-ms-cosmos-batch-continue-on-error` is not sent
- The module's primary type is `operation`, not the conventional `t`

---

### 12. Point Operation Optimizations

**Problem:** No specialized paths for high-throughput point operations.

**Suggested:**
```ocaml
module Point_operations = struct
  (* Optimized for single-item, low-latency access *)
  
  val read : 
    ?consistent_prefix:bool ->
    id:string ->
    partition_key:string ->
    container ->
    (item, error) result io
  
  val write : 
    ?if_match:string ->
    id:string ->
    partition_key:string ->
    item:'a ->
    container ->
    (write_result, error) result io
end
```

---

### 13. Resource Token Authentication (Connect as a User) — ✅ Implemented

**Was:** The SDK could mint resource tokens but not use them — every caller needed the all-access
master key.

**Now:** A resource token is pre-signed by the service, so the authorization header is just the
percent-encoded `_token`. `Account` was kept unchanged; a new implementation plus an injection seam
was added, so `Auth_key`, `Make`, `Auth` and `Database` all keep their signatures.

```ocaml
(* databases_intf.ml *)
module Credential = struct
  type t =
    | Master_key of string
    | Resource_token of string
    | Resource_token_provider of (unit -> string)  (* tokens expire in 1-5h *)
end

module type Credentials = sig
  val credential : Credential.t
  val endpoint : string
end

(* databases_core.ml *)
module Auth_credential (C : Databases_intf.Credentials) : Account
module Make_account (IO) (Http) (Account : Account)
module Make_credential (IO) (Http) (C : Credentials) = Make_account (IO) (Http) (Auth_credential (C))
module Make (IO) (Http) (Auth_key) = Make_account (IO) (Http) (Auth (Auth_key))

(* cosmos_lwt / cosmos_eio *)
module Database_as (C : Credentials) : S
val credentials_of_token : endpoint:string -> string -> (module Credentials)
val credentials_of_token_provider : endpoint:string -> (unit -> string) -> (module Credentials)
```

Signing lives in `Utility.authorization_token_using_resource_token`; tests cover it in
`test/core/resource_token_tests.ml` and `test/core/resource_token_integration_tests.ml`.

**Usage:**
```ocaml
module Admin = Database (MyMasterKey)

let%lwt Ok (_, perm) =
  Admin.Permission.get ~dbname ~user_name ~permission_name () in
let (module C) = credentials_of_token ~endpoint perm.token in
let module As_user = Database_as (C) in
As_user.Collection.Document.get ~partition_key dbname coll_name doc_id
```

**Related gap — closed:** `Permission.create` / `get` / `replace` now take `?expiry_seconds:int`,
sent as `x-ms-documentdb-expiry-seconds` (1..18000, default 3600) via
`Utilities.apply_to_header_if_some`.

**Caveat, documented but not enforced:** master-key-only operations (`list_databases`, `User.*`,
`Permission.*`, `Offer.*`) return 401/403 under a resource token. Making that a compile-time
restriction would require splitting the module signature and is not backward compatible.

**Bonus:** `Make_account` is also the seam needed for Entra ID (`type=aad`) auth later, though that
additionally requires async token acquisition.

**Full plan (delivered):** [`RESOURCE_TOKEN_AUTH_PLAN.md`](RESOURCE_TOKEN_AUTH_PLAN.md)

---

## Implementation Priority

### Completed
- **Transaction support** - `Collection.Batch` / `Collection.Batch_builder` (transactional batch, incl. patch operations)
- **Throughput management** - `Offer` module with manual/autoscale `Throughput.t`, migration headers, and `?offer_throughput` at collection creation
- **Mock-based test infrastructure** - `Mock_io`, `Mock_http`, `Mock_response` enable HTTP-free unit tests
- **Centralised retry helper** - `with_throttle_retry` shared across write operations
- **Resource token authentication** - `Credential.t`, `Make_credential`, `Database_as`,
  `credentials_of_token(_provider)`, and `?expiry_seconds` on `Permission`

### High Priority
1. **Client abstraction** - Essential for production use with connection pooling
2. **Strongly typed documents** - Replace raw JSON strings with typed interfaces
3. **Unified response type** - Consistent, informative response handling
4. **Streaming query results** - Critical for large dataset handling
5. **Change feed (pull model)** - Plan ready in [`CHANGE_FEED_PLAN.md`](CHANGE_FEED_PLAN.md); also
   fixes `304 Not Modified` being reported as an error

### Medium Priority
6. **Request options builder** - Cleaner API, easier to maintain
7. **Configurable retry policies** - Make the existing `with_throttle_retry` policy pluggable; fix 429 exhaustion reported as `Timeout_error`
8. **Standalone document patch** - Patch a single document outside a batch
9. **Modernize terminology** - Align with Azure SDK standards
10. **Bulk executor improvements** - RU/s-aware rate limiting and per-item results

### Low Priority
11. **Type-safe query builder** - Nice-to-have, significant implementation effort
12. **Change feed processor (lease/push model)** - Builds on the pull model, complex implementation
13. **Point operation optimizations** - Performance enhancement
14. **Entra ID (AAD) authentication** - Enabled by the `Make_account` seam from improvement 13

## Migration Path

For breaking changes (terminology updates), consider:

```ocaml
(* Deprecation cycle *)
module Collection = struct
  [@@@ocaml.deprecated "Use Container module instead"]
  include Container
end

module Document = struct
  [@@@ocaml.deprecated "Use Item module instead"]
  include Item
end
```

## References

### Azure SDK v3+ Standards

- **[Azure Cosmos DB .NET SDK v3 Migration Guide](https://learn.microsoft.com/en-us/azure/cosmos-db/migrate-dotnet-v3)** - Official Microsoft documentation detailing the terminology changes from v2 to v3 SDK, including:
  - `DocumentCollection` → `Container`
  - `Document` → `Item`
  - `CreateDocumentAsync` → `CreateItemAsync`
  - `UriFactory` → Fluent client builder pattern

- **[Azure SDK Design Guidelines](https://azure.github.io/azure-sdk/general_introduction.html)** - General design principles for Azure SDK client libraries

### Authentication

- [Access control on Cosmos DB resources](https://learn.microsoft.com/en-us/rest/api/cosmos-db/access-control-on-cosmosdb-resources) - Master key vs resource token authorization header
- [Operations on Cosmos DB Permissions](https://learn.microsoft.com/en-us/rest/api/cosmos-db/permissions) - Resource token lifetime and `x-ms-documentdb-expiry-seconds`
- [Secure access to data in Azure Cosmos DB](https://learn.microsoft.com/en-us/azure/cosmos-db/secure-access-to-data) - Resource token / token broker model

### SDK API Documentation

- [Azure Cosmos DB .NET SDK v3 API Reference](https://docs.microsoft.com/en-us/dotnet/api/microsoft.azure.cosmos)
- [Azure Cosmos DB Python SDK v4 API Reference](https://docs.microsoft.com/en-us/python/api/azure-cosmos/azure.cosmos)
- [Azure Cosmos DB Java SDK v4](https://docs.microsoft.com/en-us/java/api/com.azure.cosmos) - Also follows v3+ naming conventions

### OCaml-Specific Resources

- [OCaml 5.0 Seq Module](https://ocaml.org/api/Seq.html) - For streaming/lazy iteration
- [ATDgen Documentation](https://atd.readthedocs.io/en/latest/) - For JSON serialization handling

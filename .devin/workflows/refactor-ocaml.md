---
description: Refactor OCaml code patterns for better elegance and maintainability
auto_execution_mode: 3
---

# OCaml Refactoring Patterns

This workflow identifies common OCaml code patterns that can be refactored for better elegance, readability, and maintainability.

## Pattern 1: Repetitive Option matching with list accumulation

**Before:**
```ocaml
let optional_fields = [] in
let optional_fields =
  match opt1 with
  | Some v -> ("key1", v) :: optional_fields
  | None -> optional_fields
in
let optional_fields =
  match opt2 with
  | Some v -> ("key2", v) :: optional_fields
  | None -> optional_fields
in
...
```

**After:**
```ocaml
let add_field name opt acc =
  Option.fold opt ~none:acc ~some:(fun v -> (name, v) :: acc)
in
[]
|> add_field "key1" opt1
|> add_field "key2" opt2
...
```

## Pattern 2: Repetitive record construction in pattern matches

**Before:**
```ocaml
match x with
| A { f1; f2 } ->
    ( "type_a",
      {
        field1 = f1;
        field2 = f2;
        common1 = shared;
        common2 = shared;
      } )
| B { f1; f2 } ->
    ( "type_b",
      {
        field1 = f1;
        field2 = f2;
        common1 = shared;
        common2 = shared;
      } )
...
```

**After:**
```ocaml
let make_record ~field1 ~field2 () =
  {
    field1;
    field2;
    common1 = shared;
    common2 = shared;
  }
in
match x with
| A { f1; f2 } -> ("type_a", make_record ~field1:f1 ~field2:f2 ())
| B { f1; f2 } -> ("type_b", make_record ~field1:f1 ~field2:f2 ())
...
```

## Pattern 3: String formatting with Printf.sprintf

**Before:**
```ocaml
Printf.sprintf "{\"field\": \"%s\"}" value
```

**After:**
```ocaml
Printf.sprintf {|{"field": "%s"}|} value
```
Use verbatim strings `{|...|}` for JSON or strings containing quotes.

## Pattern 4: Top-level types with scattered operations

Types defined at the top level with functions named after the type should be lifted into a module where the type is `t` and operations live alongside it.

**Before:**
```ocaml
type some_type = {
  field1 : string;
  field2 : int;
}

let make_some_type ~field1 ~field2 = { field1; field2 }
let some_type_to_string t = ...
let update_some_type_field1 t v = { t with field1 = v }
```

**After:**
```ocaml
module Some = struct
  type t = {
    field1 : string;
    field2 : int;
  }

  let make ~field1 ~field2 = { field1; field2 }
  let to_string t = ...
  let update_field1 t v = { t with field1 = v }
end
```

Apply this when:
- A type and its related functions share a common name prefix (e.g. `some_type_*`)
- A type has more than one or two associated operations
- The type is used as an argument in multiple functions in the same file

The canonical type name inside a module is always `t`. The module name should be the concept in `UpperCamelCase` (e.g. `Response_headers`, `Batch`, `Query_builder`).

## How to Apply

1. Look for files with repetitive `match` expressions that shadow the same variable name
2. Identify functions with multiple pattern match cases that construct similar records
3. Extract helper functions with labeled arguments for clarity
4. Use `Option.fold` or `Option.map` to eliminate repetitive option handling
5. Use the pipe operator `|>` to chain operations when accumulating values
6. Look for top-level `type foo = ...` declarations with associated `foo_*` functions and lift them into a `module Foo` with `type t`

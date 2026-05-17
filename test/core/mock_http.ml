type 'a io = 'a Mock_io.t
type http_error = Connection_refused | Other_error of exn

type request = {
  method_ : [ `Get | `Post | `Put | `Delete ];
  uri : Uri.t;
  headers : Cohttp.Header.t;
  body : string option;
}

type expectation = {
  method_ : [ `Get | `Post | `Put | `Delete ];
  uri : Uri.t;
  expected_headers : (string * string) list;
  expected_body : string option;
  response : (Cohttp.Response.t * string, http_error) result;
}

type state = {
  mutable expectations : expectation Queue.t;
  mutable recorded :
    (request * (Cohttp.Response.t * string, http_error) result) list;
}

let current_mock : state option ref = ref None
let create () = { expectations = Queue.create (); recorded = [] }
let install mock = current_mock := Some mock
let uninstall () = current_mock := None

let expect exp =
  match !current_mock with
  | None -> Alcotest.fail "Mock HTTP: No mock installed, call install first"
  | Some mock -> Queue.add exp mock.expectations

let check_headers expected actual =
  List.for_all
    (fun (key, value) ->
      match Cohttp.Header.get actual key with
      | Some v -> String.equal v value
      | None -> false)
    expected

let match_method m1 m2 =
  match (m1, m2) with
  | `Get, `Get -> true
  | `Post, `Post -> true
  | `Put, `Put -> true
  | `Delete, `Delete -> true
  | _ -> false

let match_uri u1 u2 =
  let h1 = Uri.host u1 in
  let h2 = Uri.host u2 in
  let p1 = Uri.path u1 in
  let p2 = Uri.path u2 in
  Option.equal String.equal h1 h2 && String.equal p1 p2

let verify () =
  match !current_mock with
  | None -> Alcotest.fail "Mock HTTP: No mock installed, call install first"
  | Some mock ->
      if not (Queue.is_empty mock.expectations) then
        let remaining = Queue.length mock.expectations in
        Alcotest.fail
          (Printf.sprintf "Mock HTTP: %d unconsumed expectations" remaining)

let get_recorded () =
  match !current_mock with
  | None -> Alcotest.fail "Mock HTTP: No mock installed, call install first"
  | Some mock -> List.rev mock.recorded

let check_body expected actual =
  match expected with
  | None -> true
  | Some expected_body -> (
      match actual with
      | Some actual_body -> String.equal expected_body actual_body
      | None -> false)

let handle_request ?body method_ uri headers =
  match !current_mock with
  | None -> Alcotest.fail "Mock HTTP: No mock installed, call install first"
  | Some mock ->
      if Queue.is_empty mock.expectations then
        Alcotest.fail "Mock HTTP: No more expectations, unexpected request";
      let exp = Queue.pop mock.expectations in
      if not (match_method method_ exp.method_) then
        Alcotest.fail
          (Printf.sprintf "Mock HTTP: Method mismatch (expected %s)"
             (match exp.method_ with
             | `Get -> "GET"
             | `Post -> "POST"
             | `Put -> "PUT"
             | `Delete -> "DELETE"));
      if not (match_uri uri exp.uri) then
        Alcotest.fail (Printf.sprintf "Mock HTTP: URI host/path mismatch");
      if not (check_headers exp.expected_headers headers) then
        Alcotest.fail "Mock HTTP: Header mismatch";
      if not (check_body exp.expected_body body) then
        Alcotest.fail "Mock HTTP: Body mismatch";
      let req = { method_; uri; headers; body } in
      mock.recorded <- (req, exp.response) :: mock.recorded;
      exp.response

let get ~headers uri = handle_request `Get uri headers
let post ~headers ~body uri = handle_request ~body `Post uri headers
let put ~headers ~body uri = handle_request ~body `Put uri headers
let delete ~headers uri = handle_request `Delete uri headers

let with_mock mock f =
  install mock;
  Fun.protect ~finally:uninstall (fun () ->
      let result = f () in
      verify ();
      result)

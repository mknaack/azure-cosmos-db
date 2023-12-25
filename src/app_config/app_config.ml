(*

Docs:
https://learn.microsoft.com/en-us/azure/azure-app-configuration/rest-api-authentication-hmac
https://learn.microsoft.com/en-us/azure/azure-app-configuration/rest-api-key-value

*)

let ms_date () =
  let now = Unix.time () in
  Utilities.x_ms_date now

let content_hash body =
  let hash = Cryptokit.Hash.sha256 () in
  hash#add_string body;
  Base64.encode_exn hash#result

let signature secret string_to_sign =
  let key = Base64.decode_exn secret in
  let hash = Cryptokit.MAC.hmac_sha256 key in
  hash#add_string string_to_sign;
  Base64.encode_exn hash#result

let date_header date header = Cohttp.Header.add header "x-ms-date" date

let content_hash_header content header =
  let content_hash = content_hash content in
  Cohttp.Header.add header "x-ms-content-sha256" content_hash

let authorization_header credential signed_headers signature header =
  let s =
    Printf.sprintf "HMAC-SHA256 Credential=%s&SignedHeaders=%s&Signature=%s"
      credential signed_headers signature
  in
  Cohttp.Header.add header "Authorization" s

let headers date body credential signed_headers signature =
  Cohttp.Header.init () |> date_header date |> content_hash_header body
  |> authorization_header credential signed_headers signature

let signed_headers = "x-ms-date;host;x-ms-content-sha256"

let string_to_sign uri date host content_hash =
  let path_and_query = Uri.path_and_query uri in
  Printf.sprintf "GET\n%s\n%s;%s;%s" path_and_query date host content_hash

let headers host uri credential secret () =
  let now = Unix.time () in
  let now = Utilities.x_ms_date now in
  let body = "" in
  let content_hash = content_hash body in
  let string_to_sign = string_to_sign uri now host content_hash in
  let signature = signature secret string_to_sign in
  let headers = headers now "" credential signed_headers signature in
  headers

let uri host =
  Uri.make ~scheme:"https" ~host ~path:"kv"
    ~query:[ ("api-version", [ "1.0" ]) ]
    ()

let request headers uri = Cohttp_lwt_unix.Client.get ~headers uri

let call host credential secret () =
  let uri = uri host in
  let headers = headers host uri credential secret () in
  let%lwt reponse, body = request headers uri in
  let code = reponse |> Cohttp.Response.status |> Cohttp.Code.code_of_status in
  let%lwt body = Cohttp_lwt.Body.to_string body in
  Lwt.return (code, body)

let call_json host credential secret () =
  let%lwt code, body = call host credential secret () in
  let result = Json_j.kv_result_of_string body in
  Lwt.return (code, result)

module Json = Json_j

(*
Endpoint=https://tp-app-conf.azconfig.io;Id=wTzC;Secret=lwxrZtJAmHjO/QvFwyR0TS7om7rOEoLlrwjRiJfLv10=
api-version=1.0
Endpoint=https://tp-app-conf.azconfig.io;Id=/mQv;Secret=lKsGlGSd9aiY3iz1v9Q3o9BRhTchHCVtAmdOhapJtpA=

Credential = /mQv
Secret lKsGlGSd9aiY3iz1v9Q3o9BRhTchHCVtAmdOhapJtpA=

Docs:
https://learn.microsoft.com/en-us/azure/azure-app-configuration/rest-api-authentication-hmac
https://learn.microsoft.com/en-us/azure/azure-app-configuration/rest-api-key-value

Execute:
dune exec -- ./src/app_config/app_config.exe
*)

(*
type verb = Get | Post | Put | Delete

let string_of_verb = function
  | Get -> "GET"
  | Post -> "POST"
  | Put -> "PUT"
  | Delete -> "DELETE"

let weekday_of_tm_wday = function
  | 0 -> "Sun"
  | 1 -> "Mon"
  | 2 -> "Tue"
  | 3 -> "Wed"
  | 4 -> "Thu"
  | 5 -> "Fri"
  | 6 -> "Sat"
  | d -> failwith @@ Printf.sprintf "Day number unknown: %i" d

(* "Jan"  /  "Feb" /  "Mar"  /  "Apr"
               /  "May"  /  "Jun" /  "Jul"  /  "Aug"
               /  "Sep"  /  "Oct" /  "Nov"  /  "Dec"*)
let month_of_tm_mon = function
  | 0 -> "Jan"
  | 1 -> "Feb"
  | 2 -> "Mar"
  | 3 -> "Apr"
  | 4 -> "May"
  | 5 -> "Jun"
  | 6 -> "Jul"
  | 7 -> "Aug"
  | 8 -> "Sep"
  | 9 -> "Oct"
  | 10 -> "Nov"
  | 11 -> "Dec"
  | d -> failwith @@ Printf.sprintf "Month number unknown: %i" d

let string_replace old_value new_value string =
  let regexp = Str.regexp_string old_value in
  let result = Str.global_replace regexp new_value string in
  result

let x_ms_date time =
  let t = Unix.gmtime time in
  let weekday = weekday_of_tm_wday t.tm_wday in
  Printf.sprintf "%s, %02i %s %i %02i:%02i:%02i GMT" weekday t.tm_mday
    (month_of_tm_mon t.tm_mon) (t.tm_year + 1900) t.tm_hour t.tm_min t.tm_sec

let ms_date () =
  let now = Unix.time () in
  x_ms_date now

let string_to_sign verb date =
  string_of_verb verb ^ "\n" ^ "/keys&api-version=1.0" ^ "\n" ^ date
  ^ ";tp-app-conf.azconfig.io;"

(* https://learn.microsoft.com/en-us/azure/azure-app-configuration/rest-api-authentication-hmac *)
let authorization_token_using_master_key verb resource_type resource_id date
    master_key =
  let key = Base64.decode_exn master_key in
  (* let text =
       String.lowercase_ascii verb
       ^ "\n" ^ path_and_query ^ "\n" ^ resource_id ^ "\n"
       ^ String.lowercase_ascii date
       ^ "\n" ^ "" ^ "\n"
     in *)
  let string_to_sign =
    (* "GET" ^ "\n" ^ "/keys&api-version=1.0" ^ "\n" ^ date
       ^ ";tp-app-conf.azconfig.io;" *)
    string_to_sign verb date
  in
  let body = Bytes.of_string string_to_sign in
  let signature =
    let hash = Cryptokit.MAC.hmac_sha256 key in
    hash#add_substring body 0 (Bytes.length body);
    Base64.encode_exn hash#result
  in
  (* let result =
       Uri.pct_encode ~component:`Userinfo
         ("type=" ^ master_token ^ "&ver=" ^ token_version ^ "&sig=" ^ signature)
     in *)
  let signed_headers = "x-ms-date;host;x-ms-content-sha256" in
  let result =
    Uri.pct_encode ~component:`Userinfo
      ("HMAC-SHA256 Credential=" ^ master_key ^ "&SignedHeaders="
     ^ signed_headers ^ "&Signature=" ^ signature)
  in
  result

let string_to_sign date =
  let date = x_ms_date date in
  "GET" ^ "\n" ^ "/keys&api-version=1.0" ^ "\n" ^ date
  ^ ";tp-app-conf.azconfig.io;"

let signature key string_to_sign =
  let hash = Cryptokit.MAC.hmac_sha256 key in
  let string_to_sign = Bytes.of_string string_to_sign in
  hash#add_substring string_to_sign 0 (Bytes.length string_to_sign);
  Base64.encode_exn hash#result

let content_hash body =
  let hash = Cryptokit.Hash.sha256 () in
  hash#add_string body;
  Base64.encode_exn hash#result

let host_header header =
  Cohttp.Header.add header "Host" "tp-app-conf.azconfig.io"

let date_header date header = Cohttp.Header.add header "Date" (x_ms_date date)

let content_hash_header content header =
  let content_hash = content_hash content in
  Cohttp.Header.add header "x-ms-content-sha256" content_hash

(*
     { name: "Authorization", value: "HMAC-SHA256 Credential=" + credential + "&SignedHeaders=" + signedHeaders + "&Signature=" + signature }
   *)
let authorization_header credential signed_headers signature header =
  Cohttp.Header.add header "Authorization"
    ("HMAC-SHA256 Credential=" ^ credential ^ "&SignedHeaders=" ^ signed_headers
   ^ "&Signature=" ^ signature)

let headers date body credential signed_headers signature =
  (* let h = Cohttp.Header.init () in *)
  (* let h = Cohttp.Header.add h "Host" "tp-app-conf.azconfig.io" in *)
  (* let h = Cohttp.Header.add h "Date" (x_ms_date date) in *)
  (* let h = Cohttp.Header.add h "x-ms-content-sha256" content_hash in *)
  (* let h = Cohttp.Header.add h "Authorization" in *)
  (* h *)
  Cohttp.Header.init () |> host_header |> date_header date
  |> content_hash_header body
  |> authorization_header credential signed_headers signature

let uri =
  Uri.make ~scheme:"https" ~host:"tp-app-conf.azconfig.io" (*~port:443*)
    ~path:"kv"
    ~query:[ ("api-version", [ "1.0" ]) ]
    ()

let request headers =
  (* let now = Unix.time () in
     let headers = headers now "" in *)
  Cohttp_lwt_unix.Client.get ~headers uri

let credential = "/mQv"
let secret = "lKsGlGSd9aiY3iz1v9Q3o9BRhTchHCVtAmdOhapJtpA="

let headers () =
  let now = Unix.time () in
  let string_to_sign = string_to_sign now in
  let signature = signature secret string_to_sign in
  let headers = headers now "" credential string_to_sign signature in
  headers

let make_call =
  let headers = headers () in
  let () =
    Printf.printf "Uri: %s\nHeaders:\n%s\n" (Uri.to_string uri)
      (Cohttp.Header.to_string headers)
  in
  let%lwt reponse, body = request headers in
  let code = reponse |> Cohttp.Response.status |> Cohttp.Code.code_of_status in
  let%lwt body = Cohttp_lwt.Body.to_string body in
  Printf.printf "Response code: %d\n" code;
  Printf.printf "Headers: %s\n"
    (reponse |> Cohttp.Response.headers |> Cohttp.Header.to_string);
  Printf.printf "Body of length: %d\n" (String.length body);
  Printf.printf "Body %s\n" body;
  Lwt.return_unit

let () =
  let () = Lwt_main.run make_call in
  print_endline "make_call done"
*)
(* let () = print_endline test1
   let () = Uri.to_string uri |> print_endline *)

(*
function signRequest(host,
                     method,      // GET, PUT, POST, DELETE
                     url,         // path+query
                     body,        // request body (undefined of none)
                     credential,  // access key id
                     secret)      // access key value (base64 encoded)
{
        var verb = method.toUpperCase();
        var utcNow = new Date().toUTCString();
        var contentHash = CryptoJS.SHA256(body).toString(CryptoJS.enc.Base64);

        //
        // SignedHeaders
        var signedHeaders = "x-ms-date;host;x-ms-content-sha256"; // Semicolon separated header names

        //
        // String-To-Sign
        var stringToSign =
            verb + '\n' +                              // VERB
            url + '\n' +                               // path_and_query
            utcNow + ';' + host + ';' + contentHash;   // Semicolon separated SignedHeaders values

        //
        // Signature
        var signature = CryptoJS.HmacSHA256(stringToSign, CryptoJS.enc.Base64.parse(secret)).toString(CryptoJS.enc.Base64);

        //
        // Result request headers
        return [
            { name: "x-ms-date", value: utcNow },
            { name: "x-ms-content-sha256", value: contentHash },
            { name: "Authorization", value: "HMAC-SHA256 Credential=" + credential + "&SignedHeaders=" + signedHeaders + "&Signature=" + signature }
        ];
}   

Credential = /mQv
Secret lKsGlGSd9aiY3iz1v9Q3o9BRhTchHCVtAmdOhapJtpA=

signRequest('tp-app-conf.azconfig.io', 'get', 'keys', '/mQv', 'lKsGlGSd9aiY3iz1v9Q3o9BRhTchHCVtAmdOhapJtpA=')
Array (3) = $1
0 {name: "x-ms-date", value: "Mon, 04 Dec 2023 22:05:30 GMT"}
1 {name: "x-ms-content-sha256", value: "47DEQpj8HBSa+/TImW+5JCeuQeRkm5NMpJWZG3hSuFU="}
2 {name: "Authorization", value: "HMAC-SHA256 Credential=/mQv&SignedHeaders=x-ms-date;host;x-ms-content-sha256&Signature=4yWmLngKBU6VoJhciKneZorasHWwOjBxzf6mgsxrLHM="}

Array Prototype

*)
(* let weekday_of_tm_wday = function
     | 0 -> "Sun"
     | 1 -> "Mon"
     | 2 -> "Tue"
     | 3 -> "Wed"
     | 4 -> "Thu"
     | 5 -> "Fri"
     | 6 -> "Sat"
     | d -> failwith @@ Printf.sprintf "Day number unknown: %i" d

   (* "Jan"  /  "Feb" /  "Mar"  /  "Apr"
                  /  "May"  /  "Jun" /  "Jul"  /  "Aug"
                  /  "Sep"  /  "Oct" /  "Nov"  /  "Dec"*)
   let month_of_tm_mon = function
     | 0 -> "Jan"
     | 1 -> "Feb"
     | 2 -> "Mar"
     | 3 -> "Apr"
     | 4 -> "May"
     | 5 -> "Jun"
     | 6 -> "Jul"
     | 7 -> "Aug"
     | 8 -> "Sep"
     | 9 -> "Oct"
     | 10 -> "Nov"
     | 11 -> "Dec"
     | d -> failwith @@ Printf.sprintf "Month number unknown: %i" d

   let x_ms_date time =
     let t = Unix.gmtime time in
     let weekday = weekday_of_tm_wday t.tm_wday in
     Printf.sprintf "%s, %02i %s %i %02i:%02i:%02i GMT" weekday t.tm_mday
       (month_of_tm_mon t.tm_mon) (t.tm_year + 1900) t.tm_hour t.tm_min t.tm_sec *)

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

(* let sign_request host method_ url body credential secret =
   let verb = String.uppercase_ascii method_ in
   (* let utc_now = ms_date () in *)
   let utc_now = "Sat, 09 Dec 2023 21:25:08 GMT" in

   let content_hash = content_hash body in
   let signed_headers = "x-ms-date;host;x-ms-content-sha256" in
   let string_to_sign =
     Printf.sprintf "%s\n%s\n%s;%s;%s" verb url utc_now host content_hash
   in
   let signature = signature secret string_to_sign in
   Printf.sprintf
     "%s x-ms-date: %s\n\
      x-ms-content-sha256: %s Authorization: HMAC-SHA256 \
      Credential=%s&SignedHeaders=%s&Signature=%s"
     verb utc_now content_hash credential signed_headers signature *)

(* let host = "tp-app-conf.azconfig.io"
   let method_ = "GET"
   let url = "/kv?api-version=1.0"*)
let body = ""
let credential = "/mQv"
let secret = "lKsGlGSd9aiY3iz1v9Q3o9BRhTchHCVtAmdOhapJtpA="
(* let headers = sign_request host method_ url body credential secret *)

(* let headers=$(sign_request "$host" "$method" "$url" "$body" "$credential" "$secret") *)
(* let () =
   (* print_endline host;
      print_endline method_;
      print_endline url;
      print_endline body;
      print_endline credential;
      print_endline secret;
      print_endline (content_hash body); *)
   print_endline headers *)

(* let host_header header =
   Cohttp.Header.add header "Host" "tp-app-conf.azconfig.io" *)

(* let date_header date header = Cohttp.Header.add header "Date" (x_ms_date date) *)
let date_header date header = Cohttp.Header.add header "x-ms-date" date

let content_hash_header content header =
  let content_hash = content_hash content in
  Cohttp.Header.add header "x-ms-content-sha256" content_hash

(*
       { name: "Authorization", value: "HMAC-SHA256 Credential=" + credential + "&SignedHeaders=" + signedHeaders + "&Signature=" + signature }
     *)
let authorization_header credential signed_headers signature header =
  Cohttp.Header.add header "Authorization"
    ("HMAC-SHA256 Credential=" ^ credential ^ "&SignedHeaders=" ^ signed_headers
   ^ "&Signature=" ^ signature)

let headers date body credential signed_headers signature =
  (* let h = Cohttp.Header.init () in *)
  (* let h = Cohttp.Header.add h "Host" "tp-app-conf.azconfig.io" in *)
  (* let h = Cohttp.Header.add h "Date" (x_ms_date date) in *)
  (* let h = Cohttp.Header.add h "x-ms-content-sha256" content_hash in *)
  (* let h = Cohttp.Header.add h "Authorization" in *)
  (* h *)
  (* let () = print_endline signed_headers in *)
  (* let () = print_endline signature in *)
  Cohttp.Header.init ()
  (* |> host_header *)
  |> date_header date
  |> content_hash_header body
  |> authorization_header credential signed_headers signature

(* type verb = Get | Post | Put | Delete

   let string_of_verb = function
     | Get -> "GET"
     | Post -> "POST"
     | Put -> "PUT"
     | Delete -> "DELETE" *)

(* let string_to_sign verb date =
   string_of_verb verb ^ "\n" ^ "/v&api-version=1.0" ^ "\n" ^ date
   ^ ";tp-app-conf.azconfig.io;" *)
(*
  let string_to_sign =
    Printf.sprintf "%s\n%s\n%s;%s;%s" verb url utc_now host content_hash
  in
*)
let signed_headers = "x-ms-date;host;x-ms-content-sha256"

let string_to_sign date content_hash =
  (* let date = x_ms_date date in *)
  "GET" ^ "\n" ^ "/kv?api-version=1.0" ^ "\n" ^ date
  ^ ";tp-app-conf.azconfig.io;" ^ content_hash

let headers () =
  let now = Unix.time () in
  let now = Utilities.x_ms_date now in
  (* let now = "Sat, 09 Dec 2023 21:25:08 GMT" in *)
  let body = "" in
  let content_hash = content_hash body in
  let string_to_sign = string_to_sign now content_hash in
  (* let () = print_endline string_to_sign in *)
  let signature = signature secret string_to_sign in
  let headers = headers now "" credential signed_headers signature in
  headers

let uri =
  Uri.make ~scheme:"https" ~host:"tp-app-conf.azconfig.io" ~path:"kv"
    ~query:[ ("api-version", [ "1.0" ]) ]
    ()

let request headers =
  (* let now = Unix.time () in
     let headers = headers now "" in *)
  Cohttp_lwt_unix.Client.get ~headers uri

let make_call () =
  let headers = headers () in
  let () =
    Printf.printf "Uri: %s\nHeaders:\n%s\n" (Uri.to_string uri)
      (Cohttp.Header.to_string headers)
  in
  let%lwt reponse, body = request headers in
  let code = reponse |> Cohttp.Response.status |> Cohttp.Code.code_of_status in
  let%lwt body = Cohttp_lwt.Body.to_string body in
  Lwt.return (code, body)
(* Printf.printf "Response code: %d\n" code;
   Printf.printf "Headers: %s\n"
     (reponse |> Cohttp.Response.headers |> Cohttp.Header.to_string);
   Printf.printf "Body of length: %d\n" (String.length body);
   Printf.printf "Body %s\n" body;
   Lwt.return_unit *)

(* let () =
   let () = Lwt_main.run make_call in
   print_endline "make_call done" *)

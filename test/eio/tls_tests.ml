(* Handshakes against a local TLS server whose certificate is issued by a
   test CA, checking that [Databases.https] verifies the server name. *)

module DN = X509.Distinguished_name

let dn name =
  [ DN.Relative_distinguished_name.singleton (DN.CN (DN.Common_name.v name)) ]

let now () = Option.get (Ptime.of_float_s (Unix.gettimeofday ()))

let validity () =
  let now = Unix.gettimeofday () in
  let at s = Option.get (Ptime.of_float_s s) in
  (at (now -. 3600.), at (now +. 86400.))

let sign_cert ~subject ~extensions ~issuer_key ~issuer key =
  let valid_from, valid_until = validity () in
  let csr = Result.get_ok (X509.Signing_request.create subject key) in
  Result.get_ok
    (X509.Signing_request.sign csr ~valid_from ~valid_until ~extensions
       issuer_key issuer)

let ca =
  lazy
    (let key = X509.Private_key.generate `P256 in
     let subject = dn "Cosmos test CA" in
     let extensions =
       X509.Extension.(
         singleton Basic_constraints (true, (true, None))
         |> add Key_usage (true, [ `Key_cert_sign ]))
     in
     (key, sign_cert ~subject ~extensions ~issuer_key:key ~issuer:subject key))

let server_cert names =
  let ca_key, ca_cert = Lazy.force ca in
  let key = X509.Private_key.generate `P256 in
  let general_names =
    List.fold_left
      (fun acc -> function
        | `Dns name ->
            let existing =
              Option.value ~default:[] (X509.General_name.find DNS acc)
            in
            X509.General_name.add DNS (name :: existing) acc
        | `Ip ip ->
            let existing =
              Option.value ~default:[] (X509.General_name.find IP acc)
            in
            X509.General_name.add IP
              (Ipaddr.to_octets (Ipaddr.of_string_exn ip) :: existing)
              acc)
      X509.General_name.empty names
  in
  let extensions =
    X509.Extension.singleton Subject_alt_name (false, general_names)
  in
  let cert =
    sign_cert ~subject:(dn "Cosmos test server") ~extensions ~issuer_key:ca_key
      ~issuer:(X509.Certificate.subject ca_cert)
      key
  in
  ([ cert ], key)

let authenticator () =
  let _, ca_cert = Lazy.force ca in
  X509.Authenticator.chain_of_trust ~time:(fun () -> Some (now ())) [ ca_cert ]

(* Runs [Databases.https] for [uri] against a server presenting a
   certificate for [names]. *)
let handshake net ~names ~uri =
  let server_config =
    Result.get_ok
      (Tls.Config.server ~certificates:(`Single (server_cert names)) ())
  in
  Eio.Switch.run @@ fun sw ->
  let listener =
    Eio.Net.listen ~sw ~backlog:1 ~reuse_addr:true net
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
  in
  Eio.Fiber.fork ~sw (fun () ->
      Eio.Switch.run @@ fun sw ->
      let flow, _ = Eio.Net.accept ~sw listener in
      try ignore (Tls_eio.server_of_flow server_config flow) with _ -> ());
  Eio.Switch.run @@ fun sw ->
  let socket = Eio.Net.connect ~sw net (Eio.Net.listening_addr listener) in
  match
    Cosmos_eio.Databases.https ~authenticator:(authenticator ()) uri socket
  with
  | _ -> Ok ()
  | exception exn -> Error exn

let uri host = Uri.make ~scheme:"https" ~host ~port:443 ()

let expect_ok = function
  | Ok () -> ()
  | Error exn -> Alcotest.failf "handshake failed: %s" (Printexc.to_string exn)

let expect_auth_failure = function
  | Ok () ->
      Alcotest.fail "handshake succeeded with a certificate for another name"
  | Error (Tls_eio.Tls_failure (`Error (`AuthenticationFailure _))) -> ()
  | Error exn -> Alcotest.failf "unexpected error: %s" (Printexc.to_string exn)

let expect_invalid_argument = function
  | Error (Invalid_argument _) -> ()
  | Ok () -> Alcotest.fail "handshake succeeded without a valid host"
  | Error exn -> Alcotest.failf "unexpected error: %s" (Printexc.to_string exn)

let tests net =
  Mirage_crypto_rng_unix.use_default ();
  [
    ( "accepts certificate for the URI host",
      fun () ->
        handshake net ~names:[ `Dns "localhost" ] ~uri:(uri "localhost")
        |> expect_ok );
    ( "rejects certificate for another host",
      fun () ->
        handshake net ~names:[ `Dns "attacker.example" ] ~uri:(uri "localhost")
        |> expect_auth_failure );
    ( "accepts certificate for the URI IP address",
      fun () ->
        handshake net ~names:[ `Ip "127.0.0.1" ] ~uri:(uri "127.0.0.1")
        |> expect_ok );
    ( "rejects certificate without the URI IP address",
      fun () ->
        handshake net ~names:[ `Dns "localhost" ] ~uri:(uri "127.0.0.1")
        |> expect_auth_failure );
    ( "rejects URI without host",
      fun () ->
        handshake net ~names:[ `Dns "localhost" ] ~uri:(Uri.of_string "/path")
        |> expect_invalid_argument );
    ( "rejects URI with invalid host name",
      fun () ->
        handshake net ~names:[ `Dns "localhost" ] ~uri:(uri "not_a_host")
        |> expect_invalid_argument );
  ]

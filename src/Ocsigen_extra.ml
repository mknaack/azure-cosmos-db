open Lwt

let delete ?v6 ?https ?port ?headers ~host ~uri () =
  Ocsigen_lib.Ip_address.get_inet_addr ?v6 host >>= fun inet_addr ->
  Ocsigen_http_client.raw_request
    ?https
    ?port
    ?headers
    ~http_method:Ocsigen_http_frame.Http_header.DELETE
    ~content:None
    ~host:(match port with None -> host | Some p -> host^":"^string_of_int p)
    ~inet_addr
    ~uri
    ()
    ()

let put_string ?v6 ?https ?port ?(headers = Http_headers.empty) ~host ~uri ~content ~content_type () =
  Ocsigen_lib.Ip_address.get_inet_addr ?v6 host >>= fun inet_addr ->
  let content_type = String.concat "/" [fst content_type; snd content_type] in
  Ocsigen_http_client.raw_request
    ?https
    ?port
    ~http_method:Ocsigen_http_frame.Http_header.PUT
    ~content:(Some (Ocsigen_stream.of_string content))
    ~content_length:(Int64.of_int (String.length content))
    ~headers:(Http_headers.add Http_headers.content_type content_type headers)
    ~host:(match port with None -> host | Some p -> host^":"^string_of_int p)
    ~inet_addr
    ~uri
    ()
    ()

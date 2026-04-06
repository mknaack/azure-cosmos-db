let string_replace old_value new_value s =
  Str.global_replace (Str.regexp_string old_value) new_value s

let adjust_host endpoint =
  let azure_end = ".documents.azure.com" in
  if String.length endpoint > String.length azure_end then
    let start =
      String.sub endpoint
        (String.length endpoint - String.length azure_end)
        (String.length azure_end)
    in
    if String.equal start azure_end then endpoint else endpoint ^ azure_end
  else endpoint ^ azure_end

let authorization_token_using_master_key verb resource_type resource_id date
    master_key =
  let key = Base64.decode_exn master_key in
  let text =
    String.lowercase_ascii verb
    ^ "\n"
    ^ String.lowercase_ascii resource_type
    ^ "\n" ^ resource_id ^ "\n"
    ^ String.lowercase_ascii date
    ^ "\n" ^ "" ^ "\n"
  in
  let body = Bytes.of_string text in
  let signature =
    let hash = Cryptokit.MAC.hmac_sha256 key in
    hash#add_substring body 0 (Bytes.length body);
    Base64.encode_exn hash#result
  in
  let master_token = "master" in
  let token_version = "1.0" in
  let result =
    Uri.pct_encode ~component:`Userinfo
      ("type=" ^ master_token ^ "&ver=" ^ token_version ^ "&sig=" ^ signature)
  in
  result |> string_replace "%3D" "%3d" |> string_replace "%2B" "%2b"
  |> string_replace "%2F" "%2f"

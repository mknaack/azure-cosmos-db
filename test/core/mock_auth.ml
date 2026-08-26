module Auth : Cosmos.Databases_intf.Auth_key = struct
  let master_key = "dGhlcXVpY2ticm93bmZveGp1bXBzb3ZlcnRoZWxhenlkb2c="
  let endpoint = "mock-account.documents.azure.com"
end

let endpoint = "mock-account.documents.azure.com"

let sample_resource_token =
  "type=resource&ver=1&sig=m32/kQcHRQ4dSIiRZUFPRA==;token-body=="

module Resource_token_auth : Cosmos.Databases_intf.Credentials = struct
  let credential =
    Cosmos.Databases_intf.Credential.Resource_token sample_resource_token

  let endpoint = endpoint
end

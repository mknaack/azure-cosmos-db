[![Coverage Status](https://coveralls.io/repos/github/mknaack/azure-cosmos-db/badge.svg?branch=actions)](https://coveralls.io/github/mknaack/azure-cosmos-db?branch=actions)

Azure cosmos db
===============

Provides an interface to Microsoft Azure Cosmos db.

# Documentation

[Microsoft documentation](https://docs.microsoft.com/en-us/rest/api/cosmos-db/)

[API documentation](https://knaack.bitbucket.io/)

# Quick start

```ocaml
module MyAuthKeys : Auth_key = struct
  let master_key = "key guid found in azure portal"
  let endpoint = "endpoint found in azure portal (e.g. endpoint.documents.azure.com)"
end

module D = Database (MyAuthKeys)
D.list_databases ()
```
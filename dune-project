(lang dune 3.21)

(generate_opam_files true)

(license MIT)

(maintainers "Morten Knaack <knaack@gmail>")

(authors "Morten Knaack <knaack@gmail>")

(source
 (github mknaack/azure-cosmos-db))

(package
 (name azure-cosmos-db)
 (synopsis "Azure cosmos db interface")
 (description "Provides an interface to the Azure Cosmos db.")
 (depends
  (ocaml
   (>= 4.08.0))
  (atdgen
   (>= 2.0.0))
  (base64
   (>= 3.5.0))
  uri
  cohttp
  cryptokit
  (bisect_ppx :with-test)
  (odoc :with-doc))
 (conflicts
  (result
   (< 1.5))))

(package
 (name azure-cosmos-db-lwt)
 (synopsis "Lwt backend for azure-cosmos-db")
 (description
  "Provides an Lwt-based backend for the Azure Cosmos DB interface.")
 (depends
  (ocaml
   (>= 4.08.0))
  azure-cosmos-db
  lwt
  (lwt_ppx
   (>= 2.0.2))
  lwt_ssl
  ssl
  cohttp-lwt-unix
  (alcotest-lwt :with-test)
  (bisect_ppx :with-test)
  (odoc :with-doc))
 (conflicts
  (result
   (< 1.5))))

(package
 (name azure-cosmos-db-eio)
 (synopsis "Eio backend for azure-cosmos-db")
 (description
  "Provides an Eio-based backend for the Azure Cosmos DB interface.")
 (depends
  (ocaml
   (>= 5.0.0))
  azure-cosmos-db
  (eio
   (>= 1.0))
  eio_main
  (cohttp-eio
   (>= 6.0.0))
  tls-eio
  ca-certs
  (mirage-crypto-rng
   (>= 1.0.0))
  (alcotest :with-test)
  (bisect_ppx :with-test)
  (odoc :with-doc))
 (conflicts
  (result
   (< 1.5))))

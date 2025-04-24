(lang dune 2.9)

(generate_opam_files)

(source
 (bitbucket knaack/azure-cosmos-db))

(authors "Morten Knaack")

(maintainers "Morten Knaack")

(license "MIT")

(package
 (name azure-cosmos-db)
 (synopsis "Azure cosmos db interface")
 (description "Provides an interface to the Azure Cosmos db.")
 (depends
  (ocaml
   (>= "4.08.0"))
  lwt
  (lwt_ppx
   (>= "2.0.2"))
  lwt_ssl
  ssl
  (atdgen
   (>= "2.0.0"))
  (base64
   (>= "3.5.0"))
  uri
  cohttp
  cohttp-lwt-unix
  cryptokit
  (alcotest-lwt :with-test)
  (bisect_ppx :with-test)
  (ocamlformat :with-dev))
 (conflicts
  (result
   (< "1.5"))))

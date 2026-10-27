(lang dune 3.10)

(name finances)

(version 0.1.0)

(generate_opam_files true)

(source
 (github vbmithr/finances))

(authors "Vincent Bernardoff")

(maintainers "Vincent Bernardoff")

(license ISC)

(package
 (name finances)
 (synopsis "Personal accounting and portfolio tracking")
 (depends
  (ocaml
   (>= 5.1))
  rel
  conf-sqlite3
  base
  stdio
  bos
  fpath
  ptime
  (decimal
   (>= 1.0.2))
  ppx_sexp_conv
  ppx_compare
  ppx_let
  (cmdliner
   (>= 1.3.0))
  yojson
  xmlm
  re
  digestif
  (csv
   (>= 2.4))
  hledger
  iso4217
  (alcotest :with-test)
  (zarith :with-test)))

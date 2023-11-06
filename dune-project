(lang dune 3.11)

(name ml_network_modeling)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name ml_network_modeling)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends
  ocaml
  dune
  (yojson (>= 2.1.1))
  (ppx_yojson_conv (>= v0.16.0))
  (ppx_deriving (>= 5.2.1)))
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project

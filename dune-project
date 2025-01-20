(lang dune 3.17)

(name ppx_validate)

(generate_opam_files true)

(source
 (github fegor04/ppx_validate))

(authors "Egor Fedorov <fegor2004@gmail.com>")

(maintainers "Egor Fedorov <fegor2004@gmail.com>")

(license MIT)

(documentation https://github.com/fegor04/ppx_validate)

(package
 (name ppx_validate)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml ocamlformat ppx_deriving ppxlib ppx_expect)
 (tags
  ("add topics" "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html

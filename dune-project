(lang dune 3.16)

(name loxcaml)

(generate_opam_files true)

(package
 (name loxcaml)
 (synopsis "A treewalking interpreter for the Lox language, written in OCaml")
 (description
  "LoxCaml is an OCaml implementation of the treewalking interpreter for the Lox language from Crafting Interpreters.")
 (maintainers "Debadree Chatterjee <debadree333@gmail.com>")
 (authors "Debadree Chatterjee <debadree333@gmail.com>")
 (homepage "https://github.com/debadree25/loxcaml")
 (bug_reports "https://github.com/debadree25/loxcaml/issues")
 (source (github debadree25/loxcaml))
 (license MIT)
 (allow_empty true)
 (version 0.1)
 (depends
  (ocaml (>= 4.14))
  dune
  lwt
  (alcotest :with-test)))

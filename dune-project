(lang dune 3.17)

(name ansifmt)

(version 2.0.0)

(generate_opam_files true)

(source
 (github qexat/ansifmt))

(authors "lexa <contact@qexat.com>")

(maintainers "lexa <contact@qexat.com>")

(license MIT)

(package
 (name ansifmt)
 (synopsis "A simple, lightweight library for ANSI styling")
 (description "A simple, lightweight library for ANSI styling.")
 (depends
  (ocaml
   (>= "5.1"))
  (ppx_inline_test :with-test)
  (ppx_expect :with-test)
  (re
   (= "1.12.0"))
  (rich-string
   (>= "1.0.0")))
 (tags
  ("ansi" "formatting" "styling" "pretty-printing" "terminal")))

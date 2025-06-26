(lang dune 3.17)

(name ansifmt)

(version 0.4.0b2)

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
   (>= "4.08"))
  (ppx_inline_test :with-test)
  (ppx_expect :with-test)
  (re
   (= "1.9.0")))
 (tags
  ("ansi" "formatting" "styling" "pretty-printing" "terminal")))

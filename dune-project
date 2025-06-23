(lang dune 3.17)

(name ansifmt)

(version 0.4.0b1)

(generate_opam_files true)

(source
 (github qexat/ansifmt))

(authors "lexa <contact@qexat.com>")

(maintainers "lexa <contact@qexat.com>")

(license MIT)

(package
 (name ansifmt)
 (synopsis "A simple, lightweight library for ANSI formatting")
 (description
  "A simple, lightweight library for ANSI formatting with powerful features such as a element-based system for pretty-printing code in the terminal.")
 (depends
  (ocaml
   (>= "4.08"))
  (ppx_inline_test :with-test)
  (ppx_expect :with-test))
 (tags
  ("ansi" "formatting" "pretty-printing" "terminal")))

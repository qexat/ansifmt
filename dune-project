(lang dune 3.17)

(name ansifmt)

(version 0.3.0)

(generate_opam_files true)

(source
 (github qexat/ansifmt))

(authors "Qexat <contact@qexat.com>")

(maintainers "Qexat <contact@qexat.com>")

(license MIT)

(package
 (name ansifmt)
 (synopsis "A simple, lightweight library for ANSI formatting")
 (description
  "A simple, lightweight library for ANSI formatting with powerful features such as a element-based system for pretty-printing code in the terminal.")
 (depends
  (ocaml
   (>= "4.08")))
 (tags
  ("ansi" "formatting" "pretty-printing" "terminal")))

(lang dune 3.11)
(using mdx 0.4)

(name chatty)

(generate_opam_files true)

(source (github leostera/chatty))

(authors "Leandro Ostera <leandro@abstractmachines.dev>")

(maintainers "Leandro Ostera <leandro@abstractmachines.dev>")

(license MIT)

(package
 (name chatty)
 (synopsis "")
 (description "")
 (depends
   (ocaml (>= "5.1"))
   (riot (>= "0.0.7"))
   (minttea (>= "0.0.2"))
   (blink (>= "0.0.1"))
   (mdx (and :with-test (>= "2.3.1"))))
 (tags ()))

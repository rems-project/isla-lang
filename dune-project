(lang dune 2.7)

(name "isla-lang")
(authors "Peter Sewell" "Thibaut Pérami")
(maintainers "Thibaut Pérami <thibaut.perami@cl.cam.ac.uk>")
(source (github rems-project/isla-lang))

(using menhir 2.0)
(using coq 0.2)

(generate_opam_files true)

(package
 (name isla-lang)
 (synopsis "Library for parsing the output of isla")
 (depends
   (pprint (>= 20171003))
   (menhir :build)
   (coq (>= 8.13.2))
   (ott (and (>= 0.31) :build))))

(lang dune 3.0)

(name "isla-lang")
(authors "Peter Sewell" "Thibaut Pérami")
(maintainers "Thibaut Pérami <thibaut.perami@cl.cam.ac.uk>")
(source (github rems-project/isla-lang))

(using menhir 2.0)
(using coq 0.3)

(generate_opam_files true)

(package
 (name isla-lang)
 (synopsis "Library for parsing and manipulating the output of isla")
 (depends
   (pprint (>= 20171003))
   (menhir :build)
   (ott (and (>= 0.33) :build))))

(package
 (name coq-isla-lang)
 (synopsis "Library for manipulating the output of isla in Coq")
 (depends
  (coq (>= 8.13))
  (coq-stdpp (= dev))
  (coq-stdpp-unstable (= dev))
  (ott (and (>= 0.33) :build))))

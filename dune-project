(lang dune 2.5)
(using coq 0.2)

(name compelib)

(generate_opam_files true)

(license MIT)
(authors "Masayuki Mizuno <mizuno@wantedly.com>")
(maintainers "Masayuki Mizuno <mizuno@wantedly.com>")
(source (github fetburner/compelib))

(package
  (name compelib)
  (synopsis "OCaml competive programming library")
  (depends
    (coq (>= 8.10))
    (coq-mathcomp-ssreflect (>= 1.6))
    (ppx_inline_test (>= 0.14.1))))

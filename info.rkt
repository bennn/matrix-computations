#lang info
(define collection "matrix-computations")
(define deps '("base"))
(define build-deps
  '(;"cover"
    ;"cover-coveralls"
    "scribble-lib"
    "rackunit-abbrevs"
    "racket-doc"
    "rackunit-lib"))
(define pkg-desc "Matrix computations")
(define version "0.0")
(define pkg-authors '(ben))
(define scribblings '(("scribblings/api.scrbl")))

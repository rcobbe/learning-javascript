#lang typed/racket

(require (prefix-in parser: "unit-tests/parser.rkt")
         (prefix-in semant: "unit-tests/semant.rkt")
         "test-framework/tests.rkt"
         (prefix-in test-fw: "test-framework/run.rkt"))

(provide run)

(: run ([#:path test-fw:Test-Path] [#:verbose Boolean] -> Void))
(define (run #:path [path null] #:verbose [verbose #f])
  (test-fw:run
    (test-suite all-tests
      parser:tests
      semant:tests)
    #:path path
    #:verbose verbose))

;; To run tests:
;;   racket -t run-tests.rkt -e "(run)"

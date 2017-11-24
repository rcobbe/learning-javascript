#lang typed/racket

(provide (all-defined-out))

;; Source location tracking for various components of the test framework
(struct srcloc ([source : Any]
                [line : Exact-Positive-Integer]))

;; Given these definitions, the types `Test' and `test' are effectively
;; equivalent, but the type checker doesn't know that.  In particular, if we
;; know
;;    x : Test
;;    (not (test-suite? x))
;; then the type checker can conclude
;;    x : test-case
;;
;; If, however, we know
;;    x : test
;;    (not (test-suite? x))
;; then the type checker cannot infer the same conclusion, presumably because
;; it has to allow for the possibility of other subtypes of `test' that we
;; don't know about here.

(define-type Test (U test-suite test-case))

;; Supertype of all tests.  We use the location for static test errors,
;; primarily duplicate names.
(struct test ([name : Symbol] [location : srcloc]))
(struct test-suite test ([contents : (Listof Test)]))
(struct test-case test ([contents : (-> Result)]))

;; Type of the result of running a test case's thunk.  The location, where
;; present, is the source location of the check that failed.
(define-type Result (U success failure uncaught-exn))
(struct success ())
(struct failure ([msg : String] [location : srcloc]))
(struct uncaught-exn ([exn : exn:fail] [location : srcloc]))

;; internal exception type used to signal test failure.  We have to use this
;; instead of just throwing a `failure' object because, in typed racket, the
;; argument to `raise' must have type (U exn s-expression), roughly.
(struct exn:failure exn ([f : failure]))

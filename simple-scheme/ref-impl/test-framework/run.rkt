#lang typed/racket

(require "private/types.rkt")

(provide Test
         Test-Path

         run)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Tests
;;
;; A Test is either a test suite or a test case.  A test suite is simply a
;; named collection of Test.  Test cases and test suites can be arranged in a
;; tree structure, similar to a filesystem hierarchy.
;;
;; A test case is, roughly, a named thunk that is invoked when the tests are
;; run.  In general, the thunk should evaluate one or more checks, but it may
;; perform any actions.
;;
;; A check is a single operation that evaluates an expression using the
;; component being tested.  If the check succeeds, evaluation proceeds to the
;; rest of the body of the thunk; otherwise, the test case aborts.
;;
;; If a test case's thunk returns normally (i.e., without raising an uncaught
;; exception and without failing a check), then we consider the test to have
;; succeeded.  Otherwise, it is counted as a failure (failed check) or an error
;; (uncaught exception).
;;
;; If a particular test case aborts, testing continues with the remainder of
;; the test suite.
;;
;; Users should not assume that tests within a suite are evaluated in any
;; particular order.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Denotes a subtree of a Test; the empty path denotes the entire Test.
(define-type Test-Path (Listof Symbol))

;; Tree-structured report from a test run.  The report tree has same structure
;; as the tests.
(define-type Report (U test-suite-report test-case-report))
(struct report ([name : Symbol]))
(struct test-suite-report report ([contents : (Listof Report)]))
(struct test-case-report report ([result : Result]))

;; Statistics from a test run
(struct stats ([total-num-tests : Nonnegative-Integer]
               [num-successes : Nonnegative-Integer]
               [num-failures : Nonnegative-Integer]
               [num-errors : Nonnegative-Integer]))

;; Runs a portion of a Test and prints a report of the results.
;;   #:path specifies the subtree to run.  If some proper prefix of the path
;;     denotes a test case, rather than a test suite, we simply run the
;;     case without printing any diagnostic.
;;   if #:verbose is true, prints each test case's path to stderr before
;;     executing it.
(: run (Test [#:path Test-Path] [#:verbose Boolean] -> Void))
(define (run test #:path [path null] #:verbose [verbose #f])
  (cond
   [(run-test test path verbose null) => print-report]
   [else (printf "no matching tests~n")]))

;; runs the subtree of `test' matching `filter-path', with the indicated
;; verbosity.  `current-path' is the path from the original root to `test', not
;; including `test'.
(: run-test (Test Test-Path Boolean Test-Path -> (Option Report)))
(define (run-test test filter-path verbose current-path)
  (let ([new-current-path (snoc (test-name test) current-path)])
    (cond
     [(not (or (null? filter-path) (eq? (car filter-path) (test-name test))))
      ;; current test doesn't match path; skip
      #f]
     [(test-suite? test)
      (run-test-suite test
                      (if (null? filter-path) null (cdr filter-path))
                      verbose
                      new-current-path)]
     ;; it's possible that we've reached a test-case but path has more than one
     ;; entry.  Even though we can't look for subtests of a test case,
     ;; signaling an error isn't worth it.
     [else (run-test-case test verbose new-current-path)])))

;; runs the subtree of `suite' corresponding to `child-filter', which is
;; interpreted relative to `suite'.  `current-path' contains path from root to
;; suite, *including* suite.
(: run-test-suite (test-suite Test-Path Boolean Test-Path -> Report))
(define (run-test-suite suite child-filter verbose current-path)
  (test-suite-report (test-name suite)
                     (filter-map
                      (lambda ([t : Test])
                        (run-test t child-filter verbose current-path))
                      (test-suite-contents suite))))

;; always runs the given test case.  `current-path' contains path from root to
;; case, *including* `case'.
(: run-test-case (test-case Boolean Test-Path -> Report))
(define (run-test-case case verbose current-path)
  (when verbose
    (fprintf (current-error-port) "~a~n" (pretty-format current-path)))
  (test-case-report (test-name case)
                    ((test-case-contents case))))

(: print-report (Report -> Void))
(define (print-report r)
  (let* ([stats (print-report-loop null r)]
         ;; match-let doesn't work in typed-racket, apparently
         [t (stats-total-num-tests stats)]
         [s (stats-num-successes stats)]
         [f (stats-num-failures stats)]
         [e (stats-num-errors stats)])
    ;; we print stats mostly to get meaningful output if all tests pass
    (printf "~a test~a~n~a success~a~n~a failure~a~n~a error~a~n"
            t
            (plural t "s")
            s
            (plural s "es")
            f
            (plural f "s")
            e
            (plural e "s"))))

;; Determines if plural ending is necessary based on numeric arg
(: plural (Number String -> String))
(define (plural num ending)
  (if (= num 1) "" ending))

;; Print a report to current output port and return statistics.  First arg must
;; be path to `report' within global report; we use this to generate names.
(: print-report-loop (Test-Path Report -> stats))
(define (print-report-loop path report)
  (let ([augmented-path (snoc (report-name report) path)])
    (match report
      [(test-suite-report _ contents)
       (foldl
        (lambda ([r : Report] [accum : stats])
          (stats+ accum (print-report-loop augmented-path r)))
        (stats 0 0 0 0)
        contents)]
      [(test-case-report _ (success))
       (stats 1 1 0 0)]
      [(test-case-report _ (failure msg loc))
       (printf "~a: ~a failed~n~a~n" (format-srcloc loc) augmented-path msg)
       (stats 1 0 1 0)]
      [(test-case-report _ (uncaught-exn exn loc))
       (printf "~a: ~a failed with uncaught exception:~n~a~n"
               (format-srcloc loc)
               augmented-path
               exn)
       (stats 1 0 0 1)])))

(: stats+ (stats stats -> stats))
(define (stats+ a b)
  (stats (+ (stats-total-num-tests a) (stats-total-num-tests b))
         (+ (stats-num-successes a) (stats-num-successes b))
         (+ (stats-num-failures a) (stats-num-failures b))
         (+ (stats-num-errors a) (stats-num-errors b))))

(: format-srcloc (srcloc -> String))
(define (format-srcloc loc)
  (format "~a:~a" (srcloc-source loc) (srcloc-line loc)))

(: snoc (All (a) a (Listof a) -> (Listof a)))
(define (snoc x xs)
  (cond
   [(null? xs) (list x)]
   [else (cons (car xs) (snoc x (cdr xs)))]))

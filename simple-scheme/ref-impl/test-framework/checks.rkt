#lang typed/racket

(require "private/types.rkt")

(provide check-true
         check-false
         check-equal?
         check-exn
         check-no-exn
         check-binary-pred
         check-values-pred)

;; Checks that an expression evaluates to a value that isn't #f
(define-syntax (check-true stx)
  (syntax-case stx ()
    [(check-true exp ...)
     #`(let ([loc (srcloc (quote #,(syntax-source stx))
                          #,(syntax-line stx))])
         (let ([val (begin exp ...)])
           (unless val
             (raise
              (exn:failure
               "check-true failed"
               (current-continuation-marks)
               (failure (format "expected #t; got ~a" val) loc))))))]))

;; Checks that an expression evaluates to #f
(define-syntax (check-false stx)
  (syntax-case stx ()
    [(check-false exp ...)
     #`(let ([loc (srcloc (quote #,(syntax-source stx))
                          #,(syntax-line stx))])
         (let ([val (begin exp ...)])
           (when val
             (raise
              (exn:failure
               "check-false failed"
               (current-continuation-marks)
               (failure (format "expected #f; got ~a" val) loc))))))]))

;; Checks that two expressions are equal?
(define-syntax (check-equal? stx)
  (syntax-case stx ()
    [(check-equal? actual expected)
     #`(let ([loc (srcloc (quote #,(syntax-source stx))
                          #,(syntax-line stx))])
         (let ([actual-val actual]
               [expected-val expected])
           (unless (equal? actual-val expected-val)
             (raise
              (exn:failure
               "check-equal? failed"
               (current-continuation-marks)
               (failure
                (format "~nExpected:~n~a~nActual:~n~a"
                        (pretty-format expected-val)
                        (pretty-format actual-val))
                loc))))))]))

(define-syntax (check-exn stx)
  (syntax-case stx ()
    [(check-exn exn-spec body ...)
     #`(let/cc return : Void
         (let* ([loc (srcloc (quote #,(syntax-source stx))
                             #,(syntax-line stx))]
                [exn-spec-value exn-spec]
                [good-exn?
                 (cond
                  [(regexp? exn-spec-value)
                   (lambda ([e : Any])
                     (and (exn:fail? e)
                          (regexp-match exn-spec-value (exn-message e))))]
                  [(procedure? exn-spec-value) exn-spec-value]
                  [else (error 'check-exn
                               "invalid exn spec ~a" exn-spec-value)])])
           (with-handlers ([good-exn? (lambda (e) (return (void)))])
             (begin
               body ...))
           (raise
            (exn:failure
             "check-exn failed"
             (current-continuation-marks)
             (failure
              (format "Expected exn matching ~a; got normal return"
                      exn-spec-value)
              loc)))))]))

;; Checks that evaluating the expression does not raise an (uncaught)
;; exception.
(define-syntax (check-no-exn stx)
  (syntax-case stx ()
    [(check-no-exn body ...)
     #`(let ([loc (srcloc (quote #,(syntax-source stx))
                          #,(syntax-line stx))])
         (with-handlers
             ([exn:failure? raise]
              [exn:fail?
               (lambda (e)
                 (raise
                  (exn:failure
                   "check-no-exn failed"
                   (current-continuation-marks)
                   (failure
                    (format "uncaught exception:~n~a" (pretty-format e))
                    loc))))])
           (begin body ... (void))))]))

(define-syntax (check-binary-pred stx)
  (syntax-case stx ()
    [(check-binary-pred pred actual expected)
     #`(let ([loc (srcloc (quote #,(syntax-source stx))
                          #,(syntax-line stx))]
             [pred-value? pred]
             [actual-value actual]
             [expected-value expected])
         (unless (pred-value? actual-value expected-value)
           (raise
            (exn:failure
             "check-binary-pred failed"
             (current-continuation-marks)
             (failure
              (format "~nExpected:~n~a~nActual:~n~a"
                      (pretty-format expected-value)
                      (pretty-format actual-value))
              loc)))))]))

(define-syntax (check-values-pred stx)
  (syntax-case stx ()
    [(check-values actual (pred expected) ...)
     #`(let* ([loc (srcloc (quote #,(syntax-source stx)) #,(syntax-line stx))]
              [actual-values (call-with-values (lambda () actual) list)]
              [predicates (list pred ...)]
              [expected-values (list expected ...)]
              [num-values (length actual-values)])
         (unless (= num-values (length predicates))
           (raise
            (exn:failure
             "check-values failure"
             (current-continuation-marks)
             (format "Expected ~a values; got ~a~n:~a"
                     (length predicates)
                     num-values
                     (pretty-format actual-values))
             locl)))
         (for-each
          (lambda (i predicate actual-value expected-value)
            (unless (predicate actual-value expected-value)
              (raise
               (exn:failure
                "check-values failure"
                (current-continuation-marks)
                (format "value ~a~nExpected:~n~a~nActual:~n~a"
                        i
                        expected-value
                        actual-value)
                (loc)))))
          (sequence-numbers num-values)
          actual-values
          expected-values))]))

;; (sequence-numbers n) produces (list 1 ... n)
(: sequence-numbers
   (Exact-Nonnegative-Integer -> (Listof Exact-Nonnegative-Integer)))
(define (sequence-numbers n0)
  (let loop ([n : Exact-Nonnegative-Integer n0]
             [accum : (Listof Exact-Nonnegative-Integer) null])
    (if (zero? n)
        accum
        (loop (sub1 n) (cons n accum)))))

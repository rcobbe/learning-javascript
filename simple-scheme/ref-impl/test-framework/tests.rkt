#lang typed/racket

(require "private/types.rkt")

(provide
 Test
 (rename-out [make-test-suite test-suite]
             [make-test-case test-case]))

(define-syntax (make-test-suite stx)
  (syntax-case stx ()
    [(make-test-suite id test ...)
     (identifier? #'id)
     #`(let ([tests (list test ...)])
         (check-dups! tests)
         (test-suite (quote id)
           (srcloc (quote #,(syntax-source stx)) #,(syntax-line stx))
           tests))]))

(define-syntax (make-test-case stx)
  (syntax-case stx ()
    [(make-test-case id body-exp ...)
     (identifier? #'id)
     #`(let ([loc (srcloc (quote #,(syntax-source stx)) #,(syntax-line stx))])
         (test-case (quote id)
           loc
           (invoke-checks loc (lambda () body-exp ...))))]))

;; Checks for duplicate test names; signals an error if found
(: check-dups! ((Listof Test) -> Void))
(define (check-dups! tests)
  (let loop ([tests tests]
             [table : (HashTable Symbol srcloc) (hash)])
    (cond
     [(null? tests) (void)]
     [(hash-ref table (test-name (car tests)) #f) =>
      (lambda (earlier-src)
        (error 'test-suite
               "Duplicate test name ~a at ~a and ~a"
               (test-name (car tests))
               (format-srcloc earlier-src)
               (format-srcloc (test-location (car tests)))))]
     [else
      (loop (cdr tests)
            (hash-set table
                      (test-name (car tests))
                      (test-location (car tests))))])))

;; Evaluates a test thunk and creates a Result to describe its behavior
(: invoke-checks (srcloc (-> Any) -> (-> Result)))
(define (invoke-checks loc test-thunk)
  (lambda ()
    (with-handlers
        ([exn:failure? exn:failure-f]
         [exn:fail? (lambda ([e : exn:fail]) (uncaught-exn e loc))])
      (test-thunk)
      (success))))

;; Formats a source-location for use in error messages
(: format-srcloc (srcloc -> String))
(define (format-srcloc loc)
  (format "~a:~a" (srcloc-source loc) (srcloc-line loc)))

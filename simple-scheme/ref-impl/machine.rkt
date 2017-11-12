#lang typed/racket

(require "types.rkt")

(define-type Const-Impl ((Listof Value) Store Continuation -> Config))

(define-type Config (U expr-config value-config))
(struct expr-config ([expr : Expr]
                     [ρ : Env]
                     [σ : Store]
                     [κ : Continuation])
        #:transparent)
(struct value-config ([val : Value]
                      [σ : Store]
                      [κ : Continuation])
        #:transparent)

;; To simplify step's return type, I've implemented it so a value configuration
;; with the `halt' continuation simply maps to itself; we leave it to the
;; caller to detect termination.
(: step (Config -> Config))
(define (step config)
  (if (expr-config? config)
      (step-expr config)
      (step-value config)))

(: step-expr (expr-config -> Config))
(define (step-expr config)
  (error 'step-expr "unimplemented"))

(: step-value (value-config -> Config))
(define (step-value config)
  (error 'step-value "unimplemented"))

(: call/cc-impl ((Listof Value) Store Continuation -> Config))
(define (call/cc-impl args σ κ)
  (match args
    [(list (? closure-val? closure))
     (apply-closure closure (list (continuation-val κ)) σ κ)]
    [else
     (error 'call/cc-impl "expected 1 arg, a closure; got ~a" args)]))

(: constants (Immutable-HashTable Symbol (U Value Const-Impl)))
;; XXX probably want to make all constants Const-Impls
(define constants
  ((inst make-immutable-hasheq Symbol (U Value Const-Impl))
    (list (cons 'null null)
          (cons 'void (lambda ([args : (Listof Value)]
                               [σ : Store]
                               [κ : Continuation])
                        (value-config (void) σ κ)))
          (cons 'call/cc call/cc-impl))))

(: apply-closure (closure-val (Listof Value) Store Continuation -> Config))
(define (apply-closure closure arg-vals σ κ)
  (error 'apply-closure "Unimplemented"))

(: length=? (All (α) (Listof α) Natural -> Boolean))
(define (length=? xs n)
  (cond
   [(zero? n) (null? xs)]
   [else (and (not (null? xs)) (length=? (cdr xs) (sub1 n)))]))

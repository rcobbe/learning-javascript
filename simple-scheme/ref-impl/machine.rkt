#lang typed/racket

(require "value.rkt")

(define-type Const-Impl ((Listof Value) Store Continuation -> Config))

(: constants (Immutable-HashTable Symbol (U Value Const-Impl)))
(define constants
  (make-immutable-hash
   (list (cons 'null null)
         (cons 'void (lambda (args σ κ) (values (void) σ κ)))
         (cons 'call/cc (lambda (args σ κ)
                          (unless (length=? args 1)
                            (error 'call/cc "Expected single argument; got ~a"
                                   args))
                          (unless (closure-val? (car args))
                            (error 'call/cc "Expected closure; got ~a"
                                   (car args)))
                          (apply-closure (car args)
                                         (list (continuation-val κ))
                                         σ
                                         κ))))))

(: apply-closure (closure-val (Listof Value) Store Continuation -> Config))
(define (apply-closure closure arg-vals σ κ)
  (error 'apply-closure "Unimplemented"))

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

(: length=? (All (α) (Listof α) Natural -> Boolean))
(define (length=? xs n)
  (cond
   [(zero? n) (null? xs)]
   [else (and (not (null? xs)) (length=? (cdr xs) (sub1 n)))]))

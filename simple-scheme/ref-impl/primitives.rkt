#lang typed/racket

(require "types.rkt")

(provide prim-env
         prim-store)

(: cons-impl Primitive-Function)
(define (cons-impl args σ)
  (match args
    [(list x y)
     (let-values ([(new-σ addrs) (alloc* σ args)])
       (values (pair-val (car addrs) (cadr addrs)) new-σ))]
    [else (error 'cons-impl
                 "expected 2 args; got ~a"
                 args)]))

(: null?-impl Primitive-Function)
(define (null?-impl args σ)
  (match args
    [(list x) (values (null? x) σ)]
    [else (error 'null?-impl "expected single arg; got ~a" args)]))

(: car-impl Primitive-Function)
(define (car-impl args σ)
  (match args
    [(list (pair-val #{kar : Addr} _))
     (values (deref σ kar) σ)]
    [else (error 'car-impl
                 "Expected single pair; got ~a"
                 args)]))

(: cdr-impl Primitive-Function)
(define (cdr-impl args σ)
  (match args
   [(list (pair-val _ #{kdr : Addr}))
    (values (deref σ kdr) σ)]
   [else (error 'cdr-impl
                "Expected single pair; got ~a"
                args)]))

(: set-car!-impl Primitive-Function)
(define (set-car!-impl args σ)
  (match args
    [(list (pair-val #{kar : Addr} _) new-val)
     (values (void) (update σ kar new-val))]
    [else (error 'set-car!-impl
                 "expected pair, value; got ~a"
                 args)]))

(: set-cdr!-impl Primitive-Function)
(define (set-cdr!-impl args σ)
  (match args
    [(list (pair-val _ #{kdr : Addr}) new-val)
     (values (void) (update σ kdr new-val))]
    [else (error 'set-cdr-impl!
                 "Expected pair, value; got ~a"
                 args)]))

;; Originally wanted to implement call/cc as a primitive, as in R5RS, but this
;; seems to require having the primitive functions return a Config, which in
;; turn seems to require dumping everything into one big module.  So, go with
;; let/cc instead.

(: make-binary-numeric-primitive
   (Symbol (-> Number Number Value) -> Primitive-Function))
(define ((make-binary-numeric-primitive op-name op) args σ)
  (if (andmap number? args)
      (values (op (car args) (cadr args)) σ)
      (error op-name "Expected numbers; got ~a" args)))

(define =-impl (make-binary-numeric-primitive '= =))
(define +-impl (make-binary-numeric-primitive '+ +))
(define --impl (make-binary-numeric-primitive '- -))
(define *-impl (make-binary-numeric-primitive '* *))
(define /-impl (make-binary-numeric-primitive '/ /))

(: not-impl Primitive-Function)
(define (not-impl args σ)
  (match args
    [(list x) (values (not x) σ)]
    [else (error 'not-impl "expected one arg; got ~a" args)]))

(: symbol=?-impl Primitive-Function)
(define (symbol=?-impl args σ)
  (match args
    [(list (? symbol? x) (? symbol? y)) (values (eq? x y) σ)]
    [else (error 'symbol=?-impl "expected 2 symbols; got ~a" args)]))

(define-values (prim-env prim-store)
  (bind* '(cons
           null
           null?
           car
           cdr
           set-car!
           set-cdr!
           =
           +
           -
           *
           /
           not
           symbol=?)
         (list cons-impl
               null
               null?-impl
               car-impl
               cdr-impl
               set-car!-impl
               set-cdr!-impl
               =-impl
               +-impl
               --impl
               *-impl
               /-impl
               not-impl
               symbol=?-impl)
         empty-env
         empty-store))

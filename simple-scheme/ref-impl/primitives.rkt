#lang typed/racket

(require "types.rkt")

(provide prim-env
         prim-store)

;; primitives raise an exn:fail on error; it is the caller's responsibility
;; to catch this and produce an error config.  Generating an error config
;; directly from these functions would require putting everything into the same
;; module: the definition of Primitive-Function (in types.rkt, because the
;; Value type refers to it) would have to include Config, defined in
;; machine.rkt.

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
   (Symbol (-> Real Real Value) -> Primitive-Function))
(define ((make-binary-numeric-primitive op-name op) args σ)
  (match args
    [(list (? real? left) (? real? right))
     (values (op left right) σ)]
    [else (error op-name "Expected 2 numbers; got ~a" args)]))

(define =-impl (make-binary-numeric-primitive '= =))
(define <-impl (make-binary-numeric-primitive '< <))
(define <=-impl (make-binary-numeric-primitive '<= <=))
(define >-impl (make-binary-numeric-primitive '> >))
(define >=-impl (make-binary-numeric-primitive '>= >=))
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
           <
           <=
           >
           >=
           +
           -
           *
           /
           not
           symbol=?)
         (list (primitive cons-impl)
               null
               (primitive null?-impl)
               (primitive car-impl)
               (primitive cdr-impl)
               (primitive set-car!-impl)
               (primitive set-cdr!-impl)
               (primitive =-impl)
               (primitive <-impl)
               (primitive <=-impl)
               (primitive >-impl)
               (primitive >=-impl)
               (primitive +-impl)
               (primitive --impl)
               (primitive *-impl)
               (primitive /-impl)
               (primitive not-impl)
               (primitive symbol=?-impl))
         empty-env
         empty-store))

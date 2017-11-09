#lang racket

;; Reference implementation, so I can check that the definition of the machine
;; has the desired properties, and provide a basis for testing the eventual
;; Javascript implementation.

;; Env ::= Hash from Sym to Addr
(define (extend-env id addr env)
  (hash-set env id addr))

(define (env-lookup env id)
  (hash-ref env id))

;; Addr = (addr N)
(struct addr ([ptr]) #:transparent)
(define (succ-addr a) (addr (add1 (addr-ptr a))))

;; Store ::= (store Addr (Hash Addr Value))
(struct store (next table) #:transparent)

(define init-store (store (addr 0) (make-hash)))

;; alloc :: Store Value -> Store Addr
(define (alloc st val)
  (let ([a (store-next st)])
    (values (store a (hash-set (store-table st) a val)) a)))
(define (deref st addr)
  (hash-ref (store-table st) addr))

;; Value ::= Number | String | Symbol
;;         | null
;;         | (pair-val Addr Addr)
;;         | Boolean
;;         | (void)
;;         | (const Symbol)
;;         | (closure-val Env (Listof Symbol) Expr)
;;         | (continuation-val Kont)
;;         | (undefined-val)
(struct pair-val (car cdr) #:transparent)
(struct const (name) #:transparent)
(struct closure-val (env formals body) #:transparent)
(struct continuation-val (k) #:transparent)
(struct undefined-val () #:transparent)

;; Expr := Number
;;       | String
;;       | (quote Symbol)
;;       | Boolean
;;       | Symbol
;;       | (lambda (Symbol ...) Expr)
;;       | (let ([Symbol Expr] ...) Expr)
;;       | (letrec ([Symbol Expr] ...) Expr)
;;       | (call/cc Expr)
;;       | (set! Symbol Expr)
;;       | (if Expr Expr Expr)
;;       | (begin Expr ...+)
;;       | (Expr ...+)

(struct Kont () #:transparent)
(struct halt Kont () #:transparent)
(struct let-rhs-k Kont (rhs-env   ;; env for evaluating remaining rhss
                        body-env  ;; env for evaluating body; gradually built as
                                  ;; we work through rhss
                        id        ;; id to be bound to value supplied to this
                                  ;; continuation
                        ids       ;; remaining IDs to be bound
                        rhss      ;; remaining RHSs to be evaluated
                        body      ;; let expr's body
                        k) #:transparent)

;; Configuration ::= (config Expr Env Store Kont)
;;                 | (value-config Value Store Kont)
(struct config (c e s k) #:transparent)
(struct value-config (v s k) #:transparent)

;; step :: Config -> Config
(define (step c)
  (cond [(config? c) (step-config c)]
        [(value-config? c) (step-value-config c)]
        [else (error 'step "Unexpected config: ~a" c)]))

(define step-config
  (match-lambda
    [(config (? number? n) e s k) (value-config n s k)]
    [(config (? string? str) e s k) (value-config str s k)]
    [(config (list 'quote (? symbol? sym)) e s k) (value-config sym s k)]
    [(config (? boolean? b) e s k) (value-config b s k)]
    [(config (? symbol? x) e s k) (value-config (deref s (env-lookup e x)) s k)]
    [(config (list 'lambda (list (? symbol? formals) ...) body) e s k)
     (value-config (closure-val e formals body) s k)]
    [(config (list 'let (list) body) e s k) (config body e s k)]
    [(config (list 'let (list (list (? symbol ids) rhss) ...) body) e s k)
     (config (car rhss) e s
             (let-rhs-k e e (car ids) (cdr ids) (cdr rhss) body k))]
    [(config (list 'letrec (list (list (? symbol ids) rhss) ...) body) e s k)
     (let-values ([(new-store addrs) (alloc-letrec-bindings s ids)]
                  [(new-env) (bind-letrec-vars env ids addrs)])
       (config (car rhss) new-env new-store
               (letrec-rhs-k new-env (car ids) (cd ids) (cdr rhss) body k)))]))

(define (alloc-letrec-bindings store ids)
  (foldl2 (lambda (id store addrs)
            (let-values ([(new-store new-addr) (alloc store (undefined-val))])
              (values store (cons new-addr addrs))))
          store
          null
          ids))

(define (bind-letrec-vars env ids addrs)
  (foldl extend-env env ids addrs))

;; f takes x, base1, base2; returns new base 1, new base 2
(define (foldl2 f base1 base2 xs)
  (if (null? xs)
      (values base1 base2)
      (let-values ([(new-base1 new-base2) (f (car xs) base1 base2)])
        (foldl2 f new-base1 new-base2 (cdr xs)))))

;; ======================================================================

;; a constant is either a value or a function
;; from Store (Listof Value) -> Store Value
(define constants
  (hash-eq
   'null null
   'cons (lambda (st args)
           (match args
             [(list car-val cdr-val)
              (let*-values ([(st1 kar) (alloc st car-val)]
                            [(st2 kdr) (alloc st1 cdr-val)])
                (values st2 (pair-val kar kdr)))]))
   'car (lambda (st args)
          (match args
            [(list (pair-val car-addr _)) (values st (deref st car-addr))]
            [error 'car "expected single pair; got ~a" args]))
   ))

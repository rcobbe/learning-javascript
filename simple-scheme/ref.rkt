#lang racket

;; Env ::= Hash from Sym to Addr

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
(struct pair-val (car cdr) #:transparent)
(struct const (name) #:transparent)
(struct closure-val (env formals body) #:transparent)
(struct continuation-val (k) #:transparent)

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

;; Configuration ::= (config Expr Env Store Kont)
;;                 | (value-config Value Store Kont)
(struct config (c e s k) #:transparent)
(struct value-config (v s k) #:transparent)

;; step :: Config -> Config
(define step
  (match-lambda
    [(config (? number? n) e s k) (value-config n s k)]
    [(config (? string? str) e s k) (value-config str s k)]
    [(config (list 'quote (? symbol? sym)) e s k) (value-config sym s k)]
    [(config (? boolean? b) e s k) (value-config b s k)]
    [(config (? symbol? x) e s k) (value-config (deref s (hash-ref e x)) s k)]
    [(config (list 'lambda (list (? symbol? formals) ...) body) e s k)
     (value-config (closure-val e formals body) s k)]
    [(config (list 'let

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

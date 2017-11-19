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

;; To simplify step's return type, a value configuration with the `halt'
;; continuation simply maps to itself; we leave it to the caller to detect
;; termination.
(: step (Config -> Config))
(define (step config)
  (if (expr-config? config)
      (step-expr config)
      (step-value config)))

(: step-expr (expr-config -> Config))
(define (step-expr config)
  (let ([ρ (expr-config-ρ config)]
        [σ (expr-config-σ config)]
        [κ (expr-config-κ config)])
    (match (expr-config-expr config)
      [(? number? n) (value-config n σ κ)]
      [(? string? s) (value-config s σ κ)]
      [(list 'quote (? symbol? s)) (value-config s σ κ)]
      [(? boolean? b) (value-config b σ κ)]
      [(? symbol? x) (value-config (deref σ (lookup ρ x)) σ κ)]
      ;; XXX symbol case needs to allow for constant references -- or,
      ;; we prep initial environment.
      [(list 'lambda (list (? symbol? #{xs : (Listof Symbol)}) ...) body)
       (check-unique! xs)
       (value-config (closure-val ρ xs body) σ κ)]
      [(list 'let (list (list (? symbol? #{xs : (Listof Symbol)})
                              #{rhss : (Listof Expr)}) ...)
             body)
       (check-unique! xs)
       (expr-config `((lambda ,xs ,body) ,@rhss) ρ σ κ)]
      [(list 'letrec _ ...)
       (error 'step-expr "letrec unimplemented")]
      [(list 'set! (? symbol? x) rhs)
       (expr-config rhs ρ σ (set!-k (lookup ρ x) κ))]
      [(list 'if #{e1 : Expr} #{e2 : Expr} #{e3 : Expr})
       (expr-config e1 ρ σ (if-k ρ e2 e3 κ))]
      [expr (error 'step-expr "unimplemented expression ~a" expr)])))

(: step-value (value-config -> Config))
(define (step-value config)
  (match config
    [(value-config v _ (halt-k)) config]
    [(value-config v σ (set!-k addr κ))
     (value-config (void) (update σ addr v) κ)]
    [(value-config test-value σ (if-k ρ e2 e3 κ))
     (expr-config (if test-value e2 e3) ρ σ κ)]
    [else (error 'step-value "unknown configuration ~a" config)]))

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

;; Ensures that all symbols in the list are unique; signasl error otherwise.
(: check-unique! ((Listof Symbol) -> Any))
(define (check-unique! xs)
  (foldl (lambda ([x : Symbol] [accum : (Setof Symbol)]) : (Setof Symbol)
           (if (set-member? accum x)
               (error 'check-unique "duplicate symbol: ~a" x)
               (set-add accum x)))
         ((inst list->seteq Symbol) null)
         xs))

(: length=? (All (α) (Listof α) Natural -> Boolean))
(define (length=? xs n)
  (cond
   [(zero? n) (null? xs)]
   [else (and (not (null? xs)) (length=? (cdr xs) (sub1 n)))]))

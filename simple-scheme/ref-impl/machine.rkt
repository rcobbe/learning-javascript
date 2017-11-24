#lang typed/racket

(require "types.rkt"
         "utils.rkt")

(define-type Const-Impl ((Listof Value) Store Continuation -> Config))

;; I chose to use an explicit value-configuration value here instead of having
;; the step function call an apply-continuation helper for two reasons:
;;   1) This makes it easier to show that we do satisfy the space requirements
;;      of the tail-call optimization in cases where one value config goes
;;      directly to another.  (This will be more important in the
;;      Javascript implementation, of course.)
;;   2) It allows us to dump value configs as well as expression configs, to
;;      get a better indication of the history of an evaluation.
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
       (value-config (closure-val ρ xs body) σ κ)]
      [(list 'let (list (list (? symbol? #{xs : (Listof Symbol)})
                              #{rhss : (Listof Expr)}) ...)
             body)
       (expr-config `((lambda ,xs ,body) ,@rhss) ρ σ κ)]
      [(list 'letrec _ ...)
       (error 'step-expr "letrec unimplemented")]
      [(list 'set! (? symbol? x) rhs)
       (expr-config rhs ρ σ (set!-k (lookup ρ x) κ))]
      [(list 'if #{e1 : Expr} #{e2 : Expr} #{e3 : Expr})
       (expr-config e1 ρ σ (if-k ρ e2 e3 κ))]
      [(list 'begin #{es : (Listof Expr)} ...) (step-begin es ρ σ κ)]
      [(list #{rator : Expr} #{rands : (Listof Expr)} ...)
       (expr-config rator ρ σ (rator-k ρ rands κ))]
      [expr (error 'step-expr "unimplemented expression ~a" expr)])))

(: step-value (value-config -> Config))
(define (step-value config)
  (match config
    [(value-config v _ (halt-k)) config]
    [(value-config v σ (set!-k addr κ))
     (value-config (void) (update σ addr v) κ)]
    [(value-config test-value σ (if-k ρ e2 e3 κ))
     (expr-config (if test-value e2 e3) ρ σ κ)]
    [(value-config _ σ (begin-k ρ exprs κ))
     (expr-config (car exprs) ρ σ (begin-k ρ (cdr exprs) κ))]
    [(value-config rator-value σ (rator-k ρ args κ))
     (step-rator rator-value σ ρ args κ)]
    [(value-config rand-value σ (rand-k _ clo arg-values (list) κ))
     (apply-closure clo (reverse (cons rand-value arg-values)) σ κ)]
    [(value-config rand-value σ (rand-k ρ clo arg-values remaining-args κ))
     (expr-config (car remaining-args) ρ σ
                  (rand-k ρ clo
                          (cons rand-value arg-values)
                          (cdr remaining-args)
                          κ))]
    [else (error 'step-value "unknown configuration ~a" config)]))

;; Takes a single stop when evaluating a BEGIN expression
(: step-begin ((Listof Expr) Env Store Continuation -> Config))
(define (step-begin exprs ρ σ κ)
  (cond
   [(null? exprs) (value-config (void) σ κ)]
   [(null? (cdr exprs)) (expr-config (car exprs) ρ σ κ)]
   [else (expr-config (car exprs) ρ σ (begin-k ρ (cdr exprs) κ))]))

;; Takes a single step when supplying a value to a `rator-k'.
(: step-rator (Value Store Env (Listof Expr) Continuation -> Config))
(define (step-rator rand-val σ ρ args κ)
  (cond
   [(not (closure-val? rand-val))
    (error 'step-rator "Expected closure; got ~a" rand-val)]
   [(null? args) (apply-closure rand-val args σ κ)]
   [else (expr-config (car args)
                      ρ
                      σ
                      (rand-k ρ
                              rand-val
                              null
                              (cdr args)
                              κ))]))

;; Apply a closure to arguments.
(: apply-closure (closure-val (Listof Value) Store Continuation -> Config))
(define (apply-closure clo actuals σ κ)
  (match-let ([(closure-val ρ formals body) clo])
    (unless (same-length? formals actuals)
      (let ([num-formals (length formals)])
        (error 'apply-closure
               "Closure expected ~a arg~a; got ~a"
               num-formals
               (if (= num-formals 1) "" "s")
               (length actuals))))
    (let-values ([(new-ρ new-σ) (bind* formals actuals ρ σ)])
      (expr-config body new-ρ new-σ κ))))

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

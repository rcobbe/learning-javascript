#lang typed/racket

;; Defines general types for reference implementation.  I'd have preferred to
;; split this into separate modules, including at least one module each for
;; env and store, but unfortunately the mutually-recursive nature of the types
;; makes this impossible.  I'd tried to solve this by making the Store and Env
;; types polymorphic, but the type system doesn't do what I need here; see the
;; thread at
;; <https://groups.google.com/forum/#!topic/racket-users/Ma9Fh72gfQg>.  In
;; principle I could move Store out to a separate module, but it doesn't really
;; seem to be worth it.

(provide Addr

         Env
         empty-env
         lookup
         bound?
         extend
         extend*

         Store
         empty-store
         alloc
         alloc*
         deref
         update

         bind
         bind*

         Value
         (struct-out pair-val)
         (struct-out const)
         (struct-out closure-val)
         (struct-out continuation-val)
         (struct-out undefined-val)

         Expr

         Continuation
         (struct-out halt-k)
         (struct-out set!-k)
         (struct-out if-k)
         )

;; Represents an address in a Store
(struct addr ([ptr : Natural]) #:transparent)
(define-type Addr addr)

;; Increment an address
(: next-addr (Addr -> Addr))
(define (next-addr a)
  (addr (add1 (addr-ptr a))))

;; An environment mapping identifiers to addresses
(struct env ([bindings : (Immutable-HashTable Symbol Addr)]) #:transparent)
(define-type Env env)

;; An environment with no bindings
(: empty-env Env)
(define empty-env (env (make-immutable-hash null)))

;; Looks up a symbol in an environment.  Aborts if not found.
(: lookup (Env Symbol -> Addr))
(define (lookup ρ x)
  (hash-ref (env-bindings ρ) x
            (lambda ()
              (error 'lookup "identifier ~a not bound" x))))

;; Does the environment contain a binding for the symbol?
(: bound? (Env Symbol -> Boolean))
(define (bound? ρ x)
  (hash-has-key? (env-bindings ρ) x))

;; Extends an environment with a new binding, shadowing any existing binding.
(: extend (Symbol Addr Env -> Env))
(define (extend x a ρ)
  (env (hash-set (env-bindings ρ) x a)))

;; Extend an environment with a sequence of bindings, shadowing as
;; appropriate.  If the same symbol occurs multiple times in the argument, the
;; binding to the right takes precedence.
(: extend* ((Listof Symbol) (Listof Addr) Env -> Env))
(define (extend* xs addrs ρ)
  (unless (same-length? xs addrs)
    (error 'extend* "got ~a symbols, ~a addrs" (length xs) (length addrs)))
  (foldr extend ρ xs addrs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Stores
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Store mapping addresses to values.
(struct store ([next : Addr]  ;; next address to allocate
               [values : (Immutable-HashTable Addr Value)])
        #:transparent)
(define-type Store store)

;; Store with no contents
(: empty-store Store)
(define empty-store (store (addr 0) (make-immutable-hash null)))

;; Allocate a new address, and initialize it to the specified value.
(: alloc (Store Value -> (Values Store Addr)))
(define (alloc σ val)
  (let ([a (store-next σ)])
    (values (store (next-addr a) (hash-set (store-values σ) a val))
            a)))

;; Allocate space for multiple values; return addresses for each.
(: alloc* (Store (Listof Value) -> (Values Store (Listof Addr))))
(define (alloc* σ vals)
  (foldr2 (lambda ([val : Value]
                   [σ : Store]
                   [addrs : (Listof Addr)])
            (let-values ([(new-σ addr) (alloc σ val)])
              (values new-σ (cons addr addrs))))
          σ
          null
          vals))

;; Look up the value at an address.  Signals an error if address is not
;; allocated.
(: deref (Store Addr -> Value))
(define (deref σ a)
  (hash-ref (store-values σ) a
            (lambda ()
              (error 'deref "Bad address ~a" a))))

;; Update the value at an address.  Aborts if address is not allocated.
(: update (Store Addr Value -> Store))
(define (update σ a v)
  (let ([t (store-values σ)])
    (if (hash-has-key? t a)
        (struct-copy store [values (hash-set t a v)])
        (error 'update "Bad address ~a" a))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Bindings
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Bind an identifier to a value; return updated env and store.
(: bind (Symbol Value Env Store -> (Values Env Store)))
(define (bind x val ρ σ)
  (let-values ([(new-σ addr) (alloc σ val)])
    (values (extend x addr ρ) new-σ)))

(: bind/p ((Pairof Symbol Value) Env Store -> (Values Env Store)))
(define (bind/p symval ρ σ)
  (bind (car symval) (cdr symval) ρ σ))

;; Binds multiple identifiers to values; returns updated env & store.  If a
;; symbol appears multiple times, right-hand occurrence. takes precedence.
(: bind* ((Listof Symbol) (Listof Value) Env Store -> (Values Env Store)))
(define (bind* xs vals ρ σ)
  (foldr2 bind/p ρ σ (zip xs vals)))

(: zip (All (α β) (Listof α) (Listof β) -> (Listof (Pairof α β))))
(define (zip xs ys)
  (cond
   [(and (null? xs) (null? ys)) null]
   [(or (null? xs) (null? ys)) (error 'zip "lists of different length")]
   [else (cons (cons (car xs) (car ys)) (zip (cdr xs) (cdr ys)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Values
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct pair-val ([car : Addr] [cdr : Addr]) #:transparent)
(struct const ([name : Symbol]) #:transparent)
(struct closure-val ([ρ : Env]
                     [formals : (Listof Symbol)]
                     [body : Expr]) #:transparent)
(struct continuation-val ([κ : Continuation]) #:transparent)
(struct undefined-val () #:transparent)

(define-type Value
  (U Number
     String
     Symbol
     Null
     Boolean
     Void
     pair-val
     const
     closure-val
     continuation-val
     undefined-val))

(define-type Expr Sexp)
;; Expr ::= Number
;;        | String
;;        | (quote Symbol)
;;        | Boolean
;;        | Symbol
;;        | (lambda (Symbol ...) Expr)
;;        | (let ([Symbol Expr] ...) Expr)
;;        | (letrec ([Symbol Expr] ...) Expr)
;;        | (set! Symbol Expr)
;;        | (if Expr Expr Expr)
;;        | (begin Expr ...)
;;        | (Expr ...+)
;; null, void, call/cc are constants.

(define-type Continuation (U halt-k set!-k if-k))
(struct halt-k () #:transparent)
(struct set!-k ([a : Addr] [κ : Continuation]) #:transparent)
(struct if-k ([ρ : Env]
              [consequent : Expr]
              [alternative : Expr]
              [κ : Continuation])
        #:transparent)

;; Do two lists have the same length?  Linear in length of shorter list.
(: same-length? (All (α β) (Listof α) (Listof β) -> Boolean))
(define (same-length? xs ys)
  (if (null? xs)
      (null? ys)
      (and (not (null? ys))
           (same-length? (cdr xs) (cdr ys)))))

;; Foldr for functions with 2 accumulators and thus 2 return values.
(: foldr2 (All (α β γ) (γ α β -> (Values α β)) α β (Listof γ) -> (Values α β)))
(define (foldr2 f accum1 accum2 xs)
  (if (null? xs)
      (values accum1 accum2)
      (let-values ([(new-accum1 new-accum2) (foldr2 f accum1 accum2 (cdr xs))])
        (f (car xs) new-accum1 new-accum2))))

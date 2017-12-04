#lang typed/racket

(define-signature store^
  [empty-store : Store]
  Addr
  Store
  [alloc : (Store Integer -> (Values Store Addr))]
  [alloc* : (Store (Listof Integer) -> (Values Store (Listof Addr)))]
  [deref : (Store Addr -> Integer)]
  [update : (Store Addr Integer -> Store)])

(define store@
  (unit
    (import)
    (export store^)

    (struct addr ([ptr : Natural]))
    (define-type Addr addr)

    (: next-addr (Addr -> Addr))
    (define (next-addr a)
      (addr (add1 (addr-ptr a))))

    (struct store ([next : Addr] [values (Immutable-HashTable Addr Integer)]))
    (define-type Store store)

    (: empty-store Store)
    (define empty-store (store (addr 0) (make-immutable-hash null)))

    (: alloc (Store Integer -> (Values Store Addr)))
    (define (alloc σ val)
      (let ([a (store-next σ)])
        (values (store (next-addr a) (hash-set (store-values σ) a val))
                a)))

    ;; Allocate space for multiple values; return addresses for each.
    (: alloc* (Store (Listof Integer) -> (Values Store (Listof Addr))))
    (define (alloc* σ vals)
      (foldr2 (lambda ([val : Integer]
                       [σ : Store]
                       [addrs : (Listof Addr)])
                (let-values ([(new-σ addr) (alloc σ val)])
                  (values new-σ (cons addr addrs))))
              σ
              null
              vals))

    ;; Look up the value at an address.  Signals an error if address is not
    ;; allocated.
    (: deref (Store Addr -> Integer))
    (define (deref σ a)
      (hash-ref (store-values σ) a
                (lambda ()
                  (error 'deref "Bad address ~a" a))))

    ;; Update the value at an address.  Aborts if address is not allocated.
    (: update (Store Addr Integer -> Store))
    (define (update σ a v)
      (let ([t (store-values σ)])
        (if (hash-has-key? t a)
            (struct-copy store [values (hash-set t a v)])
            (error 'update "Bad address ~a" a))))))

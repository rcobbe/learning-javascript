#lang typed/racket

(provide same-length?
         length=?
         foldr2
         check-unique!)

;; Do two lists have the same length?  Linear in length of shorter list.
(: same-length? (All (α β) (Listof α) (Listof β) -> Boolean))
(define (same-length? xs ys)
  (if (null? xs)
      (null? ys)
      (and (not (null? ys))
           (same-length? (cdr xs) (cdr ys)))))

;; Is the length of the list equal to the given number?
(: length=? (All (α) (Listof α) Natural -> Boolean))
(define (length=? xs n)
  (cond
   [(zero? n) (null? xs)]
   [else (and (not (null? xs)) (length=? (cdr xs) (sub1 n)))]))

;; Foldr for functions with 2 accumulators and thus 2 return values.
(: foldr2 (All (α β γ) (γ α β -> (Values α β)) α β (Listof γ) -> (Values α β)))
(define (foldr2 f accum1 accum2 xs)
  (if (null? xs)
      (values accum1 accum2)
      (let-values ([(new-accum1 new-accum2) (foldr2 f accum1 accum2 (cdr xs))])
        (f (car xs) new-accum1 new-accum2))))

;; Ensures that all symbols in the list are unique; signasl error otherwise.
(: check-unique! ((Listof Symbol) -> Any))
(define (check-unique! xs)
  (foldl (lambda ([x : Symbol] [accum : (Setof Symbol)]) : (Setof Symbol)
           (if (set-member? accum x)
               (error 'check-unique "duplicate symbol: ~a" x)
               (set-add accum x)))
         ((inst list->seteq Symbol) null)
         xs))

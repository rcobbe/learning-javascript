#lang typed/racket

(provide same-length?
         foldr2)

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

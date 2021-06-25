(define (sum n total)
(if (zero? n) total
  (sum (- n 1) (+ n total))))
(sum 1001 0)
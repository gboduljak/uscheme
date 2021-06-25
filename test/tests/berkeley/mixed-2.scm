(define square (lambda (x) (* x x)))

(apply square '(2))

(apply + '(1 2 3 4))

(apply (if false + append) '((1 2) (3 4)))

(if 0 1 2)

(if '() 1 2)

(or false true)

(or)

(and)

(or 1 2 3)

(and 1 2 3)

(and false (/ 1 0))

(or 3 (/ 1 0))


(or (quote hello) (quote world))

(if nil 1 2)

(if 0 1 2)

(if (or false false #f) 1 2)

(define (loop) (loop))
(cond (false (loop))
      (12))

((lambda (x) (display x) (newline) x) 2)

(define (print-and-square x)
  (display x)
  (square x))
(print-and-square 12)
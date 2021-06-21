(define fib (lambda (n) (
if (< n 3)
    1
    (+ (fib (- n 1)) (fib (- n 2)))
)
))

(define factorial (
lambda (n) (
    if (= n 1) 
        1 
        (* n (factorial (- n 1)))
    )
))

(define ackermann (
lambda (m n) (
    if (= m 0)
        (+ n 1)
        (if (= n 0)
            (ackermann (- m 1) 1)
            (ackermann (- m 1) (ackermann m (- n 1))))
)))
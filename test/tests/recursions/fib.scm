(define fib (lambda (n) (
    if (< n 3)
        1
        (+ (fib (- n 1)) (fib (- n 2)))
    )
))
(fib 1)
(fib 2)
(fib 3)
(fib 4)
(fib 5)
(fib 10)
(fib 20)
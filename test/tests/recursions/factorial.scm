(define factorial (
    lambda (n) (
        if (= n 1) 
            1 
            (* n (factorial (- n 1)))
        )
))
(factorial 1)
(factorial 2)
(factorial 3)
(factorial 4)
(factorial 5)
(factorial 6)
(factorial 7)
(factorial 8)
(factorial 9)
(factorial 10)
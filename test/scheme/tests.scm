(define fib (lambda (n) (
    if (< n 3)
        1
        (+ (fib (- n 1)) (fib (- n 2)))
    )
))
(fib 10)
; 55

(define factorial (
lambda (n) (
    if (= n 1) 
        1 
        (* n (factorial (- n 1)))
    )
))
(factorial 6)
; 720

(define ackermann (
    lambda (m n) (
        if (= m 0)
            (+ n 1)
            (if (= n 0)
                (ackermann (- m 1) 1)
                (ackermann (- m 1) (ackermann m (- n 1))))
)))
(ackermann 3 3)
; 61

(define (counter inc) (lambda (x) (set! inc (+ x inc)) inc ))
(define my-count (counter 5))
(my-count 3); 8;
(my-count 6); 14;
(my-count 5); 5;

(let ((a 5) (b 6)) (let  ((a 10)) a )) 
; 10
(let ((a 5) (b 6)) (let  ((a 10)) b )) 
; 6


(let ((x 5) (f (lambda (y) y)  ) ) f)
; lambda

(let ((x 5) (f (lambda (y) y)  ) ) ( f x ))
; 5, scope of lambda is root

(let ((x 5) (f (lambda (y) y)  ) ) ( 
    let ((g  (lambda (z) z ))) (g (f x))
  ))
; equivalent to (let ((x 5) (f (lambda (y) y)  ) ) ( let ((g  (lambda (z) z ))) (g (f x)) ))
; 5, scope of lambda g is 1

;
(define x 8)
(define sumWith5 (lambda (x) (+ x 5)))
(sumWith5 2)
x
; x in root should be 8, value 7

(define (apply func arg) (func arg))
(apply bool? 4)
;

(define (curry func arg1)  (lambda (arg) (func arg1 arg)))
(define (sumWith5 x) ((curry + 5) x ))
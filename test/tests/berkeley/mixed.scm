(+ 3 6)

(even? 2)

(odd? 3)

'(1 (2 three . (4 . 5)))

(define (add_one x) (+ x 1)) (add_one 2)

(define (multiply_ten x) (* x 10)) (multiply_ten 5)

(define sign (lambda (x) (if (> x 0) (quote +) (quote -)))) (sign 10)

(define (cddr s) (cdr (cdr s)))

(define (cadr s) (car (cdr s)))

(define (caddr s) (car (cddr s)))

(define evens (list 0 2 4 6 8))

(cddr evens)

(cadr evens)

(caddr evens)

(car '(1 2))

(define (increment x) (+ x 1))

(define (two x) (/ x (* x 2)))

(define (fib n)
  (if (< n 2) 
    1 
    (+ (fib (- n 1)) (fib (- n 2)))))

(fib 9)

(increment (two 5))

'scheme!

'(i love coding in scheme)

(begin 'a 'b)

(begin (+ 1 7) (+ 2 2))

(begin (display 1) (+ 1 1))

(begin 9 'nine)

(lambda (x y) (/ x y))

((lambda (y) 42 (* y 2)) 5)

(define equation (+ 9 9))
equation

(define (f x) (* x 5))
f

(and)

(or)

(and 1 2 3 4)

(and false 1 2 3 4)

(or false 1 2 3 4)

(or false false false)

(if (= 3 1) true false)

(cond ((= 5 8) 'wrong)
    (else 'conditional))

(cond ((= 2 5) 'water)
  ((= 100 1000) 'fire)
    ((= 10000 10000) 'sup))

(cond ((= 4 4) 'here (define value_b 4) 42 (* value_b 3))
     (else 'wat 0))


(if (= 10 10) 10 'wrong)

(let ((x (lambda (x) (* x x))) (y 5)) (x y))

(let ((a 5)
    (b (* 10 2)))
    (* a b))

(define value_a 'sun)
(define value_b 'moon)
(let ((value_a 6)
      (value_b (+ 1 8)))
    (list value_a value_b))

(list value_a value_b)
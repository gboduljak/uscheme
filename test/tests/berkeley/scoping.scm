((lambda (x y z) (+ x y (square z))) 1 2 3)

(define (f x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))
(f 3 4)

(define x 5)
(+ (let ((x 3))
     (+ x (* x 10)))
   x)

(let ((x 3)
      (y (+ x 2)))
  (* x y))
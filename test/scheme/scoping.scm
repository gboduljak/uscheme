(let ((x 5) (f (lambda (x y) (+ x y)))) (begin (define g (lambda (y z) ( + (+ x y) (+ 0 z) ))) (g x 6)))

(define not (lambda (x) (if x #f #t)))

(define (factorial x) (if (= x 0) 1 (* x (factorial (- x 1)))))

(define factorial2 (lambda (x) (if (= x 0) 1 (* x (factorial2 (- x 1))))))
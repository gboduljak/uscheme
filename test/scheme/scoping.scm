(let ((x 5) (f (lambda (x y) (+ x y)))) (begin (define g (lambda (y z) ( + (+ x y) (+ 0 z) ))) (g x 6)))
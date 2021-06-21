(let ((x 5) (f (lambda (x y) (+ x y)))) (begin (define g (lambda (y z) ( + (+ x y) (+ 0 z) ))) (g x 6)))

(define foo 1)

(define (baz) foo)

(set! foo 12)

(define (quux)
   (let ((foo 6))
      (baz)))

(quux)
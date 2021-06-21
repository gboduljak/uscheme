
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


(let ((x 42) (y (* x 10))) (list x y)); undefined x

(define (abs x)
(cond ((> x 0) x)
      ((= x 0) 0)
      ((< x 0) (- x))))
    
(define (add-rat x y)
(make-rat (+ (* (numer x) (denom y))
             (* (numer y) (denom x)))
          (* (denom x) (denom y))))
(define (sub-rat x y)
(make-rat (- (* (numer x) (denom y))
             (* (numer y) (denom x)))
          (* (denom x) (denom y))))
(define (mul-rat x y)
(make-rat (* (numer x) (numer y))
          (* (denom x) (denom y))))
(define (div-rat x y)
(make-rat (* (numer x) (denom y))
          (* (denom x) (numer y))))
(define (equal-rat? x y)
(= (* (numer x) (denom y))
   (* (numer y) (denom x))))

(define x (cons 1 2))
(car x)

(cdr x)

(define x (cons 1 2))
(define y (cons 3 4))
(define z (cons x y))
(car (car z))

(car (cdr z))

z

(define (make-rat n d) (cons n d))
(define (numer x) (car x))
(define (denom x) (cdr x))
(define (print-rat x)
(display (numer x))
(display '/)
(display (denom x))
(newline))
(define one-half (make-rat 1 2))
(print-rat one-half)

(define one-third (make-rat 1 3))
(print-rat (add-rat one-half one-third))

(print-rat (mul-rat one-half one-third))

(print-rat (add-rat one-third one-third))

(define (gcd a b)
(if (= b 0)
    a
    (gcd b (remainder a b))))
(define (make-rat n d)
(let ((g (gcd n d)))
  (cons (/ n g) (/ d g))))
(print-rat (add-rat one-third one-third))

(define one-through-four (list 1 2 3 4))
one-through-four

(car one-through-four)

(cdr one-through-four)

(car (cdr one-through-four))

(cons 10 one-through-four)

(cons 5 one-through-four)

(define (map proc items)
(if (null? items)
    nil
    (cons (proc (car items))
          (map proc (cdr items)))))
(map abs (list -10 2.5 -11.6 17))

(map (lambda (x) (* x x))
   (list 1 2 3 4))

(define (scale-list items factor)
(map (lambda (x) (* x factor))
     items))
(scale-list (list 1 2 3 4 5) 10)

(define (count-leaves x)
(cond ((null? x) 0)
      ((not (pair? x)) 1)
      (else (+ (count-leaves (car x))
               (count-leaves (cdr x))))))
(define x (cons (list 1 2) (list 3 4)))
(count-leaves x)

(count-leaves (list x x))

(define (odd? x) (= 1 (remainder x 2)))
(define (filter predicate sequence)
(cond ((null? sequence) nil)
      ((predicate (car sequence))
       (cons (car sequence)
             (filter predicate (cdr sequence))))
      (else (filter predicate (cdr sequence)))))
(filter odd? (list 1 2 3 4 5))

(define (accumulate op initial sequence)
(if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))
(accumulate + 0 (list 1 2 3 4 5))

(accumulate * 1 (list 1 2 3 4 5))

(accumulate cons nil (list 1 2 3 4 5))

(define (enumerate-interval low high)
(if (> low high)
    nil
    (cons low (enumerate-interval (+ low 1) high))))
(enumerate-interval 2 7)

(define (enumerate-tree tree)
(cond ((null? tree) nil)
      ((not (pair? tree)) (list tree))
      (else (append (enumerate-tree (car tree))
                    (enumerate-tree (cdr tree))))))
(enumerate-tree (list 1 (list 2 (list 3 4)) 5))

(define (len s)
(if (eq? s '())
  0
  (+ 1 (len (cdr s)))))
(len '(1 2 3 4))
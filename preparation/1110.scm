;; 1.1.4

(define (square x) (* x x))
(square 21)
;; 441

(square (+ 2 5))
;; 49

(define (sum_of_squares x y)
    (+ (square x) (square y)))

(sum_of_squares 3 4)
;; 25

(define (f a)
    (sum_of_squares (+ a 1) (* a 2)))

(f 5)
;; 136


;; 1.1.5

;; no process to show

;; 1.1.6
(define (abs x)
    (cond ((> x 0) x)
          ((= x 0) 0)
          ((< x 0) (-x))))

(define (abs x)
    (if (< x 0)
        (-x)
        x))


(and (> x 5) (< x 10))

(define (>= x y)
    (or (> x y) (= x y)))

(define (>= x y)
    (not (< x y)))


;; Excercise 1.1
10
;; 10
(+ 5 3 4)
;; 12
(- 9 1)
;; 8
(/ 6 2)
;; 3
(+ (* 2 4) (- 4 6))
;; 6
(define a 3)
(define b (+ a 1))
(+ a b (* a b))
;; 19
(= a b)
;; #f
(if (and (> b a) (< b (* a b)))
    b
    a)
;; 4 , returns b
(cond ((= a 4) 6)
       (= b 4) (+ 6 7 a)
       (else 25))
;; 4
(+ 2 (if (> b a) b a))
;; 6
(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1))
;; 16

;; Excercise 1.2
(/ (+ 5 4
      (- 2
         (- 3
            (+ 6
               (/ 4 5)))))
   (* 3
      (- 6 2)
      (- 2 7)))


;; Excercise 1.3    動かない。。。
(define (test d e)
  (+ (* d d) (* e e)))
(define (func a b c)
  (cond ((and (> a b) (> b c))
         (test a b))
        ((and (> b c) (> c a))
         (test b c))
        ((and (> c a) (> a b))
         (test c a))))
(func 1 2 3)


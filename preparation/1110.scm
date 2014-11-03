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

;; 1
10

;; 2
(+ 5 3 4)

;; 3
(- 9 1)

;; 4
(/ 6 2)

;; 5
(+ (* 2 4) (- 4 6))

;; 6
(define a 3)

;; 7
(define b (+ a 1))

;; 8
(if (and (> b a) (< b (* a b)))
	b
	a)

;; 9
(cond ((= a 4) 6)
			((= b 4) (+ 6 7 a))
			(else 25))

;; 10
(+ 2 (if (> b a) b a))

;; 11
(* (cond ((> a b) a)
				 ((< a b) b)
				 (else -1))
	 (+ a 1))

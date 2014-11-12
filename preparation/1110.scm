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

(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3 (- 6 2) (- 2 7)))


;; Excercise 1.3
(define (test d e)
  (+ (* d d) (* e e)))

(define (power a b c)
  (cond ((> a b)
         (if (> b c)
           (test a b)
           (test a c)))
        ((> b c)
         (if (> c a)
           (test b c)
           (test b a)))
        ((> c a)
         (if (> a b)
           (test c a)
           (test c b)))))

;; Excercise 1.4
(define (a-plus-abs-b a b)
    ((if (> b 0)
       +
       -)
     a b))

(a-plus-abs-b 1 2)
;; 3
(a-plus-abs-b 1 -2)
;; 3
;; 最初の関数がbの値によって決定する


;; Excercise 1.5
(define (p) (p))

(define (test x y)
    (if (= x 0)
      0
      y))

;;無限ループ
;;pという関数を作っていて、その戻り値がpの呼び出しを行ってる
;;



;; 1.1.7


(define (square x)
    (* x x))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
        (print guess)
        guess
        (sqrt-iter (improve guess x)
        x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(sqrt 9)

;; Excercise 1.6
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
          (else else-clause)))

(new-if (= 2 3) 0 5)

(define (sqrt-iter guess x)
  (new-if ; #f(good-enough? guess x)
          ; 1.0  guess
            (sqrt-iter (improve guess x) ; 3, 2, 10
                x))) ;10

;def new-if(test, then, else):
;    if test:
;        return then
;    else:
;        return else

;def sqrt-iter(g, x):
;    if good-enough?(g,x):
;        return g
;    else:
;        return sqrt-iter(improve(g, x), x)

;def sqrt-iter(g, x):
;    return new-if(good-enough?(g,x), g, sqrt-iter(improve(g, x) x))


(sqrt-iter 1.0 10)
;;0  (good-enough? guess x)
;;      At line 44 of "(stdin)"
;;1  (sqrt-iter (improve guess x) x)
;;      At line 46 of "(stdin)"
;; 無限ループと思わえる

;; 調べてみると、ifやcondは特殊形式であるが、new-ifは手続きの適用になる。手続きの適用ではすべての中身が評価される。
;; そのために、trueになっても、計算が続き無限ループに入る

;; Excercise 1.7
;; 小さい場合、誤差が大きい。大きい場合、計算回数が多くなる　くらいしかわからん
;; 上のことが本当なら、割合で決めるべき。
(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess previous_guess)
    (if (or (good-enough? guess) (< (abs (- previous_guess guess)) 0.001))
        guess
        (sqrt-iter (improve guess) previous_guess)))
  (define (square x)
    (* x x))
  (define (average x y)
    (/ (+ x y) 2))
  (define (sqrt-iter1 guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess) guess)))
  (sqrt-iter1 1.0))


;; excercise 1.8
(define (route x y)
    (/ (+ (/ x (* y y)) (* 2 y)) 3)

(define (cubic_route guess)
  (define (good-enough? guess)
    (< (abs (- (square guess) guess)) 0.001))
  (define (improve guess)
    (/ (+ (/ guess (* improve improve)) (* 2 improve)) 3))
  (define (sqrt-iter guess previous_guess)
    (if (or (good-enough? guess) (< (abs (- previous_guess guess)) 0.001))
        guess
        (sqrt-iter (improve guess) previous_guess)))
  (define (sqrt-iter1 guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess) guess)))
  (sqrt-iter1 1.0))

(define (cubic guess previous x)
  (if (good-enough? guess previous)
    guess
    (cubic guess (improve guess x) x)))

(define (good-enough? guess previous)
  (< (abs (- (- previous guess) guess)) 0.001))

(define (improve guess x)
  (print guess)
  (/ (+ (/ x (* guess guess)) (* 2 guess)) 3))

(define (cube_root x)
  (cubic 0.0 1.0 x))


(define (cubic guess previous x)
    (if (good-enough? guess previous)
    guess
    (cubic (improve guess x) guess x)))

(define (good-enough? guess previous)
    (< (abs (- guess previous)) 0.001))

(define (improve guess x)
  (/ (+ (/ x (* guess guess)) (* 2 guess)) 3))

(define (cubic1 guess x)
    (cubic (improve guess x) guess x))

(define (cube_root x)
    (cubic1 (improve 1.0 x) x))



(define (factorial n)
    (if (= n 1)
    1
    (* n (factorial (- n 1)))))

(define (factorial n)
    (fact_iter 1 1 n))

(define (fact_iter product count max_count)
    (if (> count max_count)
        product
        (fact_iter (* count product)
            (+ count 1)
            (max_count))))


(define (factorial n)
    (fact-iter 1 1 n))

(define (fact-iter product counter max-count)
  (if (> counter max-count)
    product
    (fact-iter (* counter product)
               (+ counter 1)
                max-count)))

;; 線形反復プロセスと線形再帰プロセスの違いがよくわからん。
;; one of the character of recursive process is a chain of deferred operation
;; iterative process has state variables, together with a fixed rule that describes how the state variables should be updated as the process proceed to next step

;; recursive procedure is distinct from recursive recursive iteration in terms of syntax. the procedure is syntactic fact itself. on the other hand, the iteration

;;[1]
(define (+ a b)
    (if (= a 0)
        b
        (inc (+ (dec a) b))))

;; [2]
(define (+ a b)
    (if (= a 0)
        b
        (+ (dec a) (inc b))))

;; 置き換えモデルで(+ 4 5)
;　下のは違うっぽい
(define (+ 4 5)
    (if (= 4 0)
        8
        (inc (+ (dec 4) 5))))
(define (+ 4 5)
    (if (= 4 0)
        8
        (+ (+ (- 4 1) 5 1))))
(define (+ 4 5)
    (if (= 4 0)
        8
        9))

(define (+ a b)
    (if (= a 0)
        b
        (+ (dec a) (inc b))))
(define (+ 4 5)
    (if (= 4 0)
        5
        (+ (dec 4) (inc 5))))
(define (+ 4 5)
    (if (= 4 0)
        5
        (+ (- 4 1) (+ 5 1))))
(define (+ 4 5)
    (if (= 4 0)
        5
        9))

(define (+ 4 5)
    (if (= 4 0)
        8
        (inc (+ (dec 4) 5))))
(define (+ 4 5)
    (if (= 4 0)
        8
        (plus (+ (minus 4 1) 5 1))))
(define (+ 4 5)
    (if (= 4 0)
        8
        (plus (if (= (minus 4 1) 0)
            8
            (plus (+ (minus 4 1) 5 1))
        (minus 4 1) 5 1))))
;; recursive stateを示す変数がないため

;; 1.10
(define (A x y)
    (cond ((= y 0) 0)
          ((= x 0) (* 2 y))
          ((= y 1) 2)
          (else (A (- x 1) (A x (- y 1))))))

(A 1 10)  ;; 1024
(A 2 4)   ;; 65536
(A 3 3)   ;; 65536

(define (f n) (A 0 n))  ; fn = 2*n
(define (g n) (A 1 n))  ; gn = 2^n
(define (h n) (A 2 n))  ; hn = (n+1) * 2^(n+1) ?

;1.2.2
(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))




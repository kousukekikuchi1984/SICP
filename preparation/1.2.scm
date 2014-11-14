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
;木構造再帰プロセス
(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))
; この方法だと、計算量は木構造の節の数に比例するので、n^2

(define (fib n)
  (fib-iter 1 0 n))

(define (fib-iter a b count)
  (if (= count 0)
    b
    (fib-iter (+ a b) a (- count 1))))

;; countで何回繰り返したかを計算 aとbで値を保存
;; 計算量はn倍になる
;; 木構造は問題を考える上でシンプルになる。ただ、それを普通に適応すると、計算量が必要になる
;; しかし、計算量を抑える効率的な設計にすれば良い。

(define (count-change amount)
  (cc amount 5))

(define (cc amount kind-of-coins)
  cond ((= amount 0) 1)
       ((or (< amount 0) (= kind-of-coins 0)) 0)
       (else (+ (cc amount (- kind-of-coins 1))
                (cc (- amount (first-denomination kind-of-coins))
                    kind-of-coins))))

(define (first-denomination kind-of-coins)
  (cond ((= kind-of-coins 1) 1)
        ((= kind-of-coins 2) 5)
        ((= kind-of-coins 3) 10)
        ((= kind-of-coins 4) 25)
        ((= kind-of-coins 5) 50)))


(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

;; 1.11
(define (func n)
  (cond (>= n 3)
        (+ (func (- n 1) (* 2 (func (- n 2)) (* 3 (func ((- n 3)))))))))

(define (func n)
  (func-iter 1 1 1 n 0))

(define (func-iter n-1 n-2 n-3 n state)
  (if (= state n)
    n-1
    (func-iter (+ n-1 (* 2 n-2) (* 3 n-3)) n-1 n-2 n (+ state 1))))
;;

;1.12

(define (pascal n x)
  (if (or (< n 3) (= x 1) (= x n))
    1
    (+ (pascal (- n 1) (+ x 1)) (pascal (- n 1) x))))



;; 1.13  ここにはかかない
;; fn = fai-n + kai-nとして定義
;; またfai-2 とkai-2をあらかじめ計算して、帰納法に流し込む
;;
;;
;;
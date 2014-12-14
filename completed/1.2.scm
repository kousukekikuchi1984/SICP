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

(+ 4 5)
(inc (+ 3 5))
(inc (inc (+ 2 5)))
(inc (inc (inc (+ 1 5))))
(inc (inc (inc (inc (+ 0 5)))))
(inc (inc (inc (inc 5))))
(inc (inc (inc 6)))
(inc (inc 7))
(inc 8)
9


(+ 4 5)
(+ 3 6)
(+ 2 7)
(+ 1 8)
(+ 0 9)
9

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
(define (h n) (A 2 n))  ; h(n) = 2^h(n-1)

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
        (+ (func (- n 1) (* 2 (func (- n 2))) (* 3 (func ((- n 3)))))
        (else n))))

(define (func n)
  (if (< n 3)
    n
    (+ (func (- n 1))
       (* 2 (func (- n 2)))
       (* 3 (func (- n 3))))))

(define (func n)
  (func-iter 1 1 1 n 0))

(define (func-iter n-1 n-2 n-3 n state)
  (if (= state n)
    n-1
    (func-iter (+ n-1 (* 2 n-2) (* 3 n-3)) n-1 n-2 n (+ state 1))))



(define (func n)
  (if (< n 3)
    n
    (func-iter n s1 s2 s3 iter)))
(define (func-iter n s1 s2 s3 iter)
  (if(= n iter)
    s1
    (func-iter n (func-iter n s1 s2 s3 iter))))



;give up
;
(define (f x)
  (define (iter p pp ppp count)
    (if (< count 3)
        p
        (iter (+ p (* 2 pp) (* 3 ppp)) p pp (- count 1))))
  (if (< x 3)
    x
    (iter (- x 1) (- x 2) (- x 3) x)))

(define (f x)
  (define (iter p pp ppp index limit)
    (if (> index limit)
        p
        (iter (+ p (* 2 pp) (* 3 ppp)) p pp (+ index 1) limit)))
  (if (< x 3)
    x
    (iter 2 1 0 3 x)))
;;

;1.12

(define (pascal n x)
  (if (or (< n 3) (= x 1) (= x n))
    1
    (+ (pascal (- n 1) (+ x 1)) (pascal (- n 1) x))))



;; 1.13  ここにはかかない
;; fn = fai-n + kai-nとして定義
;; またfai-2 とkai-2をあらかじめ計算して、帰納法に流し込む
;; また、ø=(1-√5)/√5 は|ø| < 1なので、ø^nにすると0に収束する
;;

;; 1.2.3
;;
;;
;;1.14
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
; count-change 11
;   cc amount 11
;
;
(count-change 11)
(cc 11 5)
(+ (cc 11 4) (cc -39 5))
(+ (cc 11 3) (cc -14 4))
(+ (cc 11 2) (cc  1 4))
(+ (cc 11 1) (cc 6 2) (+ (cc 1 3) (cc -24 4)))
(+ (cc 11 0) (cc 10 1) (cc 6 1) (cc 1 2) (cc -9 3))       ;cc 6 1じゃなくcc 6 2だね
(+ 0 (cc 10 0) (cc9 1) (cc6 0) (cc 5 1) (cc 1 1))
(+ 0 0 (cc 9 0) (cc 8 1) 0 (cc 5 0) (cc 4 1) (cc 1 0) (cc 0 1))
(+ 0 0 0 (cc 7 1) (cc 8 0) 0 0 (cc 4 0) (cc 3 1) 1 0)
(+ 0 0 0 (cc 7 0) (cc 6 1) 0 0 0 0 (cc 3 0) (cc 2 1) 1 0)
(+ 0 0 0 0 (cc 6 0) (+ 5 1) 0 0 0 0 0 (cc 2 0) (cc 1 1) 1 0)
(+ 0 0 0 0 0 (cc 5 0) (cc 4 1) 0 0 0 0 0 0 (cc 1 0) (cc 0 1) 1 0)
(+ 0 0 0 0 0 0 (cc 4 0) cc(3 1) 0 0 0 0 0 0 0 1 1 0)
(+ 0 0 0 0 0 0 0 (cc 3 0) (cc 2 1) 0 0 0 0 0 0 0 1 1 0)
(+ 0 0 0 0 0 0 0 0 (cc 2 0) (cc 1 1) 0 0 0 0 0 0 0 1 1 0)
(+ 0 0 0 0 0 0 0 0 0 (cc 1 0) (cc 1 0) 0 0 0 0 0 0 1 1 0)
(+ 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 1 1 0)
;; どっかで間違えてる


; 1.15
;
(define (cube x) (* x x x))

(define (p x) (- (* 3 x) (* 4 (cube x))))

(define (sine angle)
  (print angle)
  (if (not (> (abs angle) 0.1))
    angle
    (p (sine (/ angle 3.0)))))

; 7回
;  √√ø  わからない。


; 1.2.4
(define (expt b n)
  (if (= n 0)
    1
    (* b (expt b (- n 1)))))


(define (expt b n)
  (expt-iter b n 1))

(define (expt-iter b counter product)
  (if (= counter 0)
    product
    (expt-iter b (- counter 1) (* b product))))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (square x)
  (* x x))


(define (even? n)
  (= (remainder n 2) 0))

; 1.16
(define (fast-expt b n a)
  (* a (fast-expt b (- n 1) (* a b))))


(define (fast-expt b n a)
  (if (= n 0)
    a
    (fast-expt b (- n 1) (* a b))))

(define (fast-expt b n a)
  (cond ((= n 0) a)
        ((even? n) (fast-expt (* b b) (- n 1) a))
        (else (fast-expt b (- n 1) (* a b)))))

; 偶数時bとnをおきかえる
; 奇数時にaを更新
; n = ０の時にaが求める値となる

; 1.17
(define (* a b)
  (if (= b 0)
    0
    (* a (* a (- b 1)))))

(define (fast-times a b)
  (times a b 0))

(define (times a b state)
  (cond ((= b 0) 0)
        ((even? b) (times (double a) (halve b) state))
        (else (times (double a) (halve (- b 1) (+ state a)))))
;; 対数的ステップ数
(define (times a b)
  (if (= b 0)
    a
    (+ a (times a (- b 1)))))

(define (times a b)
  (print a b)
  (cond ((= b 0) a)
        (even? b) (times (double a) (halve b))
        (else (+ a (times a (- b 1))))))

; 1.18
(define (fast-times a b)
  (times a b 0))

(define (double x)
  (* x 2))

(define (halve x)
  (/ x 2))

(define (times a b state)
  (cond ((= b 0) state)
        ((even? b) (times (double a) (halve b) state))
        (else (times (double a) (halve (- b 1)) (+ state a)))))

;; 反復
; 1.19
;
; maxtrixを作る、TpnTpnを計算して、an+1とbn+1の値を計算

(define (fib n)
  (fib-iter 1 0 0 1 n))


(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (square p) (square q))
                   (+ (* 2 p q) (square q))
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

(define (square x)
  (* x x))

; 1.2.5
; Euclidの互除法
(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (remainder a b))))

; 1.20
;
; 訳がわからないよ／人◕‿‿◕人＼

(define (gcd a b)
  (print a)
  (if (= b 0)
    a
    (gcd b (remainder a b))))

(gcd 206 40)

;; step 1
(if (= 40 0)
  206
  (gcd 40 (remainder 206 40)))

(gcd 40 (remainder 206 40))
(if (= (remainder 206 40) 0)
  40
  (gcd (remainder 206 40) (remainder 40 (remainder 206 40))))

(gcd 40 (remainder 206 40))
(if (= 6 0)
  40
  (gcd (remainder 206 40) (remainder 40 (remainder 206 40)))) ;;

(if (= (remainder 40 (remainder 206 40) 0))
  (remainder 206 40)
  (gcd (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))
;; 書くのがめんどい



(define (p) (p))

(define (test x y)
  if (= x 0)
  0
  y)
(test 0 (p))


; 1.2.6
(define (smallest-divisor n)
  (find-divisor n 2))


(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))


(define (divides? a b)
  (= (remainder b a) 0))


(define (prime? n)
  (= n (smallest-divisor n)))


(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
          (remainder (* base (expmod base (- exp 1) m))
                     m))))


; 1.21
; 最小で割れる数字を抜き出す方法を考える
; 割り切れた場合、数字をstateとして記す。最後に返す
;
(define (smallest-divisor n)
  (find-divisor n n n))

(define (find-divisor n test state)
  (cond ((= test 1) state)
        ((divides? test n) (find-divisor n (- test 1) test))
        (else (find-divisor n (- test 1) state))))

(define (divides? a b)
  (= (remainder b a) 0))

;gosh> (smallest-divisor 199)
;199
;gosh> (smallest-divisor 1999)
;1999
;gosh> (smallest-divisor 19999)
;7

;1.22

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
    (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))
(define (search-for-primes a b)
  (search-for-primes-iter a b))

(define (search-for-primes-iter a b)
  (if (< a b)
    (and (if (prime? a)
           (timed-prime-test a))
           (search-for-primes-iter (+ a 1) b))))

(define (runtime)
  (use srfi-11)
  (let-values (((a b) (sys-gettimeofday)))
              (+ (* a 1000000) b)))


(define (smallest-divisor n)
  (find-divisor n 2))


(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (square x)
  (* x x))

(define (prime? n)
  (= n (smallest-divisor n)))

;;  (search-for-primes 1000 1100)
;;
;;  1009 *** 10
;;  1013 *** 10
;;  1019 *** 10
;;  1021 *** 10
;;  1031 *** 10
;;  1033 *** 10
;;  1039 *** 11
;;  1049 *** 10
;;  1051 *** 10
;;  1061 *** 10
;;  1063 *** 10
;;  1069 *** 10
;;  1087 *** 10
;;  1091 *** 13
;;  1093 *** 10
;;  1097 *** 10#<undef>
;;  (search-for-primes 10000 10100)
;;
;;  10007 *** 32
;;  10009 *** 31
;;  10037 *** 30
;;  10039 *** 30
;;  10061 *** 37
;;  10067 *** 38
;;  10069 *** 27
;;  10079 *** 28
;;  10091 *** 27
;;  10093 *** 28
;;  10099 *** 27#<undef>
;;  (search-for-primes 100000 100100)
;;
;;  100003 *** 99
;;  100019 *** 99
;;  100043 *** 85
;;  100049 *** 85
;;  100057 *** 97
;;  100069 *** 132#<undef>
;;  (search-for-primes 1000000 1000100)
;;
;;  1000003 *** 342
;;  1000033 *** 370
;;  1000037 *** 295
;;  1000039 *** 316
;;  1000081 *** 273
;;  1000099 *** 353#<undef>
;;  10^(1/2)になっている
;;


; 1.23
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
    (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))
(define (search-for-primes a b)
  (search-for-primes-iter a b))

(define (search-for-primes-iter a b)
  (if (< a b)
    (and (if (prime? a)
           (timed-prime-test a))
           (search-for-primes-iter (+ a 1) b))))

(define (runtime)
  (use srfi-11)
  (let-values (((a b) (sys-gettimeofday)))
              (+ (* a 1000000) b)))


(define (divides? a b)
  (= (remainder b a) 0))

(define (square x)
  (* x x))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (next n)
  (if (= n 2)
    3
    (+ n 2)))
; 二倍の速度にはならない。計算回数が半分になるものの、増加の程度はroute(n)であるので、
; 実行時間は1/route(2)になる。  appendix: route(2) = 1.414
;
;
;1.24
;f(1000000)/f(1000) = 2 であるはず（logで増加するため

; fast prime
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else #f)))

(define (random x)
  (modulo (sys-random) x))

(define (square x)
  (* x x))

;; timed-prime-test
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 1000)
    (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))
(define (search-for-primes a b)
  (search-for-primes-iter a b))

(define (search-for-primes-iter a b)
  (if (< a b)
    (and (if (prime? a)
           (timed-prime-test a))
           (search-for-primes-iter (+ a 1) b))))

(define (runtime)
  (use srfi-11)
  (let-values (((a b) (sys-gettimeofday)))
              (+ (* a 1000000) b)))

; 1013:       6038
; 100000037: 13043
;            1:2 ちょい

; 1.25
(define (expmod base exp m)
  (remainder (fast-expt base exp) m))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))


(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
          (remainder (* base (expmod base (- exp 1) m))
                     m))))

;; 新しいコードは古いコードと同じように動くと思われる
;; 1013: 72399
;  100000037: 動かん
;  上の式では、すべての値を再帰的に計算してから評価しているが下の式では、
;  余りを繰り返し評価している。
;

; 1.26
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (* (expmod base (/ exp 2) m)
                       (expmod base (/ exp 2) m))
                    m))
        (else
          (remainder (* base (expmod base (- exp 1) m))
                     m))))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
          (remainder (* base (expmod base (- exp 1) m))
                     m))))
; expmodが二回再起的に呼び出されているため。ここが呼び出されると、計算回数を倍増しているため
;

; 1.27

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermat-test n)
 (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else #f)))

(define (try-it a n)
  (= (expmod a n n) a))

(define (random x)
  (modulo (sys-random) x))

(define (square x)
  (* x x))

(define (carmichael n)
  (define (try-it a)
    (= (expmod a n n) a))
  (define (carmichael-check b)
    (if (= b 1)
      #t
      (and (try-it b)
           (carmichael-check (- b 1)))))
  (carmichael-check (- n 1)))

;; gosh> (carmichael 561)
;; #t
;; gosh> (carmichael 1105)
;; #t
;; gosh> (carmichael 1729)
;; #t
;; gosh> (carmichael 2465)
;; #t
;; gosh> (carmichael 2821)
;; #t
;; gosh> (carmichael 6601)
;; #t
;;

;1.28 millar-rabin test
;どうやら文章を数式化すると
; x ≠ 1
; x ≠ n - 1
; x^2 ≡ 1 (mod n)

(define (millar-rabin x n)
  (and (not (= x 1))
       (not (= x (- n 1)))
       (= (remainder (* x x) n) 1)))


; 判定条件を増やす
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((millar-rabin base m) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

;gosh> (carmichael 561)
;#f
;gosh> (carmichael 1105)
;#f
;gosh> (carmichael 1729)
;#f
;gosh> (carmichael 2465)
;#f
;gosh> (carmichael 2821)
;#f
;gosh> (carmichael 6601)
;#f


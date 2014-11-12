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



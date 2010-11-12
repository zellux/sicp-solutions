(define (cubic-sum x y)
  (+ (* x x x) (* y y y)))

(define (calc-weight x)
  (cubic-sum (car x) (car (cdr x))))

(define (filter-ramanujan s)
  (let ((s1 (calc-weight (stream-car s)))
         (s2 (calc-weight (stream-car (stream-cdr s)))))
     (if (= s1 s2)
         (cons-stream s1
                      (filter-ramanujan (stream-cdr (stream-cdr s))))
         (filter-ramanujan (stream-cdr s)))))
         
(stream-head
 (filter-ramanujan
  (weighted-pairs integers
                  integers
                  calc-weight))
 6)


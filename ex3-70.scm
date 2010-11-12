(define (merge-weighted s1 s2 func)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< (func s1car) (func s2car))
                  (cons-stream s1car (merge-weighted (stream-cdr s1) s2 func)))
                 ((> (func s1car) (func s2car))
                  (cons-stream s2car (merge-weighted s1 (stream-cdr s2) func)))
                 (else
                  (cons-stream s1car
                               (merge-weighted (stream-cdr s1)
                                               (stream-cdr s2)
                                               func))))))))

(display-stream
 (merge-weighted
  (stream-enumerate-interval 1 10)
  (stream-enumerate-interval 30 40)
  (lambda (x) (- 0 x))))

(define (weighted-pairs s t func)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (weighted-pairs (stream-cdr s) (stream-cdr t) func)
    func)))

(untrace stream-enumerate-interval)
(trace weighted-pairs)
(trace merge-weighted)
(trace calc-weight)
(untrace weighted-pairs)
(untrace merge-weighted)
(untrace calc-weight)

(define (calc-weight x)
  (+ (car x) (car (cdr x))))

(display-stream
 (weighted-pairs
  (stream-enumerate-interval 0 5)
  (stream-enumerate-interval 0 5)
  calc-weight))



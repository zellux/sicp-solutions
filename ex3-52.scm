(define sum 0)

(define (accum x)
  (set! sum (+ x sum))
  sum)

(define seq (stream-map accum (stream-enumerate-interval 1 20)))
(display-line sum)
;; 1

(define y (stream-filter even? seq))
(display-line sum)
;; 3

(define z (stream-filter (lambda (x) (= (remainder x 5) 0))
                         seq))
(display-line sum)
;; 10

(stream-ref y 7)
(display-line sum)
;; 136

(display-stream z)
(display-line sum)
;; 210
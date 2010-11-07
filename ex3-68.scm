(define (pairs s t)
  (interleave
   (stream-map (lambda (x) (list (stream-car s) x))
               t)
   (pairs (stream-cdr s) (stream-cdr t))))

;; infinite loop
(display-stream (pairs (stream-enumerate-interval 0 5) (stream-enumerate-interval 0 5)))

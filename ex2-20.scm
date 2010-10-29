(define (same-parity x y . z)
  (define (append-same-parity x xs)
    (if (null? xs)
      xs
      (if (= 0 (remainder (- x (car xs)) 2))
        (cons (car xs) (append-same-parity x (cdr xs)))
        (append-same-parity x (cdr xs)))))
  (cons x (append-same-parity x (cons y z))))

(same-parity 1 2 3 4 5 6 7)

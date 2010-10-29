(define (deep-reverse l)
  (cond ((null? l) l)
        ((not (pair? l)) l)
        (else (reverse (map deep-reverse l)))))

(deep-reverse (list (list 1 2) (list 3 4)))

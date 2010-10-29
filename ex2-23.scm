(define (for-each op items)
  (cond ((null? items) 't)
        (else (op (car items))
              (for-each op (cdr items)))))

(for-each (lambda (x) (newline) (display x))
          (list 57 321 88))

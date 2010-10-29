(define (partial-sum integers)
  (cond ((stream-null? integers) the-empty-stream)
        ((stream-null? (stream-cdr integers)) (stream-car integers))
        (else (cons-stream (stream-car integers)
                           (add-streams (stream-cdr integers)
                                        (partial-sum integers))))))

(define testlist
  ;; (cons-stream 1
  ;;              (cons-stream 2
  ;;                           (cons-stream 4 6))))
                            
(display-stream (partial-sum (stream-enumerate-interval 1 20)))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

(stream-for-each
 display-line
 (stream-map +
             (stream-enumerate-interval 1 3)
             (stream-enumerate-interval 10 20)
             (stream-enumerate-interval 700 800)))

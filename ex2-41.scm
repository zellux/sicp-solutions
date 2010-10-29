(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))
   
(define (find-triple n s)
  (filter (lambda (triple)
            (and (not (= (car triple) (cadr triple)))
                 (not (= (car triple) (caddr triple)))
                 (not (= (cadr triple) (caddr triple)))
                 (= (+ (car triple) (cadr triple) (caddr triple)) s)))
          (flatmap (lambda (i)
                     (flatmap (lambda (j)
                                (map (lambda (k) (list i j k))
                                     (enumerate-interval 1 n)))
                              (enumerate-interval 1 n)))
                   (enumerate-interval 1 n))))

(find-triple 4 6)


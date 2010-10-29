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

(define (safe? k positions)
  ;; Test wheter (x1, y1) collide with (x2, y2), here we assume (x1 != x2)
  (define (collide? x1 y1 x2 y2)
    (or (= y1 y2)
        (= (+ x1 y1) (+ x2 y2))
        (= (- x1 y1) (- x2 y2))))
  (define (col-safe? col other-pos)
    (if (null? other-pos)
        #t
        (and (not (collide? col (car other-pos) k (car positions)))
             (col-safe? (- col 1) (cdr other-pos)))))
  (col-safe? (- k 1) (cdr positions)))

(define (adjoin-position new-row k rest-of-queens)
  (append (list new-row) rest-of-queens))

(define empty-board
  '())

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(safe? 4 (list 3 1 4 2))                ;Test fuction safe?

(adjoin-position 2 1 (adjoin-position 1 3 empty-board))
                 
(queens 8)

(RESTART 1)
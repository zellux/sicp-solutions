#lang scheme

(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

(define (split op1 op2)
  (define (draw painter n)
    (if (= n 0)
        painter
        (let ((smaller (draw painter (- n 1))))
          (op1 painter (op2 smaller smaller)))))
  draw)
    

(define right-split (split beside below))
(define up-split (split below beside))
 
(define (
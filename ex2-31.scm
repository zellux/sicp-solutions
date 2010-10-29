(define (tree-map func tree)
  (cond ((null? tree) tree)
        ((not (pair? tree)) (func tree))
        (else (map (lambda (x) (tree-map func x)) tree))))

(define (square-tree tree)
  (tree-map square tree))

(square-tree
  (list 1
        (list 2 (list 3 4) 5)
        (list 6 7)))

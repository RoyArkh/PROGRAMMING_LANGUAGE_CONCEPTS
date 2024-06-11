(define (flatten-tree tree)
  (define (flatten-subtree subtree)
    (cond
      ((null? subtree) '())
      ((not (pair? subtree)) (list subtree))
      (else (fold-right append '() (map flatten-subtree subtree)))))
  (fold-right append '() (map flatten-subtree tree)))
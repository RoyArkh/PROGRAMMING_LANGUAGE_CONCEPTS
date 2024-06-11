#lang scheme

(define helper (lambda (elem n)
                 (if (<= n 0)
                     '()
                     (cons elem (helper elem (- n 1)))
                     )))
                 

(define repeatN (lambda (lst n)
                  (if (null? lst)
                      '()
                      (append (helper (car lst) n) (repeatN (cdr lst) n))
                      )))


(define (reduce op init lst)
  (if (null? lst)
      init
      (reduce op (op init (car lst)) (cdr lst))))
(define (sum-even lst)
   (reduce + 0 (filter even? lst)))




(define isIn? (lambda (lst x)
                (if (null? lst)
                    #f
                    (if (= (car lst) x)
                        #t
                        (isIn? (cdr lst) x)
                        ))))

(define (isIn?-nested lst n)
  (cond
    ((null? lst) #f)
    ((pair? (car lst)) (or (isIn?-nested (car lst) n) (isIn?-nested (cdr lst) n)))
    ((list? (car lst)) (isIn?-nested (car lst) n))
    ((= (car lst) n) #t)
    (else (isIn?-nested (cdr lst) n))))

(define (myProc pred lst1 lst2 op init)
  (foldl op init (filter
                  (lambda (e) (pred lst2 e))
                  lst1)))







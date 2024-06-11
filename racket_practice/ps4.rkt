#lang scheme

(define (empty-list)
  (lambda (mode)
    (display "end of list")))

(define (prepend-list a lst)
  (lambda (mode)
    (if mode
        a
        lst)))

(define (car-list lst)
  (lst #t))

(define (cdr-list lst)
  (lst #f))

(define x
  (prepend-list 13
                (prepend-list 3
                              (prepend-list 6
                                            (prepend-list 7
                                                          (empty-list))))))



(define (find-min lst)
  (cond ((null? lst) #f)
        ((= (length lst) 1) (car lst))
        ((> (car lst) (car (cdr lst))) (find-min ((cdr lst))))
        (else (find-min (remove (car (cdr lst)) lst)))
  )
)


(define (remove-n-times elem lst n)
  (cond ((null? lst) '())
        ((= n 0) lst)
        (else (remove-n-times elem (remove elem lst) (- n 1)))
  )
)


(define (count-occurrence-nested lst x)
  (cond ((null? lst) 0)
        ((pair? (car lst)) (+ (count-occurrence-nested (car lst) x)
                              (count-occurrence-nested (cdr lst) x)))
        ((equal? (car lst) x) (+ 1 (count-occurrence-nested (cdr lst) x)))
        (else (count-occurrence-nested (cdr lst) x))))

(count-occurrence-nested '(a b (a b (a b)) (a b)) 'a)

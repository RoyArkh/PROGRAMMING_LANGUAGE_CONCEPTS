#lang scheme

(define lengths (lambda (lst)
                  (if (null? lst)
                      '()
                      (cons (string-length (car lst)) (lengths (cdr lst)))
                      )))

(define total-length
  (lambda (lst)
    (if (null? lst)
        0
        (+ (string-length (car lst)) (total-length (cdr lst))))))

(define string-filter (lambda (lst)
                        (if (null? lst)
                            '()
                            (if (string? (car lst))
                                (cons (car lst) (string-filter (cdr lst)))
                                (string-filter (cdr lst))
                                ))))

(define string-em-up (lambda (lst) 
                       (if (null? lst)
                           0
                           (total-length (string-filter lst)))))


(string-em-up (list "I" "really" "like" 3.1416))

#lang scheme

(define match (lambda (pattern text)
                (helper pattern pattern text 0)))



(define helper (lambda (patternSoFar pattern text n)
                 (if (null? text)
                     n
                     (if (equal? (car patternSoFar) (car text))
                         (helper (if (null? (cdr patternSoFar))
                                     pattern
                                     (cdr patternSoFar))
                                 pattern
                                 (cdr text)
                                 (if (null? (cdr patternSoFar))
                                     (+ 1 n)
                                     n))
                         (helper patternSoFar pattern (cdr text) n))
                     )
                     ))


(define helper* (lambda (patternSoFar pattern text n)
                 (if (null? text)
                     n
                     (if (equal? (car patternSoFar) '*)
                         (helper* (if (null? (cdr patternSoFar))
                                     pattern
                                     (cdr patternSoFar))
                                 pattern
                                 (cdr text)
                                 (if (null? (cdr patternSoFar))
                                     (+ 1 n)
                                     n))
                         (if (equal? (car patternSoFar) (car text))
                             (helper* (if (null? (cdr patternSoFar))
                                         pattern
                                         (cdr patternSoFar))
                                     pattern
                                     (cdr text)
                                     (if (null? (cdr patternSoFar))
                                         (+ 1 n)
                                         n))
                             (helper* patternSoFar pattern (cdr text) n))
                         )
                     )
                     ))

(define match* (lambda (pattern text)
                 (helper* pattern pattern text 0)))
                 



(define *test* '(a b c a b a b a b c b c b c a b c))

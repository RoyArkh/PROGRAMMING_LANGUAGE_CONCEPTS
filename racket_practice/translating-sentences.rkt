#lang scheme

(define memq (lambda (word text)
               (if (null? text)
                   #f
                   (if (symbol=? (car text) word)
                       text
                       (memq word (cdr text))
                       ))))
                   
(define lookup (lambda (word dictionary)
                 (if (or (null? word) (null? dictionary))
                   #f
                   (if (equal? (car (car dictionary)) word)
                       (car (cdr (car dictionary)))
                       (lookup word (cdr dictionary))
                       ))))

(define *english-to-french*
        '((cat chat) (cake gateau) (present cadeau) (I je) (eat mange) 
          (the le)))


(define translate (lambda (sentence dictionary)
                    (map (lambda (word) (lookup word dictionary)) sentence)))

(translate '(I eat the cat) *english-to-french*)

  
#lang scheme
(require rnrs/mutable-pairs-6)

(define last-cons (lambda (l)
                    (if (null? l)
                        '()
                        (if (null? (cdr l))
                            l
                            (last-cons (cdr l)))
                        )
                    ))

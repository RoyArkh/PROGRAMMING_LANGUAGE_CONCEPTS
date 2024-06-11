#lang scheme

(define fill-queue! (lambda (queue elts)
                      (if (null? elts)
                          queue
                          (fill-queue! (insert-queue! queue (car elts)) (cdr elts))
                          )))

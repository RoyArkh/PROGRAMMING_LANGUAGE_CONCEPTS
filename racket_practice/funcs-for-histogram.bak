#lang scheme

(define in-histogram? 
  (lambda (word histogram)
    (and (pair? histogram)
	 (or (eq? word (first (car histogram)))
	     (in-histogram? word (cdr histogram))))))

(define add-new-word-to-histogram
  (lambda (word histogram)
    (cons (list word 1) histogram)))

(define increment-word-count-in-histogram (lambda (word histogram)
                                            (map (lambda (entry)
                                                   (if (equal? (car entry) word)
                                                       (list word (+ (car (cdr entry)) 1))
                                                       entry))
                                                 histogram)))





(define add-words-to-histogram (lambda (words histogram)
                                 (if (null? words)
                                     histogram
                                     (if (in-histogram? (car words) histogram)
                                         (add-words-to-histogram (cdr words) (increment-word-count-in-histogram (car words) histogram))
                                         (add-words-to-histogram (cdr words) (add-new-word-to-histogram (car words) histogram))
                                         ))))


(define times-occuring (lambda (histogram word)
                         (if (null? histogram)
                             0
                             (if (equal? (car (car histogram)) word)
                             (car (cdr (car histogram)))
                             (times-occuring (cdr histogram) word))
                             )))


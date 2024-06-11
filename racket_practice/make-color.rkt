#lang scheme

(define round-to-integer
  (lambda (x)
    (if (>= (- x (floor x)) 0.5)
        (ceiling x)
        (floor x))))

(define make-color
  (lambda (red-amt green-amt blue-amt)
    (list (restrict red-amt)
	  (restrict green-amt)
	  (restrict blue-amt))))

(define restrict
  (lambda (x)
    (cond ((< x 0) 0)
	  ((> x 255) 255)
	  (else (round-to-integer x)))))



(define half-as-bright
  (lambda (color)
    (map (lambda (val) (/ val 2)) color)))

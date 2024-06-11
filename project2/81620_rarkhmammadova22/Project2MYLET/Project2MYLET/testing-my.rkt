#lang eopl

;; interpreter for the LET language.  The \commentboxes are the
;; latex code for inserting the rules into the code in the book.
;; These are too complicated to put here, see the text, sorry.

(require "lang.rkt")
(require "data-structures.rkt")
(require "environments.rkt")


(provide value-of-program value-of)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

;; value-of-program : Program -> ExpVal
;; Page: 71
(define value-of-program 
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
                 (value-of exp1 (init-env))))))

;; value-of : Exp * Env -> ExpVal
;; Page: 71
(define value-of
  (lambda (exp env)
    (cases expression exp
      
      (const-exp (num) (num-val num))

      (var-exp (var) (apply-env env var))

      (op-exp (exp1 exp2 op)
              (let ((val1 (value-of exp1 env))
                    (val2 (value-of exp2 env)))
                  (let ((num1 (expval->rational val1))
                        (num2 (expval->rational val2)))
                      (cond 
                        ((and (number? num1) (number? num2))
                          (num-val
                            (cond 
                              ((= op 1) (+ num1 num2))
                              ((= op 2) (* num1 num2))
                                    ;; -----------------------
                                    ;; INSERT YOUR CODE HERE 
                                    ;; -----------------------
                              ((= op 3) (/ num1 num2))
                              (else (- num1 num2))
                                    ;; -----------------------
                              )))
                        
                        ((and (number? num1) (not (number? num2)))
                          (rational-val
                          (let ((num2top (car num2))
                                (num2bot (cdr num2)))
                            (cond 
                              ((= op 1) (cons (+ (* num1 num2bot) num2top) num2bot))
                              ((= op 2) (cons (* num1 num2top) num2bot))
                              ;; -----------------------
                              ;; INSERT YOUR CODE HERE 
                              ;; -----------------------
                              ((= op 3) (cons (* num1 num2bot) (num2top)))
                              (else (cons (- (* (* num1 num1) num2bot) (* num2top num1)) (* num1 num2bot)))

                              ;; -----------------------

                              
                              ))))

                        ((and (number? num2) (not (number? num1)))
                          (rational-val
                          (let ((num1top (car num1))
                                (num1bot (cdr num1)))
                            (cond 
                              ((= op 1) (cons (+ (* num1bot num2) num1top) num1bot))
                              ((= op 2) (cons (* num1top num2) num1bot))
                              ;; -----------------------
                              ;; INSERT YOUR CODE HERE 
                              ;; -----------------------
                              ((= op 3) (cons (* num2 num1bot) (num1top)))
                              (else (cons (- (* (* num2 num2) num1bot) (* num1top num2)) (* num2 num1bot)))
                              ;; -----------------------
                              ))))

                        (else
                          (rational-val
                          (let ((num1top (car num1))
                                (num1bot (cdr num1))
                                (num2top (car num2))
                                (num2bot (cdr num2)))
                            (cond 
                              ((= op 1) (cons (+ (* num1top num2bot) (* num1bot num2top)) (* num1bot num2bot))) ;; add
                              ((= op 2) (cons (* num1top num2top) (* num1bot num2bot))) ;; multiply
                              ;; -----------------------
                              ;; INSERT YOUR CODE HERE 
                              ;; -----------------------
                              ((= op 3) (cons (* (num1top num2bot)) (* num1bot num2top)))
                              (else (cons (- (* num1top num2bot) (* num2top num1bot)) (* num1bot num2bot)))


                              ;; ----------------------- 
                            ))))))))

     

      (cons-exp (exp1 exp2)
                (let ((val1 (value-of exp1 env))
                      (val2 (value-of exp2 env)))
                  (let ((num1 (expval->num val1))
                        (lst1 (expval->list val2)))
                    (if (number? num1)
                        (list-val (cons num1 lst1))
                        (list-val lst1))
                    )
                  )
                )
      
      (mul-exp (exp1)
               (let ((val1 (value-of exp1 env)))
                 (let ((lst1 (expval->list val1)))
                   (mul-help lst1)
                   )))

      (min-exp (exp1)
               (let ((val1 (value-of exp1 env)))
                 (let ((lst1 (expval->list val1)))
                   (if (null? lst1)
                       -1
                       (min-help lst1 (car lst1))
                       )
                   )))

      (if-elif-exp (exp11 exp12 exp21 exp22 exp3)
                   (let ((val11 (value-of exp11 env))
                         (val12 (value-of exp12 env))
                         (val21 (value-of exp21 env))
                         (val22 (value-of exp22 env))
                         (val3 (value-of exp3 env)))
                     (cond (val11
                            val12)
                           (val21
                            val22)
                           (else val3))
                     ))


      (rational-exp (num1 num2)
                    (if (= 0 num2)
                        eopl:error 
                        (rational-val (cons num1 num2))
                        ))

      

      

      (list-exp (list-val empty-list))

      )))


(define mul-help (lambda (lst)
                        (if (null? lst)
                            0
                            (* (car lst) (if (null? (cdr lst))
                                              1
                                              (mul-help (cdr lst)))
                               ))))

(define min-help (lambda (lst currentmin)
                   (if (null? lst)
                       currentmin
                       (if (<= (car lst) currentmin)
                           (min-help (cdr lst) (car lst))
                           (min-help (cdr lst) currentmin))
                       )))
                            
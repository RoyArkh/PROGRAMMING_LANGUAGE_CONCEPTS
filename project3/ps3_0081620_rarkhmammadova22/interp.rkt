#lang eopl

;; interpreter for the PROC language, using the procedural
;; representation of procedures.



(require "lang.rkt")
(require "data-structures.rkt")
(require "environments.rkt")

(provide value-of-program value-of)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

;; run : String -> ExpVal
(define run
  (lambda (s)
    (value-of-program (scan&parse s))))

;; value-of-program : Program -> ExpVal
(define value-of-program 
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
                 (value-of exp1 (init-env))))))

;; value-of : Exp * Env -> ExpVal
(define value-of
  (lambda (exp env)
    (cases expression exp
      
      
      (const-exp (num) (num-val num))
      
      
      (var-exp (var) (apply-env env var))
      
     
      (diff-exp (exp1 exp2)
                (let ((val1 (value-of exp1 env))
                      (val2 (value-of exp2 env)))
                  (let ((num1 (expval->num val1))
                        (num2 (expval->num val2)))
                    (num-val
                     (- num1 num2)))))
      
      
      (zero?-exp (exp1)
                 (let ((val1 (value-of exp1 env)))
                   (let ((num1 (expval->num val1)))
                     (if (zero? num1)
                         (bool-val #t)
                         (bool-val #f)))))
      
     
      (if-exp (exp1 exp2 exp3)
              (let ((val1 (value-of exp1 env)))
                (if (expval->bool val1)
                    (value-of exp2 env)
                    (value-of exp3 env))))
      
      
      (let-exp (var exp1 body)       
               (let ((val1 (value-of exp1 env)))
                 (value-of body
                           (extend-env var val1 env))))
      
      (proc-exp (var body)
                (proc-val (procedure var body env)))
      
      (call-exp (rator rand)
                (let ((proc (expval->proc (value-of rator env)))
                      (arg (value-of rand env)))
                  (apply-procedure proc arg)))
      
      ;;----------------------------------------------------
      ; INSERT YOUR CODE HERE
      ; Write the required expressions starting from here

      (stack-exp () (stack-val '()))

      (stack-push-exp (exp1 exp2)
                      (let ((val1 (value-of exp1 env))
                            (val2 (value-of exp2 env)))
                        (let ((stack (expval->stack val1))
                              (num (expval->num val2)))
                          (stack-val (cons num stack))
                          )))

      (stack-pop-exp (exp)
                     (let ((val (value-of exp env)))
                       (let ((stack (expval->stack val)))
                         (cond ((null? stack)
                                (display "Empty stack cannot be popped!\n")
                                (stack-val '()))
                               (else
                                (stack-val (cdr stack)))
                               ))))

      (stack-peek-exp (exp)
                      (let ((val (value-of exp env)))
                       (let ((stack (expval->stack val)))
                         (cond ((null? stack)
                                (display "Empty stack cannot be peeked!\n")
                                (num-val 2813))
                               (else
                                (num-val (car stack)))
                               ))))


      (stack-push-multi-exp (exp exps)
                      (let ((stack (expval->stack (value-of exp env))))
                        (let loop ((expressions exps)
                                   (current-stack stack))
                          (if (null? expressions)
                              (stack-val current-stack)
                              (let ((val (value-of (car expressions) env)))
                                (loop (cdr expressions) (cons (expval->num val) current-stack)))))))


      (stack-pop-multi-exp (exp1 num)
                           (let ((val1 (value-of exp1 env)))
                             (let ((stack (expval->stack val1)))
                               (cond ((null? stack)
                                      (display "Empty stack cannot be popped!\n")
                                      (stack-val '()))
                                     ((= num 0)
                                      (stack-val stack))
                                     (else
                                      (let loop ((remaining num) (current-stack stack))
                                        (if (or (<= remaining 0) (null? current-stack))
                                            (stack-val current-stack)
                                            (loop (- remaining 1) (cdr current-stack))))))
                               )))


      (stack-merge-exp (exp1 exp2)
                       (let ((val1 (value-of exp1 env))
                             (val2 (value-of exp2 env)))
                         (let ((stack1 (expval->stack val1))
                               (stack2 (expval->stack val2)))
                           (stack-val (merge-help stack1 stack2))
                           )))
                               
                      

      ;;-------------------------------------------------
      
      )))

;;-----------------------------------------
; INSERT YOUR CODE HERE
; you may use this area to define helper functions
;;-----------------------------------------
           
(define merge-help (lambda (s1 s2)
                     (append (reverse s2) s1)
                     ))
                           

;;-----------------------------------------

(define apply-procedure
  (lambda (proc1 val)
    (cases proc proc1
      (procedure (var body saved-env)
                 (value-of body (extend-env var val saved-env))))))

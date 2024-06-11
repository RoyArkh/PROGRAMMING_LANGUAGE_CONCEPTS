#lang eopl
(require eopl/tests/private/utils)

(require "data-structures.rkt")  ; for expval constructors
(require "lang.rkt")             ; for scan & parse
(require "interp.rkt")           ; for value-of-program

;; run : String -> ExpVal
;; Page: 71
(define run
  (lambda (string)
    (value-of-program (scan&parse string))))

(define equal-answer?
  (lambda (ans correct-ans)
    ;; (equal? (eopl:error "ans " ans))))
    (equal? ans (sloppy->expval correct-ans))))

(define sloppy->expval 
  (lambda (sloppy-val)
    (cond
      ((number? sloppy-val) (num-val sloppy-val))
      ((boolean? sloppy-val) (bool-val sloppy-val))
      ((string? sloppy-val) (str-val sloppy-val))
      ((list? sloppy-val) (list-val sloppy-val)) 
      ((pair? sloppy-val) (rational-val sloppy-val)) ; may not work if you define rational with list instead of cons
      (else
       (eopl:error 'sloppy->expval 
                   "Can't convert sloppy value to expval: ~s"
                   sloppy-val)))))

(define-syntax-rule (check-run (name str res) ...)
  (begin
    (cond [(eqv? 'res 'error)
           (check-exn always? (lambda () (run str)))]
          [else
           (check equal-answer? (run str) 'res (symbol->string 'name))])
    ...))

;;;;;;;;;;;;;;;; tests ;;;;;;;;;;;;;;;;

(check-run

 ;; simple arithmetic
 (positive-const "11" 11)
 (zero "0" 0)
 
 ;; check environment
 (env-i "i" error)
 
 ;; replace these with the values you defined
 (env-x "x" 4)
 (env-y "y" 2)
 (env-z "z" 3)
 
;; simple unbound variables
 (test-unbound-var-1 "foo" error)
 (test-unbound-var-2 "-(x,foo)" error)

;; test dynamic typechecking
 (no-bool-to-diff-1 "op(zero?(0),1, 4)" error)
 (no-bool-to-sum-2 "op(1,zero?(0), 1)" error)
 (no-int-to-if "if 1 then 2 else 3" error)
 (no-string-to-if "if 'gul' then 'sena' else 'altintas'" error)
 
;; simple let
 (simple-let-1 "let x = 3 in x" 3)
 
;; make sure the body and rhs get evaluated
 (eval-let-body "let x = 3 in op(x,1,4)" 2)
 (eval-let-rhs "let x = op(4,1,1) in op(x,1,4)" 4)
 
;; check nested let and shadowing
 (simple-nested-let "let x = 3 in let y = 4 in op(x,y, 4)" -1)
 (check-shadowing-in-body "let x = 3 in let x = 4 in x" 4)
 (check-shadowing-in-rhs "let x = 3 in let x = op(x,1,1) in x" 4)

;; op exps
 (op-test-sum "op(3, 2, 1)" 5)
 (op-test-mult "op(3, 2, 2)" 6)
 (op-test-div "op(3, 2, 3)" 3/2)
 (op-test-diff "op(3, 2, 4)" 1)

 ;; if elif else test
 (if-elif-test1  "if zero?(0) then 1 elif zero?(3) then 2 else 3" 1)
 (if-elif-test2 "if zero?(-5) then 1 elif zero?(0) then 2 else 3" 2)
 (if-elif-test3  "if zero?(0) then 1 elif zero?(0) then 2 else 3" 1)
 (if-elif-test3  "if zero?(2) then 1 elif zero?(4) then 2 else 3" 3)

;; rational number test
 
 (simple-rat-def "(5 / 2)"  (5 . 2))
 (simple-rat-def2 "(5 / 0)" error)

 (rat-op-test-sum1 "op((5 / 2), (3 / 5), 1)" (31 . 10))
 (rat-op-test-sum2 "op((5 / 2), 4, 1)" (13 . 2))
 (rat-op-test-sum3 "op(3, (3 / 5), 1)" (18 . 5))
 
 (rat-op-test-mult1 "op((5 / 2), (3 / 5), 2)" (15 . 10))
 (rat-op-test-mult2 "op((5 / 2), 4, 2)" (20 . 2))
 (rat-op-test-mult3 "op(3, (3 / 5), 2)" (9 . 5))

 (rat-op-test-div1 "op((5 / 2), (3 / 5), 3)" (25 . 6))
 (rat-op-test-div2 "op((5 / 2), 4, 3)" (5 . 8))
 (rat-op-test-div3 "op(3, (3 / 5), 3)" (15 . 3))

 (rat-op-test-diff1 "op((5 / 2), (3 / 5), 5)" (19 . 10))
 (rat-op-test-diff2 "op((5 / 2), 4, -1)" (-3 . 2))
 (rat-op-test-diff3 "op(3, (3 / 5), 10)" (12 . 5))

 (rat-op-test-simpl1 "simpl((10 / 4))" (5 . 2))
 (rat-op-test-simpl2 "simpl((10 / 3))" (10 . 3))
 (rat-op-test-simpl3 "simpl((5 / 25))" (1 . 5))

 (rat-op-test-simpl-mult "simpl(op((5 / 2), 4, 2))" (10 . 1))

 (new-list-test1 "create-new-list()" ())
 (new-list-test2 "cons 5 to create-new-list()" (5))
 (new-list-test3 "cons 6 to cons 5 to create-new-list()" (6 5))
 (new-list-test4 "cons 6 to cons 5 to create-new-list()" (6 5))
 (new-list-test5 "multiplication (cons 6 to cons 5 to create-new-list())" 30)
 (new-list-test6 "multiplication (cons 7 to cons 6 to cons 5 to create-new-list())" 210)
 (new-list-test7 "multiplication (create-new-list())" 0)
 (min-test1 "min (create-new-list())" -1)
 (min-test2 "min (cons 8 to cons 13 to cons 5 to create-new-list())" 5)
 (min-test3 "min (cons 2000 to cons 2000 to cons 2000 to create-new-list())" 2000)
 (min-test4 "min (cons 139 to create-new-list())" 139)

)
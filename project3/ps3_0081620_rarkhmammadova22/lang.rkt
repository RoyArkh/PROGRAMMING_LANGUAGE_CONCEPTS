#lang eopl

;; grammar for the PROC language

(provide (all-defined-out))

;;;;;;;;;;;;;;;; grammatical specification ;;;;;;;;;;;;;;;;

(define the-lexical-spec
  '((whitespace (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier
     (letter (arbno (or letter digit "_" "-" "?")))
     symbol)
    (number (digit (arbno digit)) number)
    (number ("-" digit (arbno digit)) number)
    ))

(define the-grammar
  '((program (expression) a-program)
    
    (expression (number) const-exp)
    (expression
     ("-" "(" expression "," expression ")")
     diff-exp)
    
    (expression
     ("zero?" "(" expression ")")
     zero?-exp)
    
    (expression
     ("if" expression "then" expression "else" expression)
     if-exp)
    
    (expression (identifier) var-exp)
    
    (expression
     ("let" identifier "=" expression "in" expression)
     let-exp)

    (expression
     ("proc" "(" identifier ")" expression)
     proc-exp)
    
    (expression
     ("(" expression expression ")")
     call-exp)

    ;;---------------------------------------------------------
    ; INSERT YOUR CODE HERE
    ; Add the syntax for the new expressions here below:

    (expression
     ("empty-stack()")
     stack-exp)

    (expression
     ("stack-push(" expression "," expression ")")
     stack-push-exp)

    (expression
     ("stack-pop(" expression ")")
     stack-pop-exp)

    (expression
     ("stack-peek(" expression ")")
     stack-peek-exp)

    (expression
     ("stack-push-multi(" expression (arbno "," expression) ")")
     stack-push-multi-exp)

    (expression
     ("stack-pop-multi(" expression "," number ")")
     stack-pop-multi-exp)

    (expression
     ("stack-merge(" expression "," expression ")")
     stack-merge-exp)

    ;;---------------------------------------------------------
    
    ))

;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;

(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))

(define just-scan
  (sllgen:make-string-scanner the-lexical-spec the-grammar))


(module data-structures (lib "eopl.ss" "eopl")

  (require "lang.scm")                  ; for expression?
  (require "store.scm")                 ; for reference?

  (provide (all-defined-out))               ; too many things to list

;;;;;;;;;;;;;;;; expressed values ;;;;;;;;;;;;;;;;

;;; an expressed value is either a number, a boolean, a procval, or a
;;; reference. 

  (define-datatype expval expval?
    (num-val
      (value number?))
    (bool-val
      (boolean boolean?))
    (proc-val 
      (proc proc?))
    (ref-val
      (ref reference?))
    ; #####################################################
    ; ###### ENTER YOUR CODE HERE
    ; ###### add a new value type for your vectors (and possible for queues)
    ; #####################################################

    (vec-val
      (vec vec?))
    (queue-val
      (queue queue?))
    
    ; #####################################################
    )

;;; extractors:

  (define expval->num
    (lambda (v)
      (cases expval v
	(num-val (num) num)
	(else (expval-extractor-error 'num v)))))

  (define expval->bool
    (lambda (v)
      (cases expval v
	(bool-val (bool) bool)
	(else (expval-extractor-error 'bool v)))))

  (define expval->proc
    (lambda (v)
      (cases expval v
	(proc-val (proc) proc)
	(else (expval-extractor-error 'proc v)))))

  (define expval->ref
    (lambda (v)
      (cases expval v
	(ref-val (ref) ref)
	(else (expval-extractor-error 'reference v)))))

  (define expval-extractor-error
    (lambda (variant value)
      (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
	variant value)))

  

  ;; HINT if you need extractors, add them here

  (define expval->vec
    (lambda (v)
      (cases expval v
        (vec-val (vec) vec)
        (else (expval-extractor-error 'vec v)))))

  (define expval->queue
    (lambda (v)
      (cases expval v
        (queue-val (queue) queue)
        (else (expval-extractor-error 'queue v)))))
  

;;;;;;;;;;;;;;;; procedures ;;;;;;;;;;;;;;;;

  ; #####################################################
  ; ###### ENTER YOUR CODE HERE
  ; ###### you might want to add a new datatype for vectors here similar 
  ; ###### to mutable pairs.
  ; #####################################################

  (define-datatype vec vec?
    (a-vector
     (first reference?)
     (length integer?)))

  (define-datatype queue queue?
    (a-queue
     (entry vec?)
     (start reference?)
     (end reference?)
     (length reference?)))
  
  ; #####################################################

  (define-datatype proc proc?
    (procedure
      (bvar symbol?)
      (body expression?)
      (env environment?)))
  
  (define-datatype environment environment?
    (empty-env)
    (extend-env 
      (bvar symbol?)
      (bval expval?)
      (saved-env environment?))
    (extend-env-rec*
      (proc-names (list-of symbol?))
      (b-vars (list-of symbol?))
      (proc-bodies (list-of expression?))
      (saved-env environment?)))

  ;; env->list : Env -> List
  ;; used for pretty-printing and debugging
  (define env->list
    (lambda (env)
      (cases environment env
	(empty-env () '())
	(extend-env (sym val saved-env)
	  (cons
	    (list sym (expval->printable val))
	    (env->list saved-env)))
	(extend-env-rec* (p-names b-vars p-bodies saved-env)
	  (cons
	    (list 'letrec p-names '...)
	    (env->list saved-env))))))

  ;; expval->printable : ExpVal -> List
  ;; returns a value like its argument, except procedures get cleaned
  ;; up with env->list 
  (define expval->printable
    (lambda (val)
      (cases expval val
	(proc-val (p)
	  (cases proc p
	    (procedure (var body saved-env)
	      (list 'procedure var '... (env->list saved-env)))))
	(else val))))

  ; ###### YOU CAN WRITE HELPER FUNCTIONS HERE

  (define (vec-new length value)
    (if (> length 0)
        (let loop ((i 0) (ref -1))
          (if (= i length)
              (a-vector (- ref (- length 1)) length)
              (loop (+ i 1) (newref value))))
        (eopl:error 'vec-new "Bad Length")))
  (define (vec-zeros length)
    (vec-new length (num-val 0)))
  
  
  (define (vec-length vector)
    (cases vec vector
      (a-vector (first length) length)))
  
  
  (define (vec-set! vector index value)
    (cases vec vector
      (a-vector (first length)
                (if (and (>= index 0) (< index length))
                    (setref! (+ first index) value)
                    (eopl:error 'vec-set! "Bad Index")))))
  
  
  (define (vec-ref vector index)
    (cases vec vector
      (a-vector (first length)
                (if (and (>= index 0) (< index length))
                    (deref (+ first index))
                    (eopl:error 'vec-ref "Bad Index")))))
  
  
  (define (vec-copy vector)
    (cases vec vector
      (a-vector (first length)
                (let ((copy (vec-zeros length)))
                  (let loop ((i 0))
                    (if (= i length) copy
                        (begin (vec-set! copy i (deref (+ first i))) (loop (+ i 1)))))))))
  
  
  (define (vec-swap! vector index1 index2)
    (cases vec vector
      (a-vector (first length)
                (if (and (>= index1 0) (< index1 length) (>= index2 0) (< index2 length))
                    (let ((temp (deref (+ first index1))))
                      (setref! (+ first index1) (deref (+ first index2)))
                      (setref! (+ first index2) temp))
                    (eopl:error 'vec-swap! "Bad Index")))))

  


  (define (queue-new n)
    (a-queue (vec-new n 0)
             (newref 0)
             (newref -1)
             (newref 0)))

  (define (queue-empty? q)
    (cases queue q
      (a-queue (entry start end length)
               (= (deref length) 0))))
  
  
  (define (queue-full? q)
    (cases queue q
      (a-queue (entry start end length)
               (= (deref length) (vec-length entry)))))

  
  (define (set-start! q value)
    (cases queue q
      (a-queue (entry start end length)
               (if (and (>= value 0) (< value (vec-length entry)))
                   (setref! start value)
                   (eopl:error 'set-start! "Bad Index")))))
  

  (define (set-end! q value)
    (cases queue q
      (a-queue (entry start end length)
               (if (and (>= value 0) (< value (vec-length entry)))
                   (setref! end value)
                   (eopl:error 'set-end! "Bad Index")))))


  (define (queue-enqueue! q value)
    (cases queue q
      (a-queue (entry start end length)
               (if (queue-full? q)
                   (eopl:error 'queue-enqueue! "Full Queue")
                   (begin (set-end! q (modulo (+ (deref end) 1) (vec-length entry)))
                          (vec-set! entry (deref end) value)
                          (setref! length (+ (deref length) 1)))))))

  
  (define (queue-dequeue! q)
    (cases queue q
      (a-queue (entry start end length)
               (if (queue-empty? q)
                   (num-val -1)
                   (begin (let ((value (vec-ref entry (deref start))))
                            (set-start! q (modulo (+ (deref start) 1) (vec-length entry)))
                            (setref! length (- (deref length) 1))
                            value))))))

  
  (define (queue-length q)
    (cases queue q
      (a-queue (entry start end length)
               (deref length))))

  
  (define (queue-peek q)
    (cases queue q
      (a-queue (entry start end length)
               (if (queue-empty? q)
                   (num-val -1)
                   (vec-ref entry (deref start))))))


  (define (queue-print q)
    (cases queue q
      (a-queue (entry start end length)
               (let loop ((i 0) (index (deref start)))
                 (if (= i (deref length))
                     (newline)
                     (begin (display (vec-ref entry i))
                            (display " ")
                     (loop (+ i 1) (modulo (+ index 1) (vec-length entry)))))))))



  (define (vec-mult vector1 vector2)
    (cases vec vector1
      (a-vector (head1 length1)
                (cases vec vector2
                  (a-vector (head2 length2)
                            (if (= length1 length2)
                                (let ((result (vec-new length1 0)))
                                  (let loop ((i 0))
                                    (if (= i length1) result
                                        (begin (vec-set! result i
                                                         (num-mult (deref (+ head1 i)) (deref (+ head2 i)))) 
                                               (loop (+ i 1))))))
                                (eopl:error 'vec-mult "Bad Length")))))))

  (define (num-mult num1 num2)
    (let ((n1 (expval->num num1))
          (n2 (expval->num num2)))
      (num-val (* n1 n2))))
  
  )

(load "chez-init.ss")

; #datatypes

(define-datatype expression expression?
  [var-exp
    (id symbol?)]
  [lit-exp
    (id (lambda (x) 
          (or (number? x) (string? x) (boolean? x) (null? x) (vector? x))))] ; what about quoted lists?
  [quoted-exp 
    (id (lambda x #t))]
  [if-else-exp
    (condition expression?)
    (true expression?)
    (false expression?)]
  [if-exp
    (condition expression?)
    (true expression?)]
  [let-exp
    (vars (list-of symbol?))
    (declarations (list-of expression?))
    (body (list-of expression?))]
  [named-let-exp
    (name symbol?)
    (vars (list-of symbol?))
    (declarations (list-of expression?))
    (body (list-of expression?))]
  [let*-exp
    (vars (list-of symbol?))
    (declarations (list-of expression?))
    (body (list-of expression?))]
  [letrec-exp
    (vars (list-of symbol?))
    (declarations (list-of expression?))
    (body (list-of expression?))]
  [lambda-list-exp
    (id (list-of symbol?))
    (body (list-of expression?))]
  [lambda-single-exp
    (id symbol?)
    (body (list-of expression?))]
  [lambda-improper-exp
    (id (list-of symbol?))
    (other symbol?)
    (body (list-of expression?))]
  [set!-exp
    (var symbol?)
    (expr expression?)]
  [app-exp
    (rator expression?)
    (rand (list-of expression?))])
	
; datatype for procedures.  At first there is only one
; kind of procedure, but more kinds will be added later.

(define-datatype proc-val proc-val?
  [prim-proc
   (name symbol?)])
	 	
; environment type definitions

(define scheme-value?
  (lambda (x) #t))

(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (syms (list-of symbol?))
   (vals (list-of scheme-value?))
   (env environment?)))

; #parser

; Procedures to make the parser a little bit saner.
(define 1st car)
(define 2nd cadr)
(define 3rd caddr)
(define 4th cadddr)

(define parse-exp         
  (lambda (datum)
    (cond
      [(symbol? datum) (var-exp datum)]
      [(and (list? datum) (eqv? (1st datum) 'quote)) (quoted-exp (2nd datum))]
      [(or (number? datum) (string? datum) (boolean? datum) (null? datum) (vector? datum))  (lit-exp datum)]
      [(eqv? (1st datum) 'lambda)
       (cond
         [(list? (2nd datum)) ; args is proper list
          (if (valid-lambda-list-exp? datum) 
              (lambda-list-exp 
                (2nd datum)
                (map parse-exp (cddr datum))))]
         [(symbol? (2nd datum)) ; single arg
          (if (valid-lambda-single-exp? datum)
              (lambda-single-exp
                (2nd datum)
                (map parse-exp (cddr datum))))]
         [else ; improper list
           (if (valid-lambda-improper-exp? datum) 
               (lambda-improper-exp
                 (get-all-but-last-improper (2nd datum))
                 (get-last-improper (2nd datum))
                 (map parse-exp (cddr datum))))]
         )]
      [(eqv? (1st datum) 'if)
       (if (valid-if? datum) 
           (if 
               (eqv? (length datum) 4) 
               (if-else-exp ; if-then-else
                 (parse-exp (2nd datum)) 
                 (parse-exp (3rd datum)) 
                 (parse-exp (4th datum))) 
               (if-exp ; if-then
                 (parse-exp (2nd datum))
                 (parse-exp (3rd datum)))))]
      [(eqv? (1st datum) 'let)
       (if
         (symbol? (2nd datum)) ; named let
         (if (valid-named-let-exp? datum) 
             (named-let-exp
               (2nd datum)
               (map 1st (3rd datum))
               (map (lambda (x) (parse-exp (2nd x))) (3rd datum))
               (map parse-exp (cdddr datum))))
         (if (valid-let-exp? datum) 
             (let-exp ; normal let
               (map 1st (2nd datum))
               (map (lambda (x) (parse-exp (2nd x))) (2nd datum))
               (map parse-exp (cddr datum)))))]
      [(eqv? (1st datum) 'let*) 
       (if (valid-let*-exp? datum) 
           (let*-exp
             (map 1st (2nd datum))
             (map (lambda (x) (parse-exp (2nd x))) (2nd datum))
             (map parse-exp (cddr datum))))]
      [(eqv? (1st datum) 'letrec) 
       (if (valid-letrec-exp? datum) 
           (letrec-exp
             (map 1st (2nd datum))
             (map (lambda (x) (parse-exp (2nd x))) (2nd datum))
             (map parse-exp (cddr datum))))]
      [(eqv? (1st datum) 'set!)
       (if (valid-set!-exp? datum) 
           (set!-exp
             (2nd datum)
             (parse-exp (3rd datum))))]
         [(valid-app-exp? datum) 
          (app-exp 
            (parse-exp (1st datum))
            (map parse-exp (cdr datum)))]      
      [else (eopl:error 'parse-exp "bad expression: ~s" datum)])))

; #environment                               

; Environment definitions for CSSE 304 Scheme interpreter.  Based on EoPL section 2.3

(define empty-env
  (lambda ()
    (empty-env-record)))

(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms vals env)))

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (xsym) (eqv? sym xsym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
        	     (if (number? list-index-r)
               		 (+ 1 list-index-r)
               		 #f))))))

(define apply-env
  (lambda (env sym succeed fail) ; succeed and fail are procedures applied if the var is or isn't found, respectively.
    (cases environment env
      (empty-env-record ()
        (fail))
      (extended-env-record (syms vals env)
	(let ((pos (list-find-position sym syms)))
      	  (if (number? pos)
	      (succeed (list-ref vals pos))
	      (apply-env env sym succeed fail)))))))


 ; #interpreter                                                             

; top-level-eval evaluates a form in the global environment

(define top-level-eval
  (lambda (form)
    ; later we may add things that are not expressions.
    (eval-exp form init-env)))

; eval-exp is the main component of the interpreter

(define eval-exp
  (lambda (exp env)
    (cases expression exp
      [lit-exp (datum) datum]
      [var-exp (id)
        (apply-env init-env id; look up its value.
          (lambda (x) x) ; procedure to call if id is in the environment 
          (lambda () (eopl:error 'apply-env ; procedure to call if id not in env
		          "variable not found in environment: ~s"
			   id)))]
      [quoted-exp (id) id]
      [if-else-exp (condition true false)
        (if (eval-exp condition env) (eval-exp true env) (eval-exp false env))]
      [app-exp (rator rands)
        (let ([proc-value (eval-exp rator env)]
              [args (eval-rands rands)])
          (apply-proc proc-value args))]
      [else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)])))

; evaluate the list of operands, putting results into a list

(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-exp x env)) rands)))

;  Apply a procedure to its arguments.
;  At this point, we only have primitive procedures.  
;  User-defined procedures will be added later.

(define apply-proc
  (lambda (proc-value args)
    (cases proc-val proc-value
      [prim-proc (op) (apply-prim-proc op args)]
			; You will add other cases
      [else (error 'apply-proc
                   "Attempt to apply bad procedure: ~s" 
                    proc-value)])))

(define *prim-proc-names* '(+ - * add1 sub1 cons =))

(define init-env         ; for now, our initial global environment only contains 
  (extend-env            ; procedure names.  Recall that an environment associates
     *prim-proc-names*   ;  a value (not an expression) with an identifier.
     (map prim-proc      
          *prim-proc-names*)
     (empty-env)))

; Usually an interpreter must define each 
; built-in procedure individually.  We are "cheating" a little bit.

(define apply-prim-proc
  (lambda (prim-proc args)
    (case prim-proc
      [(+) (+ (1st args) (2nd args))]
      [(-) (- (1st args) (2nd args))]
      [(*) (* (1st args) (2nd args))]
      [(add1) (+ (1st args) 1)]
      [(sub1) (- (1st args) 1)]
      [(cons) (cons (1st args) (2nd args))]
      [(=) (= (1st args) (2nd args))]
      [else (error 'apply-prim-proc 
            "Bad primitive procedure name: ~s" 
            prim-op)])))

(define rep      ; "read-eval-print" loop.
  (lambda ()
    (display "--> ")
    ;; notice that we don't save changes to the environment...
    (let ([answer (top-level-eval (parse-exp (read)))])
      ;; TODO: are there answers that should display differently?
      (eopl:pretty-print answer) (newline)
      (rep))))  ; tail-recursive, so stack doesn't grow.

(define eval-one-exp
  (lambda (x) (top-level-eval (parse-exp x))))

; #error checking

(define valid-lambda-list-exp?
  (lambda (lst)
    (cond
      [(< (length lst) 3)
       (eopl:error 'parse-exp "lambda expression: incorrect length: ~s" lst)]
      [(not (list? (2nd lst)))
       (eopl:error 'parse-exp "lambda expression: arguments must be a proper list: ~s" (2nd lst))]
      [(not (andmap symbol? (2nd lst)))
       (eopl:error 'parse-exp "lambda argument list: formals must be symbols: ~s" (2nd lst))]
      [else #t]
      )))

(define valid-lambda-improper-exp?
  (lambda (lst)
    (cond
      [(< (length lst) 3)
       (eopl:error 'parse-exp "lambda expression: incorrect length: ~s" lst)]
      [(not (andmap symbol? (get-all-but-last-improper (2nd lst))))
       (eopl:error 'parse-exp "lambda expression: arguments must be symbols: ~s" (2nd lst))]
      [(not (symbol? (get-last-improper (2nd lst))))
       (eopl:error 'parse-exp "lambda expression: arguments must be symbols: ~s" (2nd lst))]
      [else #t]
      )))

(define valid-lambda-single-exp?
  (lambda (lst)
    (cond
      [(< (length lst) 3)
       (eopl:error 'parse-exp "lambda expression: incorrect length: ~s" lst)]
      [else #t]
      )))
               
(define valid-app-exp?
  (lambda (lst)
    (cond
      [(not (list? lst))
       (eopl:error 'parse-exp "application expression: not a proper list: ~s" lst)]
      [else #t]
      )))

(define valid-if?
  (lambda (lst)
    (cond
      [(< (length lst) 3)
       (eopl:error 'parse-exp "if expression: incomplete if expression: ~s" lst)]
      [else #t]
      )))

(define valid-let-exp?
  (lambda (lst)
    (cond
      [(< (length lst) 3)
       (eopl:error 'parse-exp "let expression: incorrect length: ~s" lst)]
      [(not (list? (2nd lst)))
       (eopl:error 'parse-exp "let expression: not all variable declarations are proper lists: ~s" (2nd lst))]
      [(not (andmap list? (2nd lst)))
       (eopl:error 'parse-exp "let expression: not all variable declarations are proper lists: ~s" (2nd lst))]
      [(not (andmap (lambda (x) (= (length x) 2)) (2nd lst)))
       (eopl:error 'parse-exp "let expression: not all variable declarations length 2: ~s" (2nd lst))]
      [(not (andmap (lambda (x) (symbol? (car x))) (2nd lst)))
       (eopl:error 'parse-exp "let expression: first members must be symbols: ~s" (2nd lst))]
      [else #t]
     )))

(define valid-named-let-exp?
  (lambda (lst)
    (cond
      [(< (length lst) 4)
       (eopl:error 'parse-exp "named let expression: incorrect length: ~s" lst)]
      [(not (list? (3rd lst)))
       (eopl:error 'parse-exp "named let expression: not all variable declarations are proper lists: ~s" (3rd lst))]
      [(not (andmap list? (3rd lst)))
       (eopl:error 'parse-exp "named let expression: not all variable declarations are proper lists: ~s" (3rd lst))]
      [(not (andmap (lambda (x) (= (length x) 2)) (3rd lst)))
       (eopl:error 'parse-exp "named let expression: not all variable declarations length 2: ~s" (3rd lst))]
      [(not (andmap (lambda (x) (symbol? (car x))) (3rd lst)))
       (eopl:error 'parse-exp "named let expression: first members must be symbols: ~s" (3rd lst))]
      [else #t]
     )))

(define valid-let*-exp?
  (lambda (lst)
    (cond
      [(< (length lst) 3)
       (eopl:error 'parse-exp "let* expression: incorrect length: ~s" lst)]
      [(not (list? (2nd lst)))
       (eopl:error 'parse-exp "let* expression: not all variable declarations are proper lists: ~s" (2nd lst))]
      [(not (andmap list? (2nd lst)))
       (eopl:error 'parse-exp "let* expression: not all variable declarations are proper lists: ~s" (2nd lst))]
      [(not (andmap (lambda (x) (= (length x) 2)) (2nd lst)))
       (eopl:error 'parse-exp "let* expression: not all variable declarations length 2: ~s" (2nd lst))]
      [(not (andmap (lambda (x) (symbol? (car x))) (2nd lst)))
       (eopl:error 'parse-exp "let* expression: first members must be symbols: ~s" (2nd lst))]
      [else #t]
     )))

(define valid-letrec-exp?
  (lambda (lst)
    (cond
      [(< (length lst) 3)
       (eopl:error 'parse-exp "letrec expression: incorrect length: ~s" lst)]
      [(not (list? (2nd lst)))
       (eopl:error 'parse-exp "letrec expression: not all variable declarations are proper lists: ~s" (2nd lst))]
      [(not (andmap list? (2nd lst)))
       (eopl:error 'parse-exp "letrec expression: not all variable declarations are proper lists: ~s" (2nd lst))]
      [(not (andmap (lambda (x) (= (length x) 2)) (2nd lst)))
       (eopl:error 'parse-exp "letrec expression: not all variable declarations length 2: ~s" (2nd lst))]
      [(not (andmap (lambda (x) (symbol? (car x))) (2nd lst)))
       (eopl:error 'parse-exp "letrec expression: first members must be symbols: ~s" (2nd lst))]
      [else #t]
      )))

(define valid-set!-exp?
  (lambda (lst)
    (cond
      [(or (< (length lst) 3) (> (length lst) 3))
       (eopl:error 'parse-exp "set! expression: incorrect length: ~s" lst)]
      [(not (symbol? (2nd lst)))
       (eopl:error 'parse-exp "set! expression: variable must be a symbol: ~s" lst)]
      [else #t]
      )))                                                                            
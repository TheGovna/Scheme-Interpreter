(load "chez-init.ss")

(define apply-k
  (lambda (k . vals)
    (apply k vals)))

 ; #datatypes
 ;  _____        _        _                         
 ; |  __ \      | |      | |                        
 ; | |  | | __ _| |_ __ _| |_ _   _ _ __   ___  ___ 
 ; | |  | |/ _` | __/ _` | __| | | | '_ \ / _ \/ __|
 ; | |__| | |_| | || |_| | |_| |_| | |_| |  __/\__ \
 ; |_____/ \__,_|\__\__,_|\__|\__, | .__/ \___||___/
 ;                             __/ | |              
 ;                            |___/|_|              

(define-datatype expression expression?
  [var-exp
    (id symbol?)]
  [lit-exp
   (id (lambda (x) 
         (or (number? x) (string? x) (boolean? x) (null? x) (vector? x))))]
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
  [lambda-ref-exp
    (id (list-of symbol?))
    (refs (list-of number?))
    ;(refs (list-of symbol?))
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
  [cond-exp
    (lefts (list-of expression?))
    (rights (list-of expression?))
    (else expression?)]
  [or-exp
    (conditions (list-of expression?))]
  [begin-exp
    (exps (list-of expression?))]
  [case-exp
    (expr expression?)
    (lefts (list-of expression?))
    (rights (list-of expression?))
    (else expression?)]
  [while-exp
    (condition expression?)
    (exprs (list-of expression?))]
  [app-exp
    (rator expression?)
    (rand (list-of expression?))]
  [define-exp
    (var symbol?)
    (expr expression?)]
  )

(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
    (syms (list-of symbol?))
    (vals (list-of box?))
    (env environment?)))

; datatype for procedures.  At first there is only one
; kind of procedure, but more kinds will be added later.

(define-datatype proc-val proc-val?
  [prim-proc
    (name symbol?)]
  [closure
    (ids (list-of symbol?))
    (bodies (list-of expression?))
    (env environment?)]
  [closure-single-arg
    (id symbol?)
    (bodies (list-of expression?))
    (env environment?)]
  [closure-improper-args
    (ids (list-of symbol?))
    (other symbol?)
    (bodies (list-of expression?))
    (env environment?)]
  [closure-ref
    (ids (list-of symbol?))
    (refs (list-of number?))
    (bodies (list-of expression?))
    (env environment?)])
	 	
; environment type definitions

(define scheme-value?
  (lambda (x) #t))

 ; #parser
 ;  _____                         
 ; |  __ \                        
 ; | |__| |_ _ _ __ ___  ___ _ __ 
 ; |  ___/ _` | '__/ __|/ _ \ '__|
 ; | |  | |_| | |  \__ \  __/ |   
 ; |_|   \__,_|_|  |___/\___|_|   

; Procedures to make the parser a little bit saner.
(define 1st car)
(define 2nd cadr)
(define 3rd caddr)
(define 4th cadddr)

; Gets everything but the last element of an improper list
; Ex: improper-lst = '(a b . c)
;     result = (a b)
(define get-all-but-last-improper
  (lambda (improper-lst)
    (let loop [[lst improper-lst]
               [result '()]]
      (if (pair? lst) 
          (loop (cdr lst) (append result (list (car lst))))
          result))))
; Gets the last element of an improper list
; Ex: improper-lst = '(a b . c)
;     result = c
(define get-last-improper
  (lambda (improper-lst)
    (let loop [[lst improper-lst]]
      (if (pair? lst) 
          (loop (cdr lst)) 
          lst))))

; Takes an object to check and a list. Returns true if obj is in the list, false otherwise
(define member?
  (lambda (obj list)
    (not (not (member obj list)))))

; Takes a list-of expression and extracts the lefts of each expression except the last
(define get-lefts
  (lambda (exp)
    (let loop [[exp exp]
               [result '()]]
      (cond
        [(= (length exp) 1) result]
        ;[else (loop (cdr exp) (cons (caar exp) result))]
        [else (loop (cdr exp) (append result (list (caar exp))))]
        ))))

; Takes a list-of expression and extracts the rights of each expression except the last
(define get-rights
  (lambda (exp)
    (let loop [[exp exp]
               [result '()]]
      (cond
        [(= (length exp) 1) result]
        [else (loop (cdr exp) (append result (list (cadar exp))))]
        ))))

(define get-ids
  (lambda (args)
    (map (lambda (x)
           (if (list? x)
               (cadr x)
               x)) args)))

(define get-refs
  (lambda (args)
    (let loop [[args args]
               [result '()]
               [count 0]]
      (cond
        [(null? args) (reverse result)]
        [(list? (car args)) ; assume ref
         (loop (cdr args) (cons count result) (+ count 1))]
        [else
          (loop (cdr args) result (+ count 1))]
        ))))

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
                (map parse-exp (cddr datum)))
              (lambda-ref-exp
                (get-ids (2nd datum))
                (get-refs (2nd datum))
                (map parse-exp (cddr datum)))
              )]
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
           (if (eqv? (length datum) 4) 
               (if-else-exp ; if-then-else
                 (parse-exp (2nd datum)) 
                 (parse-exp (3rd datum)) 
                 (parse-exp (4th datum))) 
               (if-exp ; if-then
                 (parse-exp (2nd datum))
                 (parse-exp (3rd datum)))))]
      [(eqv? (1st datum) 'cond)
       (cond-exp
         (map parse-exp (get-lefts (cdr datum)))
         (map parse-exp (get-rights (cdr datum)))
         (parse-exp (cadar (last-pair datum))))]
      [(eqv? (1st datum) 'let)
       (if (symbol? (2nd datum)) ; named let
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
      [(eqv? (1st datum) 'or)
        (or-exp (map parse-exp (cdr datum)))]
      [(eqv? (1st datum) 'begin)
       (begin-exp
         (map parse-exp (cdr datum)))]
      [(eqv? (1st datum) 'case)
       (case-exp
         (parse-exp (2nd datum))
         (map quoted-exp (get-lefts (cddr datum)))
         (map parse-exp (get-rights (cddr datum)))
         (parse-exp (cadar (last-pair datum))))]
      [(eqv? (1st datum) 'while)
       (while-exp
         (parse-exp (2nd datum))
         (map parse-exp (cddr datum)))]
      [(eqv? (1st datum) 'define)
       (define-exp
         (2nd datum)
         (parse-exp (3rd datum)))]
      [(valid-app-exp? datum) 
       (app-exp 
         (parse-exp (1st datum))
         (map parse-exp (cdr datum)))]     
      [else (eopl:error 'parse-exp "bad expression: ~s" datum)])))

 ; #environment                               
 ;  ______            _                                      _   
 ; |  ____|          |_|                                    | |  
 ; | |__   _ ____   ___ _ __ ___  _ __  _ __ ___   ___ _ __ | |_ 
 ; |  __| | '_ \ \ / / | '__/ _ \| '_ \| '_ ` _ \ / _ \ '_ \| __|
 ; | |____| | | \ V /| | | | |_| | | | | | | | | |  __/ | | | |_ 
 ; |______|_| |_|\_/ |_|_|  \___/|_| |_|_| |_| |_|\___|_| |_|\__|

; Environment definitions for CSSE 304 Scheme interpreter.  Based on EoPL section 2.3

; produces a representation of the empty environment
(define empty-env
  (lambda ()
    (empty-env-record)))

(define extend-env
  (lambda (syms vals env)
    (begin ;(display '(*** extend-env ***)) (newline) (display '-syms:) (display syms) (newline) (display '-vals:) (display vals) (newline) (display '-env:) (display env) (newline) (newline)
      (extended-env-record syms (map box vals) env))))

(define extend-env-with-refs
  (lambda (syms boxed-vals env)
    (begin ;(display '(*** extend-env ***)) (newline) (display '-syms:) (display syms) (newline) (display '-vals:) (display vals) (newline) (display '-env:) (display env) (newline) (newline)
      (extended-env-record syms boxed-vals env))))

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

 ;(define apply-env
 ; (lambda (env sym succeed fail) ; succeed and fail are procedures applied if the var is or isn't found, respectively.
 ;   (cases environment env
 ;     (empty-env-record ()
 ;       (fail))
 ;     (extended-env-record (syms vals env)
	;(let ((pos (list-find-position sym syms)))
 ;     	  (if (number? pos)
	;      (succeed (deref (list-ref vals pos)))
	;      (apply-env env sym succeed fail)))))))
(define apply-env
  (lambda (env sym succeed fail)
    (deref (apply-env-ref env sym succeed fail))))


(define (apply-env-ref env sym succeed fail)
  (cases environment env
    (empty-env-record ()
      (fail))
    (extended-env-record (syms vals env)
      (let ((pos (list-find-position sym syms)))
        (if (number? pos)
            (succeed (list-ref vals pos))
            (apply-env-ref env sym succeed fail))))))

(define (deref ref)
  (unbox ref))

(define (set-ref! ref value)
  (set-box! ref value))

(define (reset-global-env)
  (set! global-env init-env))

 ; #syntax expander
 ;   _____             _               ______                            _           
 ;  / ____|           | |             |  ____|                          | |          
 ; | (___  _   _ _ __ | |_ __ ___  __ | |__  __  ___ __   __ _ _ __   __| | ___ _ __ 
 ;  \___ \| | | | '_ \| __/ _` \ \/ / |  __| \ \/ / '_ \ / _` | '_ \ / _` |/ _ \ '__|
 ;  ____) | |_| | | | | || (_| |>  <  | |____ >  <| |_) | (_| | | | | (_| |  __/ |   
 ; |_____/ \__, |_| |_|\__\__,_/_/\_\ |______/_/\_\ .__/ \__,_|_| |_|\__,_|\___|_|   
 ;          __/ |                                 | |                                
 ;         |___/                                  |_|                                

(define syntax-expand
  (lambda (exp)
    (cases expression exp
      [var-exp (id) exp]
      [lit-exp (id) exp]
      [quoted-exp (id) exp]
      [if-else-exp (condition true false)
        (if-else-exp
          (syntax-expand condition)
          (syntax-expand true)
          (syntax-expand false))]
      [if-exp (condition true)
        (if-else-exp
          (syntax-expand condition)
          (syntax-expand true)
          (app-exp (var-exp 'void) '()))]
      [named-let-exp (name vars declarations bodies)
        (syntax-expand
          (letrec-exp (list name) (list (lambda-list-exp vars bodies)) (list (app-exp (var-exp name) declarations))))]
      [let-exp (vars declarations bodies)
        (syntax-expand
          (app-exp     
            (lambda-list-exp
              vars
              bodies)
            declarations))]
      [let*-exp (vars declarations bodies)
        (syntax-expand 
          (if (= (length vars) 0)
              (let-exp
                '()
                '()
                bodies)
              (let-exp
                (list (car vars))
                (list (car declarations))
                (list (let*-exp
                        (cdr vars)
                        (cdr declarations)
                        bodies)))))]
      [letrec-exp (vars declarations bodies)
        (let [[random-symbols (make-list-of-random-symbols (length vars))]]
          (syntax-expand
            (let-exp
              vars
              (map lit-exp (make-list-of #f (length vars)))
                (list (let-exp                    
                        random-symbols
                        declarations
                        (append 
                          (map (lambda (var temp)
                                 (set!-exp var (var-exp temp))) vars random-symbols)
                          bodies
                          ))))))]
      [lambda-list-exp (id bodies)
        (lambda-list-exp
          id
          (map syntax-expand bodies))]
      [lambda-ref-exp (ids refs bodies)
        (lambda-ref-exp
          ids
          refs
          (map syntax-expand bodies))]      
      [lambda-single-exp (id bodies) exp]      
      [lambda-improper-exp (id other bodies) exp]
      [set!-exp (var expr) 
        (set!-exp
          var
          (syntax-expand expr))]
      [app-exp (rator rands)
        (begin ;(display exp) (newline) (display rands)
        (app-exp
          (syntax-expand rator)
          (map syntax-expand rands)))]
      [cond-exp (conditions expressions else)
        (syntax-expand (cond
                         [(and (null? conditions) (null? expressions))
                          else]
                         [else
                           (if-else-exp
                             (car conditions)
                             (car expressions)
                             (cond-exp
                               (cdr conditions)
                               (cdr expressions)
                               else))]))]
      [begin-exp (exps)
        (app-exp (lambda-list-exp
                   '()
                   (map syntax-expand exps))
          '())]
      [case-exp (expr lefts rights else)
        (syntax-expand 
          (if (null? lefts) 
              (begin-exp (list expr else))
              (let [[temp (generate-random-symbol)]]
                (let-exp
                  (list temp)
                  (list expr)
                  (list
                    (let loop [[lefts lefts] [rights rights]]
                      (if-else-exp
                        (app-exp (var-exp 'member?) (list (var-exp temp) (car lefts)))
                        (car rights)
                        (if (not (null? (cdr lefts)))
                            (loop (cdr lefts) (cdr rights))
                            else))))))))]
      [while-exp (condition exprs)
        (begin ;(display condition) (newline) (display exprs)
        (let [[temp (generate-random-symbol)]]
          (syntax-expand
            (let-exp
              (list 'x)
              (list (lambda-list-exp
                 (list temp)
                 (list (if-exp
                    condition
                    (begin-exp
                      (append exprs
                        (list (app-exp (var-exp temp) (list (var-exp temp))))))))))
              (list (app-exp (var-exp 'x) (list (var-exp 'x))))))))]      
      [or-exp (exps)
        (syntax-expand
          (let [[temp (generate-random-symbol)]]
            (cond
               [(null? exps) (lit-exp #f)]
               [(null? (cdr exps)) (car exps)]
               [else
                 (let-exp (list temp) (list (car exps))
                   (list (if-else-exp (var-exp temp)
                           (var-exp temp)
                           (or-exp (cdr exps)))))]
               )))
        ]
      [define-exp (var expr) 
        (define-exp
          var
          (syntax-expand expr))
        ]
      )))
                                                                       
(define member?
  (lambda (obj list)
    (not (not (member obj list)))))

(define make-list-of
  (lambda (obj size)
    (let loop [[size size]
               [result '()]]
      (if (eq? size 0)
          result
          (loop (- size 1) (cons obj result))))))                                                                       

(define make-list-of-random-symbols
  (lambda (size)
    (let loop [[size size]
               [result '()]]
      (if (eq? size 0)
          result
          (loop (- size 1) (cons (generate-random-symbol) result))))))                                                                       

; generates a random symbol
(define generate-random-symbol
  (lambda ()
    (string->symbol (string-append "temp" (number->string (random 1000000000))))))                                                                       

 ; #interpreter   
 ;  _____       _                           _            
 ; |_   _|     | |                         | |           
 ;   | |  _ __ | |_ ___ _ __ _ __  _ __ ___| |_ ___ _ __ 
 ;   | | | '_ \| __/ _ \ '__| '_ \| '__/ _ \ __/ _ \ '__|
 ;  _| |_| | | | ||  __/ |  | |_| | | |  __/ ||  __/ |   
 ; |_____|_| |_|\__\___|_|  | .__/|_|  \___|\__\___|_|   
 ;                          | |                          
 ;                          |_|                                                                                        

                                                                       
(define map-cps
  (lambda (proc-cps L k)
    (if (null? L)
        (apply-k k '())
        (proc-cps (car L) (lambda (mapped-car) 
                            (map-cps proc-cps (cdr L) (lambda (mapped-cdr) 
                                                        (apply-k k (cons mapped-car mapped-cdr))))))
        )))
                                                                       
; top-level-eval evaluates a form in the global environment

(define top-level-eval
  (lambda (form)
    ; later we may add things that are not expressions.
      ;(eval-exp form (empty-env))
    (eval-exp-cps form (empty-env) (lambda (k) k))
    ))

(define eval-bodies
  (lambda (bodies env)
    (let eval-bodies ([bodies bodies] [env env])
      (if (null? (cdr bodies))
          (eval-exp (car bodies) env)
          (begin (eval-exp (car bodies) env) (eval-bodies (cdr bodies) env))))))

(define eval-bodies-cps
  (lambda (bodies env k)
    (let eval-bodies ([bodies bodies] [env env])
      (if (null? (cdr bodies))
          (eval-exp-cps (car bodies) env k)
          (eval-exp-cps (car bodies) env (lambda (evalled-car)
                                           (eval-bodies-cps (cdr bodies) env k)))))))
                                                                       
; eval-exp is the main component of the interpreter

(define eval-exp
  (lambda (exp env)
    (cases expression exp
      [lit-exp (datum) datum]
      [var-exp (id)
        (apply-env 
          env 
          id; look up its value.
          (lambda (x) x) ; procedure to call if id is in the environment 
          (lambda () ; procedure to call if id is not in env
            (apply-env-ref
              global-env ; was init-env
              id
              (lambda (x) x)
              (lambda () 
                (eopl:error 'apply-env ; procedure to call if id not in env
                		         "variable not found in environment: ~s"
                           id)
                ))
            ))]
      [quoted-exp (id) id]
      [if-else-exp (condition true false)
        (if (eval-exp condition env) 
            (eval-exp true env) 
            (eval-exp false env))]
      ;[if-exp (condition true)
      ;  (if (eval-exp condition env) 
      ;      (eval-exp true env) 
      ;      (void))]
      [app-exp (rator rands)
        (let ([proc-value (eval-exp rator env)]
              [args (eval-rands rands env)])
          (apply-proc proc-value args))
        ]
      ;[let-exp (vars declarations bodies)
      ;  (let [[new-env
      ;          (extend-env 
      ;            vars 
      ;            (map (lambda (x) (eval-exp x env)) declarations) 
      ;            env)]]
      ;    (eval-bodies bodies new-env))] ; evaluate bodies in order, return last value
      [lambda-list-exp (id bodies)
          (closure id bodies env)]
      [lambda-single-exp (id bodies)
        (closure-single-arg id bodies env)]
      [lambda-improper-exp (ids other bodies)
        (closure-improper-args ids other bodies env)]
      [lambda-ref-exp (ids refs bodies)
        (closure-ref ids refs bodies env)]
      [set!-exp (var expr)
        (set-ref!
          (apply-env-ref env 
            var
            (lambda (x) x)
            (lambda ()
              (apply-env-ref
                global-env
                var
                (lambda (x) x)
                (lambda ()
                  (eopl:error 'set! "Variable not previously defined: ~s" var)))))
          (eval-exp expr env))]
      [define-exp (var expr)
        (begin (set! global-env (extend-env 
                           (list var) 
                           (list (eval-exp expr env)) 
                           global-env))
          )
        ]
      [else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)])))

(define eval-exp-cps
  (lambda (exp env k)
    (cases expression exp
      [lit-exp (datum) (apply-k k datum)]
      [var-exp (id)
        (apply-k k (apply-env 
                           env 
                           id; look up its value.
                           (lambda (x) x) ; procedure to call if id is in the environment 
                           (lambda () ; procedure to call if id is not in env
                             (apply-env-ref
                               global-env ; was init-env
                               id
                               (lambda (x) x)
                               (lambda () 
                                 (eopl:error 'apply-env ; procedure to call if id not in env
                        		         "variable not found in environment: ~s"
                                   id)
                                 ))
                             )))]
      [quoted-exp (id) (apply-k k id)]
      [if-else-exp (condition true false)
        ;(if (eval-exp condition env) 
        ;    (eval-exp true env) 
        ;    (eval-exp false env))
        (eval-exp-cps condition env (lambda (is-cond-true)
                                      (if is-cond-true
                                          (eval-exp-cps true env k)
                                          (eval-exp-cps false env k))))]
      ;[if-exp (condition true)
      ;  (if (eval-exp condition env) 
      ;      (eval-exp true env) 
      ;      (void))]
      [app-exp (rator rands)
        ;(let ([proc-value (eval-exp rator env)]
        ;      [args (eval-rands rands env)])
        ;  (apply-proc proc-value args))
        (eval-exp-cps rator env (lambda (proc-value)
                                  (eval-rands-cps rands env (lambda (args)
                                                              (apply-proc-cps proc-value args k)))))]
      ;[let-exp (vars declarations bodies)
      ;  (let [[new-env
      ;          (extend-env 
      ;            vars 
      ;            (map (lambda (x) (eval-exp x env)) declarations) 
      ;            env)]]
      ;    (eval-bodies bodies new-env))] ; evaluate bodies in order, return last value
      [lambda-list-exp (id bodies) ; HEREE
          (apply-k k (closure id bodies env))]
      [lambda-single-exp (id bodies)
        (apply-k k (closure-single-arg id bodies env))]
      [lambda-improper-exp (ids other bodies)
        (apply-k k (closure-improper-args ids other bodies env))]
      [lambda-ref-exp (ids refs bodies)
        (apply-k k (closure-ref ids refs bodies env))]
      [set!-exp (var expr)
        (eval-exp-cps expr env (lambda (evalled-expr)
                                 (apply-k k (set-ref!
                                   (apply-env-ref env 
                                     var
                                     (lambda (x) x)
                                     (lambda ()
                                       (apply-env-ref
                                         global-env
                                         var
                                         (lambda (x) x)
                                         (lambda ()
                                           (eopl:error 'set! "Variable not previously defined: ~s" var)))))
                                   evalled-expr))))
        ]
      [define-exp (var expr)
        (eval-exp-cps expr env (lambda (evalled-exp)
                                 (apply-k k (set! global-env (extend-env 
                                                               (list var) 
                                                               (list evalled-exp)
                                                               global-env)))))
        
        ]
      [else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)])))
                                                                       
; evaluate the list of operands, putting results into a list

(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-exp x env)) rands)))

(define eval-rands-cps
  (lambda (rands env k)
    (map-cps (lambda (x k)
               (eval-exp-cps x env k)) rands k)))
                                                                       
(define pick-from-list
  (lambda (ls refs)
    (map (lambda (x)
           (list-ref ls x)) refs)))
                                                                       
(define pick-all-except
  (lambda (ls refs)
    (let loop [[ls ls]
               [refs refs]
               [count 0]
               [result '()]]
      (cond
        [(null? ls) (reverse result)]
        [(null? refs) 
         (loop (cdr ls) refs (+ count 1) (cons (car ls) result))]
        [(= count (car refs))
         (loop (cdr ls) (cdr refs) (+ count 1) result)]
        [else
          (loop (cdr ls) refs (+ count 1) (cons (car ls) result))]
        ))))
                                                                       
;  Apply a procedure to its arguments.
;  At this point, we only have primitive procedures.  
;  User-defined procedures will be added later.

(define apply-proc
  (lambda (proc-value args)
    (cases proc-val proc-value
      [prim-proc (op) 
        (apply-prim-proc op args)]
      [closure (ids bodies env)
        (begin ;(display '(*** apply-proc to closure ***)) (newline) (display '-ids:) (display ids) (newline) (display '-bodies:) (display bodies) (newline) (display '-environment:) (display env) (newline) (newline)
        (let [[new-env
                (extend-env
                  ids
                  args
                  env)]]
          (eval-bodies bodies new-env)))]
      [closure-single-arg (id bodies env)
        (begin ;(display '(*** apply-proc to closure-single-arg ***)) (newline) (display '-id:) (display id) (newline) (display '-bodies:) (display bodies) (newline) (display '-environment:) (display env) (newline) (display '-args:) (display args) (newline)  (newline)       
          (let [[new-env
                  (extend-env
                    (list id)
                    (list args)
                    env)]]
            (eval-bodies bodies new-env))
       )]
      [closure-improper-args (ids other bodies env)
        (begin ;(display '(*** apply-proc to closure-improper-args ***)) (newline) (display '-ids:) (display ids) (newline) (display '-other:) (display other) (newline) (display '-bodies:) (display bodies) (newline) (display '-environment:) (display env) (newline) (display '-args:) (display args) (newline)  (newline)       
          (let* [[new-ids (append ids (list other))]
                 [new-env
                   (extend-env
                     new-ids
                     (group-improper-args args (length new-ids))
                     env)]]
            (eval-bodies bodies new-env))
          )]
			; You will add other cases
      [else (error 'apply-proc
                   "Attempt to apply bad procedure: ~s" 
                    proc-value)])))
                                                                       
(define apply-proc-cps
  (lambda (proc-value args k)
    (cases proc-val proc-value
      [prim-proc (op) 
        (apply-prim-proc-cps op args k)]
      [closure (ids bodies env)
        (begin ;(display '(*** apply-proc to closure ***)) (newline) (display '-ids:) (display ids) (newline) (display '-bodies:) (display bodies) (newline) (display '-environment:) (display env) (newline) (newline)
        (let [[new-env
                (extend-env
                  ids
                  args
                  env)]]
          (eval-bodies-cps bodies new-env k)))]
      [closure-single-arg (id bodies env)
        (begin ;(display '(*** apply-proc to closure-single-arg ***)) (newline) (display '-id:) (display id) (newline) (display '-bodies:) (display bodies) (newline) (display '-environment:) (display env) (newline) (display '-args:) (display args) (newline)  (newline)       
          (let [[new-env
                  (extend-env
                    (list id)
                    (list args)
                    env)]]
            (eval-bodies-cps bodies new-env k))
       )]
      [closure-improper-args (ids other bodies env) ; uhhh
        (begin ;(display '(*** apply-proc to closure-improper-args ***)) (newline) (display '-ids:) (display ids) (newline) (display '-other:) (display other) (newline) (display '-bodies:) (display bodies) (newline) (display '-environment:) (display env) (newline) (display '-args:) (display args) (newline)  (newline)       
          (let* [[new-ids (append ids (list other))]
                 [new-env
                   (extend-env
                     new-ids
                     (group-improper-args args (length new-ids))
                     env)]]
            (eval-bodies-cps bodies new-env k))
          )]
			; You will add other cases
      [else (error 'apply-proc
                   "Attempt to apply bad procedure: ~s" 
                    proc-value)])))

(define *prim-proc-names* '(+ - * / add1 sub1 zero? not cons = < > <= >= car cdr caar
                            cadr cdar cddr caaar caadr cadar caddr cdaar cdadr cddar
                            cdddr list append null? assq eq? eqv? equal? atom? length list->vector
                            list? pair? procedure? vector->list vector make-vector
                            vector-ref vector? number? symbol? set-car! set-cdr!
                            vector-set! display newline list-tail map apply member? quotient
                            newline display void))

(define init-env         ; for now, our initial global environment only contains 
  (extend-env            ; procedure names.  Recall that an environment associates
     *prim-proc-names*   ;  a value (not an expression) with an identifier.
     ;(map box
      (map prim-proc      
          *prim-proc-names*)
       ;)
     (empty-env)))

(define global-env init-env)

; Some helpers to group arguments for improper lambdas
(define split
  (lambda (args count)
    (let loop [[result '()] 
               [curr-count count]]
      (cond
        [(eq? curr-count (length args)) result]
        [else 
          (loop (append result (list (list-ref args curr-count))) (+ curr-count 1))]
        ))))

(define group-improper-args
  (lambda (args length-ids)
    (let loop [[result '()] 
               [count 0]]
      (cond
        [(eq? count (- length-ids 1)) (append result (list (split args count)))]
        [else (loop (append result (list (list-ref args count))) (+ count 1))]
        ))))

; Usually an interpreter must define each 
; built-in procedure individually.  We are "cheating" a little bit.
; TODO: add error statements if the interpreted code attempts to apply a proc to an incorrect number of args                                                                       

(define apply-prim-proc
  (lambda (prim-proc args)
    (case prim-proc
      [(+) (apply + args)]
      [(-) (apply - args)]
      [(*) (apply * args)]
      [(/) (apply / args)]
      [(add1) (+ (1st args) 1)]
      [(sub1) (- (1st args) 1)]
      [(zero?) (if (valid-zero? args) (zero? (car args)))]
      [(not) (apply not args)]
      [(cons) (cons (1st args) (2nd args))]
      [(=) (apply = args)]
      [(<) (apply < args)]
      [(>) (apply > args)]
      [(<=) (apply <= args)]
      [(>=) (apply >= args)]
      [(car) (apply car args)]
      [(cdr) (apply cdr args)]
      [(caar) (apply caar args)]
      [(cadr) (apply cadr args)]
      [(cdar) (apply cdar args)]
      [(cddr) (apply cddr args)]
      [(caaar) (apply caaar args)]
      [(caadr) (apply caadr args)]
      [(cadar) (apply cadar args)]
      [(caddr) (apply caddr args)]
      [(cdaar) (apply cdaar args)]
      [(cdadr) (apply cdadr args)]
      [(cddar) (apply cddar args)]
      [(cdddr) (apply cdddr args)]
      [(list) (apply list args)]
      [(append) (apply append args)]
      [(null?) (apply null? args)]
      [(assq) (apply assq args)]
      [(eq?) (apply eq? args)]
      [(eqv?) (apply eqv? args)]
      [(equal?) (apply equal? args)]
      [(atom?) (apply atom? args)]
      [(length) (apply length args)]
      [(list->vector) (apply list->vector args)]
      [(list?) (apply list? args)]
      [(pair?) (apply pair? args)]
      [(procedure?) (proc-val? (car args))]
      [(vector->list) (apply vector->list args)]
      [(vector) (apply vector args)]
      [(make-vector) (apply make-vector args)]
      [(vector-ref) (apply vector-ref args)]
      [(vector?) (apply vector? args)]
      [(number?) (apply number? args)]
      [(symbol?) (apply symbol? args)]
      [(set-car!) (apply set-car! args)]
      [(set-cdr!) (apply set-cdr! args)]
      [(vector-set!) (apply vector-set! args)]
      [(display) (apply display args)]
      [(newline) (apply newline args)]
      [(map) (map (lambda (x) (apply-proc (car args) (list x))) (cadr args))]
      [(list-tail) (list-tail (car args) (cadr args))]
      [(apply) (apply-proc (car args) (cadr args))]
      [(member?) (apply member? (car args) (cdr args))]
      [(quotient) (apply quotient args)]
      [(newline) (apply newline args)]
      [(display) (apply display args)]
      [(void) (void)]
      [else (error 'apply-prim-proc 
            "Bad primitive procedure name: ~s" 
            prim-op)])))

(define apply-prim-proc-cps
  (lambda (prim-proc args k)
    (case prim-proc
      [(+) (apply-k k (apply + args))]
      [(-) (apply-k k (apply - args))]
      [(*) (apply-k k (apply * args))]
      [(/) (apply-k k (apply / args))]
      [(add1) (apply-k k (+ (1st args) 1))]
      [(sub1) (apply-k k (- (1st args) 1))]
      [(zero?) (apply-k k (zero? (1st args)))]
      [(not) (apply-k k (apply not args))]
      [(cons) (apply-k k (cons (1st args) (2nd args)))]
      [(=) (apply-k k (apply = args))]
      [(<) (apply-k k (apply < args))]
      [(>) (apply-k k (apply > args))]
      [(<=) (apply-k k (apply <= args))]
      [(>=) (apply-k k (apply >= args))]
      [(car) (apply-k k (apply car args))]
      [(cdr) (apply-k k (apply cdr args))]
      [(caar) (apply-k k (apply caar args))]
      [(cadr) (apply-k k (apply cadr args))]
      [(cdar) (apply-k k (apply cdar args))]
      [(cddr) (apply-k k (apply cddr args))]
      [(caaar) (apply-k k (apply caaar args))]
      [(caadr) (apply-k k (apply caadr args))]
      [(cadar) (apply-k k (apply cadar args))]
      [(caddr) (apply-k k (apply caddr args))]
      [(cdaar) (apply-k k (apply cdaar args))]
      [(cdadr) (apply-k k (apply cdadr args))]
      [(cddar) (apply-k k (apply cddar args))]
      [(cdddr) (apply-k k (apply cdddr args))]
      [(list) (apply-k k (apply list args))]
      [(append) (apply-k k (apply append args))]
      [(null?) (apply-k k (apply null? args))]
      [(assq) (apply-k k (apply assq args))]
      [(eq?) (apply-k k (apply eq? args))]
      [(eqv?) (apply-k k (apply eqv? args))]
      [(equal?) (apply-k k (apply equal? args))]
      [(atom?) (apply-k k (apply atom? args))]
      [(length) (apply-k k (apply length args))]
      [(list->vector) (apply-k k (apply list->vector args))]
      [(list?) (apply-k k (apply list? args))]
      [(pair?) (apply-k k (apply pair? args))]
      [(procedure?) (apply-k k(proc-val? (car args)))]
      [(vector->list) (apply-k k (apply vector->list args))]
      [(vector) (apply k (apply vector args))]
      [(make-vector) (apply-k k (apply make-vector args))]
      [(vector-ref) (apply-k k (apply vector-ref args))]
      [(vector?) (apply-k k (apply vector? args))]
      [(number?) (apply-k k (apply number? args))]
      [(symbol?) (apply-k k (apply symbol? args))]
      [(set-car!) (apply-k k (apply set-car! args))]
      [(set-cdr!) (apply-k k (apply set-cdr! args))]
      [(vector-set!) (apply-k k (apply vector-set! args))]
      [(display) (apply-k k (apply display args))]
      [(newline) (apply-k k (apply newline args))]
      [(map) ;(map (lambda (x) (apply-proc (car args) (list x))) (cadr args))
       (map-cps 
         (lambda (x k)
           (apply-proc-cps (1st args) (list x) k)) 
         (2nd args) 
         (lambda (mapped-args)
           (apply-k k mapped-args)))]
      [(list-tail) 
       (apply-k (lambda (ls)
                  (list-tail (1st ls) (2nd ls))) args)]
      [(apply) ;(apply-proc (car args) (cadr args))
       (apply-proc-cps (1st args) (2nd args) (lambda (applied-args)
                                               (apply-k k applied-args)))]
      [(member?) (apply-k k (member? (car args) (cdr args)))]
      [(quotient) (apply-k k (apply quotient args))]
      [(newline) (apply-k k (apply newline args))]
      [(display) (apply-k k (apply display args))]
      [(void) (apply-k k (void))]
      [else (error 'apply-prim-proc 
            "Bad primitive procedure name: ~s" 
            prim-op)])))
                                                                       
(define rep      ; "read-eval-print" loop.
  (lambda ()
    (display "--> ")
    ;; notice that we don't save changes to the environment...
    ;(let ([answer (top-level-eval (parse-exp (read)))]) ; I think this should be eval-one-exp
    (let ([answer (eval-one-exp (read))])
      ;; TODO: are there answers that should display differently?
      (eopl:pretty-print answer) (newline)
      (rep))))  ; tail-recursive, so stack doesn't grow.

(define eval-one-exp
  (lambda (x) (top-level-eval (syntax-expand (parse-exp x)))))

 ; #error checking
 ;  ______                        _____ _               _    _             
 ; |  ____|                      / ____| |             | |  |_|            
 ; | |__   _ __ _ __ ___  _ __  | |    | |__   ___  ___| | ___ _ __   __ _ 
 ; |  __| | '__| '__/ _ \| '__| | |    | '_ \ / _ \/ __| |/ / | '_ \ / _` |
 ; | |____| |  | | | |_| | |    | |____| | | |  __/ |__|   <| | | | | |_| |
 ; |______|_|  |_|  \___/|_|     \_____|_| |_|\___|\___|_|\_\_|_| |_|\__, |
 ;                                                                    __/ |
 ;                                                                   |___/       

(define valid-lambda-list-exp?
  (lambda (lst)
    (cond
      [(< (length lst) 3)
       #f
       ;(eopl:error 'parse-exp "lambda expression: incorrect length: ~s" lst)
       ]
      [(not (list? (2nd lst)))
       #f
       ;(eopl:error 'parse-exp "lambda expression: arguments must be a proper list: ~s" (2nd lst))
       ]
      [(not (andmap symbol? (2nd lst)))
       #f
       ;(eopl:error 'parse-exp "lambda argument list: formals must be symbols: ~s" (2nd lst))
       ]
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

(define valid-zero?
  (lambda (num)
    (cond
      [(not (null? (cdr num))) (eopl:error 'zero? "incorrect argument count in call zero? ~s" lst)]
      [else #t]
      )))
  
; UNPARSER
  (define combine-lefts-rights
    (lambda (lefts rights)
      (map (lambda (x y)
             (list x y)) 
        lefts rights)))
  
  (define unparse-exp
  (lambda (exp)
    (cases expression exp
      [var-exp (id) id]
      [lit-exp (id) id]
      [quoted-exp (id) id]
      [if-else-exp (condition true false)
        (list 'if (unparse-exp condition) 
          (unparse-exp true) 
          (unparse-exp false))]
      [if-exp (condition true) 
        (list 'if (unparse-exp condition)
          (unparse-exp true))]
      [cond-exp (lefts rights else)
        (append 
          (list 'cond)
          (combine-lefts-rights lefts rights)
          'else (unparse-exp else))]
      [or-exp (exprs)
        (append (list 'or) (exprs))]
      [begin-exp (exprs) (list 'BEGIN)]
      [case-exp (cond lefts rights else) (list 'CASE)]
      [while-exp (condition exprs) (list 'WHILE)]
      [let-exp (vars declarations body)
        (let [[unparsed-body (map unparse-exp body)]
              [let-with-vars (list 'let
                               (map (lambda (x y) 
                                      (list x (unparse-exp y))) vars declarations))]]
          (append let-with-vars unparsed-body))
        ]
      [named-let-exp (name vars declarations body)
        (let [[unparsed-body (map unparse-exp body)]
              [let-with-vars (list 'let name
                               (map (lambda (x y) 
                                      (list x (unparse-exp y))) vars declarations))]]
          (append let-with-vars unparsed-body))
        ]
      [let*-exp (vars declarations body)
        (let [[unparsed-body (map unparse-exp body)]
               [let-with-vars (list 'let*
                                (map (lambda (x y) 
                                       (list x (unparse-exp y))) vars declarations))]]
          (append let-with-vars unparsed-body))
        ]
      [letrec-exp (vars declarations body)
        (let [[unparsed-body (map unparse-exp body)]
              [let-with-vars (list 'letrec
                               (map (lambda (x y) 
                                      (list x (unparse-exp y))) vars declarations))]]
          (append let-with-vars unparsed-body))
        ]
      [lambda-list-exp (id body)
        (let [[unparsed-body (map unparse-exp body)]
              [lambda-with-vars (list 'lambda id)]]
          (append lambda-with-vars unparsed-body))
        ]
      [lambda-ref-exp (ids refs bodies) (list 'LAMBDA-REF-EXP)]
      [lambda-single-exp (id body)
        (let [[unparsed-body (map unparse-exp body)]
              [lambda-with-vars (list 'lambda id)]]
          (append lambda-with-vars unparsed-body))
        ]
      [lambda-improper-exp (id other body)
        (let [[unparsed-body (map unparse-exp body)]
              [lambda-with-vars (list 'lambda (append id other))]]
          (append lambda-with-vars unparsed-body))
        ]
      [set!-exp (var expr)
        (list 'set! var
          (unparse-exp expr))]
      [define-exp (id expr)
        (list 'DEFINE-EXP)]
      [app-exp (rator rand)
        (let [[unparsed-rand (map unparse-exp rand)]
              [app-with-rator (list (unparse-exp rator))]]
        (append app-with-rator unparsed-rand))]
      )))
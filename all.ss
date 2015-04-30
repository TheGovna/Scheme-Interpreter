(load "chez-init.ss")

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
  [lambda-single-exp
    (id symbol?)
    (body (list-of expression?))]
  [lambda-improper-exp
    (id (list-of symbol?))
    (other symbol?)
    (body (list-of expression?))]
  [set-exp
    (var symbol?)
    (expr expression?)]
  [cond-exp
    (conditions (list-of expression?))
    (exprs (list-of expression?))
    (else expression?)]
  [or-exp
    (conditions (list-of expression?))]
  [begin-exp
    (exps (list-of expression?))]
  [app-exp
    (rator expression?)
    (rand (list-of expression?))])

(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
    (syms (list-of symbol?))
    (vals (list-of scheme-value?))
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

(define member?
  (lambda (obj list)
    (not (not (member obj list)))))

; Takes a let expression and converts it to lambdas
; Ex:
; > (let->application '(let [[a 1]
;                            [b 2]] 
;                        (let [[c 3]
;                              [d 4]]
;                          (+ a b c d))))
; > ((lambda (a b) ((lambda (c d) (+ a b c d)) 3 4)) 1 2)
(define let->application
  (lambda (expr)
    (cond
      [(or (symbol? expr) (not (eqv? 'let (car expr))))
       expr]
      [else
        (let [[args (map cadr (cadr expr))]
              [vars (map car (cadr expr))]] 
          (cons (list 'lambda vars (let->application (caddr expr))) args))
        ])))

(define nest-lets
  (lambda (vars other)
    (if (= (length vars) 1) (list 'let vars other)
        (list 'let (list (car vars)) (nest-lets (cdr vars) other)))))

; Takes a let* expression and converts it to lets
; Ex:
; > (let*->let '(let* [[a 1]
;                      [b (* a 2)]] 
;                 (+ a b)))
; > (let ([a 1]) (let ([b (* a 2)]) (+ a b)))
(define let*->let
  (lambda (expr)
    (let [[vars (cadr expr)]
          [other (caddr expr)]]
      (nest-lets vars other))))

; Takes a cond expression and extracts the conditions
(define get-cond-conditions
  (lambda (exp)
    (let loop [[exp exp]
               [result '()]]
      (cond
        [(= (length exp) 1) result]
        ;[else (loop (cdr exp) (cons (caar exp) result))]
        [else (loop (cdr exp) (append result (list (caar exp))))]
        ))))

; Takes a cond expression and extracts the expressions to execute if its respective condition is true
(define get-cond-exprs
  (lambda (exp)
    (let loop [[exp exp]
               [result '()]]
      (cond
        [(= (length exp) 1) result]
        [else (loop (cdr exp) (append result (list (cadar exp))))]
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
         (map parse-exp (get-cond-conditions (cdr datum)))
         (map parse-exp (get-cond-exprs (cdr datum)))
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
       (if (valid-set-exp? datum) 
           (set-exp
             (2nd datum)
             (parse-exp (3rd datum))))]
      [(eqv? (1st datum) 'begin)
       (begin-exp
         (map parse-exp (cdr datum)))]
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
      (extended-env-record syms vals env))))

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
    ;(display exp) (newline)
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
        (if-exp
          (syntax-expand condition)
          (syntax-expand true))]
      [named-let-exp (name vars declarations bodies) exp]
      [let-exp (vars declarations bodies)
        (syntax-expand
          (app-exp     
            (lambda-list-exp
              vars
              bodies)
            declarations))]
      [let*-exp (vars declarations bodies) exp]
      [letrec-exp (vars declarations bodies) exp]
      [lambda-list-exp (id bodies)
        (lambda-list-exp
          id
          (map syntax-expand bodies))] ; think this is right?      
      [lambda-single-exp (id bodies) exp]      
      [lambda-improper-exp (id other bodies) exp]
      [set-exp (var expr) 
        exp]
      [app-exp (rator rands)
        (begin ;(display exp) (newline) (display rands)
        (app-exp
          (syntax-expand rator)
          (map syntax-expand rands)))]
      [cond-exp (conditions expressions else)
;        (cond
;          [(and (null? conditions) (null? expressions))
;           (syntax-expand else)]
;          [else
;            (if-else-exp
;              (syntax-expand (car conditions))
;              (syntax-expand (car expressions))
;              (syntax-expand
;                (cond-exp
;                  (cdr conditions)
;                  (cdr expressions)
;                  else)))])
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
                               else))]))
        ]
      [begin-exp (exps)
        (app-exp (lambda-list-exp
                   '()
                   (map syntax-expand exps))
          '())]
      [or-exp (exps)
        (cond
          [(null? exps) #f]
          [(null? (cdr exps)) (syntax-expand (car exps))]
          [else
            (let-exp ([temp (syntax-expand (car exps))])
              (if-exp temp
                  temp
                  (or-exp (cdr exps))))
          ]
        )]  
      )))                                                                       

 ; #interpreter   
 ;  _____       _                           _            
 ; |_   _|     | |                         | |           
 ;   | |  _ __ | |_ ___ _ __ _ __  _ __ ___| |_ ___ _ __ 
 ;   | | | '_ \| __/ _ \ '__| '_ \| '__/ _ \ __/ _ \ '__|
 ;  _| |_| | | | ||  __/ |  | |_| | | |  __/ ||  __/ |   
 ; |_____|_| |_|\__\___|_|  | .__/|_|  \___|\__\___|_|   
 ;                          | |                          
 ;                          |_|                                                                                        

; top-level-eval evaluates a form in the global environment

(define top-level-eval
  (lambda (form)
    ; later we may add things that are not expressions.
    (eval-exp form (empty-env))))

; let's hope Claude only does this thing only in Chez Scheme
(define eval-bodies
  (lambda (bodies env)
    (let eval-bodies ([bodies bodies])
                      (if (null? (cdr bodies))
                          (eval-exp (car bodies) env)
                          (begin (eval-exp (car bodies) env) (eval-bodies (cdr bodies)))))))
    ;(let* [[reversed-bodies (reverse bodies)]
    ;       [result-bodies (map (lambda (x) (eval-exp x env)) reversed-bodies)]
    ;       [result (car result-bodies)]]
    ;  result)))

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
            (apply-env 
              global-env ; was init-env
              id
              (lambda (x) x)
              (lambda () (eopl:error 'apply-env ; procedure to call if id not in env
		          "variable not found in environment: ~s"
			   id)))
            ))]
      [quoted-exp (id) id]
      [if-else-exp (condition true false)
        (if (eval-exp condition env) 
            (eval-exp true env) 
            (eval-exp false env))]
      [if-exp (condition true)
        (if (eval-exp condition env) 
            (eval-exp true env) 
            (void))]
      [app-exp (rator rands)
        (let ([proc-value (eval-exp rator env)]
              [args (eval-rands rands env)])
          (apply-proc proc-value args))]
      [let-exp (vars declarations bodies)
        (let [[new-env
                (extend-env 
                  vars 
                  (map (lambda (x) (eval-exp x env)) declarations) 
                  env)]]
          (eval-bodies bodies new-env))] ; evaluate bodies in order, return last value
      [lambda-list-exp (id bodies)
        (closure id bodies env)]
      [lambda-single-exp (id bodies)
        (closure-single-arg id bodies env)]
      [lambda-improper-exp (ids other bodies)
        (closure-improper-args ids other bodies env)]
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

(define *prim-proc-names* '(+ - * / add1 sub1 zero? not cons = < > <= >= car cdr caar
                            cadr cdar cddr caaar caadr cadar caddr cdaar cdadr cddar
                            cdddr list null? assq eq? equal? atom? length list->vector
                            list? pair? procedure? vector->list vector make-vector
                            vector-ref vector? number? symbol? set-car! set-cdr!
                            vector-set! display newline map apply))

(define init-env         ; for now, our initial global environment only contains 
  (extend-env            ; procedure names.  Recall that an environment associates
     *prim-proc-names*   ;  a value (not an expression) with an identifier.
     (map prim-proc      
          *prim-proc-names*)
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
      [(null?) (apply null? args)]
      [(assq) (apply assq args)]
      [(eq?) (apply eq? args)]
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
      [(apply) (apply-proc (car args) (cadr args))]
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

(define valid-set-exp?
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
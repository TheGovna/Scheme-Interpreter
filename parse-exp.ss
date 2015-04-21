(define-datatype expression expression?
  [var-exp
    (id symbol?)]
  [lit-exp
    (id (lambda (x) 
          (or (number? x) (string? x) (boolean? x) (null? x) (vector? x))))] ; what about quoted lists?
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

; --------------------------------------------------------------------

; Helpers

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

(define parse-exp         
  (lambda (datum)
    (cond
      [(symbol? datum) (var-exp datum)]
      [((lambda (x) (or (number? x) (string? x) (boolean? x) (null? x) (vector? x))) datum) (lit-exp datum)]
      [(pair? datum)
       (cond
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
         [else
           (if (valid-app-exp? datum) 
               (app-exp 
                 (parse-exp (1st datum))
                 (map parse-exp (cdr datum))))]
         )]
      [else (eopl:error 'parse-exp "bad expression: ~s" datum)])))


; --------------------------------------------------------------------

; Error checking

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
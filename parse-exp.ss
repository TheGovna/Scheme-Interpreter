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
             (lambda-list-exp 
               (2nd datum)
               (map parse-exp (cddr datum)))]
            [(symbol? (2nd datum)) ; single arg
             (lambda-single-exp
               (2nd datum)
               (map parse-exp (cddr datum)))]
            [else ; improper list
              (lambda-improper-exp
                (get-all-but-last-improper (2nd datum))
                (get-last-improper (2nd datum))
                (map parse-exp (cddr datum)))]
            )]
         [(eqv? (1st datum) 'if)
          (if 
              (eqv? (length datum) 4) 
              (if-else-exp ; if-then-else
                (parse-exp (2nd datum)) 
                (parse-exp (3rd datum)) 
                (parse-exp (4th datum))) 
              (if-exp ; if-then
                (parse-exp (2nd datum))
                (parse-exp (3rd datum))))]
         [(eqv? (1st datum) 'let)
          (if
            (symbol? (2nd datum)) ; named let
            (named-let-exp
              (2nd datum)
              (map 1st (3rd datum))
              (map (lambda (x) (parse-exp (2nd x))) (3rd datum))
              (map parse-exp (cdddr datum)))
            (let-exp ; normal let
              (map 1st (2nd datum))
              (map (lambda (x) (parse-exp (2nd x))) (2nd datum))
              (map parse-exp (cddr datum))))]
         [(eqv? (1st datum) 'let*) 
          (let*-exp
            (map 1st (2nd datum))
            (map (lambda (x) (parse-exp (2nd x))) (2nd datum))
            (map parse-exp (cddr datum)))]
         [(eqv? (1st datum) 'letrec) 
          (letrec-exp
            (map 1st (2nd datum))
            (map (lambda (x) (parse-exp (2nd x))) (2nd datum))
            (map parse-exp (cddr datum)))]
         [(eqv? (1st datum) 'set!)
          (set!-exp
            (2nd datum)
            (parse-exp (3rd datum)))]
         [else
           (app-exp 
                 (parse-exp (1st datum))
                 (map parse-exp (cdr datum)))]
         )]
      [else (eopl:error 'parse-exp "bad expression: ~s" datum)])))

(define unparse-exp
  (lambda (exp)
    (cases expression exp
      [var-exp (id) id]
      [lit-exp (id) id]
      [if-else-exp (condition true false)
        (list 'if (unparse-exp condition) 
          (unparse-exp true) 
          (unparse-exp false))]
      [if-exp (condition true) 
        (list 'if (unparse-exp condition)
          (unparse-exp true))]
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
      [app-exp (rator rand)
        (let [[unparsed-rand (map unparse-exp rand)]
              [app-with-rator (list (unparse-exp rator))]]
        (append app-with-rator unparsed-rand))]
      )))
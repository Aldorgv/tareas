#lang plait

(print-only-errors #t)


(define-type Value
  [numV (n : Number)]
  [strV (s : String)]
  [boolV (b : Boolean)])

(define-type ExprC
 
  (numC [n : Number])
  (strC [s : String])
  (boolC [b : Boolean])
  (plusC [l : ExprC] [r : ExprC])
  (appC [l : ExprC] [r : ExprC])
  (nEqC [l : ExprC] [r : ExprC])
  (sEqC [l : ExprC] [r : ExprC])
  (multC [l : ExprC] [r : ExprC])
  (ifC [a : ExprC] [b : ExprC] [c : ExprC])
  (orC [a : ExprC] [b : ExprC])
  (andC [a : ExprC] [b : ExprC])
  (zeropC [e : ExprC])
  (idC [name : Symbol])
  (letC [name : Symbol]
        [value : ExprC]
        [body : ExprC])
  (funC [name : Symbol]
        [value : ExprC]
        [body : ExprC]))


(define (bad-arg-to-op-error [op : Symbol] [v : Value])
  (error 'interp "argumento incorrecto"))

(define (bad-conditional-error [v : Value])
  (error 'interp "no es un valor booleano"))

(define (unbound-identifier-error [name : Symbol])
  (error 'interp
         (string-append
          "unbound identifier: "
          (to-string name))))

(define-type Binding
  (binding [name : Symbol]
           [value : Value]))

 (define-type-alias Environment (Symbol -> Value))

 (define (empty-env [name : Symbol]) : Value
   (unbound-identifier-error name))

 (define (lookup-env name env)
   (env name))

 (define (extend-env name val env)
   (lambda (id)
     (if (eq? id name)
         val
         (env id))))

(define (interp [e : ExprC] [env : Environment]) : Value
  (type-case ExprC e
    
    [(numC n) (numV n)]
    [(boolC b) (boolV b)] 
    [(strC s) (strV s)]
    
    [(nEqC e1 e2)
     (cond [(not (numV? (interp e1 env)))
            (bad-arg-to-op-error '+ (interp e1 env))]
           [(not (numV? (interp e2 env)))
            (bad-arg-to-op-error '+ (interp e2 env))]
           [else
            (boolV (=(numV-n (interp e1 env))(numV-n (interp e2 env)))) ])]
    
    [(sEqC s1 s2)
     (cond [(not (strV? (interp s1 env)))
            (bad-arg-to-op-error '+ (interp s1 env))]
           [(not (strV? (interp s2 env)))
            (bad-arg-to-op-error '+ (interp s2 env))]
           [else
            (boolV (equal?(strV-s (interp s1 env))(strV-s (interp s2 env)))) ])]
   
    
    [(plusC e1 e2)
     (let ([v1 (interp e1 env)]
           [v2 (interp e2 env)])
       (cond [(not (numV? v1))
              (bad-arg-to-op-error '+ v1)]
             [(not (numV? v2))
              (bad-arg-to-op-error '+ v2)]
             [else
              (numV (+ (numV-n v1) (numV-n v2)))]))]
    
    [(appC e1 e2)
     (let ([v1 (interp e1 env)]
           [v2 (interp e2 env)])
       (cond [(not (strV? v1))
              (bad-arg-to-op-error '+ v1)]
             [(not (strV? v2))
              (bad-arg-to-op-error '+ v2)]
             [else
              (strV (string-append (strV-s v1)(strV-s v2)))]))]
    
    [(multC e1 e2)
     (let ([v1 (interp e1 env)]
           [v2 (interp e2 env)])
       (cond [(not (numV? v1))
              (bad-arg-to-op-error '* v1)]
             [(not (numV? v2))
              (bad-arg-to-op-error '* v2)]
             [else
              (numV (* (numV-n v1) (numV-n v2)))]))]
    
    [(ifC e1 e2 e3)
     (let ([v1 (interp e1 env)])
       (cond [(not (boolV? v1))
              (bad-conditional-error v1)]
             [(boolV-b v1)
              (interp e2 env)]
             [else
              (interp e3 env)]))]

    [(orC e1 e2)
     (let ([v1 (interp e1 env)]
           [v2 (interp e2 env)])
       (cond [(not (boolV? v1))
              (bad-conditional-error v1)]
             [(not (boolV? v2))
              (bad-conditional-error v2)]
             [else
              (boolV (or (boolV-b v1) (boolV-b v2)))]))]

    [(andC e1 e2)
     (let ([v1 (interp e1 env)]
           [v2 (interp e2 env)])
       (cond [(not (boolV? v1))
              (bad-conditional-error v1)]
             [(not (boolV? v2))
              (bad-conditional-error v2)]
             [else
              (boolV (and (boolV-b v1) (boolV-b v2)))]))]
    
    [(zeropC e)
     (let ([v (interp e env)])
       (cond [(not (numV? v))
              (bad-arg-to-op-error 'zero? v)]
             [else
              (boolV (= (numV-n v) 0))]))]
    
    [(idC name)
     (lookup-env name env)]
    
    [(letC name value body)
     (interp body (extend-env name (interp value env) env))]

    [(funC name value body)
     (interp body (extend-env name (interp value env) env))]
    ))


(define-type ExprS
  (numS [n : Number])
  (boolS [b : Boolean])
  (strS [s : String])
  (plusS [l : ExprS] [r : ExprS])
  (appS [l : ExprS] [r : ExprS])
  (nEqS [l : ExprS] [r : ExprS])
  (sEqS [l : ExprS] [r : ExprS])
  (multS [l : ExprS] [r : ExprS])
  (bminusS [l : ExprS] [r : ExprS])
  (uminusS [e : ExprS])
  (orS [l : ExprS] [r : ExprS])
  (andS [l : ExprS] [r : ExprS])
  (notS [e : ExprS])
  (ifS [a : ExprS] [b : ExprS] [c : ExprS])
  (zeropS [e : ExprS])
  (idS [name : Symbol])
  (letS [name : Symbol]
        [value : ExprS]
        [body : ExprS])
  (funS [name : Symbol]
        [value : ExprS]
        [body : ExprS]))


(define (desugar [e : ExprS]) : ExprC
  (type-case ExprS e
    [(numS n) (numC n)]
    [(boolS b) (boolC b)]
    [(strS s) (strC s)]
    [(plusS e1 e2) (plusC (desugar e1) (desugar e2))]
    [(appS e1 e2) (appC (desugar e1) (desugar e2))]
    [(multS e1 e2) (multC (desugar e1) (desugar e2))]
    [(nEqS e1 e2)  (nEqC (desugar e1) (desugar e2))]
    [(sEqS e1 e2)  (sEqC (desugar e1) (desugar e2))]
    [(bminusS e1 e2) (plusC (desugar e1) (multC (numC -1) (desugar e2)))]
    [(uminusS e) (multC (numC -1) (desugar e))]
    [(orS e1 e2) (orC (desugar e1) (desugar e2))]
    [(andS e1 e2) (andC (desugar e1) (desugar e2))]
    [(notS e) (ifC (desugar e) (boolC #f) (boolC #t))]
    [(ifS a b c) (ifC (desugar a) (desugar b) (desugar c))]
    [(zeropS e) (zeropC (desugar e))]
    [(idS name) (idC name)]
    [(letS name value body) (letC name (desugar value) (desugar body))]
    [(funS name value body) (letC name (desugar value) (desugar body))]))


#|
(define (parse-error e)
  (error 'parse (string-append "malformed-input: " (to-string e))))
|#
(define (parse-error e)
  (error 'parse "expresión malformada"))

(define (parse [in : S-Exp]) : ExprS
  (cond
    [(s-exp-number? in) (parse-number in)]
    [(s-exp-match? `true in) (boolS #t)]
    [(s-exp-match? `false in) (boolS #f)]
    [(s-exp-string? in) (parse-string in)]
    [(s-exp-list? in)
     (let ([ls (s-exp->list in)])
       (cond [(empty? ls)
              (parse-error ls)]
             [else
              (let ([tag (first ls)])
                (cond [(s-exp-symbol? tag)
                       (case (s-exp->symbol tag)

                         [(+)
                          (if (= (length ls) 3)
                              (plusS (parse (second ls))
                                     (parse (third ls)))
                              (parse-error ls))]
                         [(++)
                          (if (= (length ls) 3)
                              (appS (parse (second ls))
                                    (parse (third ls)))
                              (parse-error ls))]
                         [(num=)
                          (if (= (length ls) 3)
                              (nEqS (parse (second ls))
                                    (parse (third ls)))
                              (parse-error ls))]
                         [(str=)
                          (if (= (length ls) 3)
                              (sEqS (parse (second ls))
                                    (parse (third ls)))
                              (parse-error ls))]
  
                         [(*)
                          (if (= (length ls) 3)
                              (multS (parse (second ls))
                                     (parse (third ls)))
                              (parse-error ls))]
                         [(-)
                          (let ([len (length ls)])
                            (cond [(= len 2)
                                   (uminusS (parse (second ls)))]
                                  [(= len 3)
                                   (bminusS (parse (second ls))
                                            (parse (third ls)))]
                                  [else
                                   (parse-error ls)]))]
                         [(or)
                          (let ([len (length ls)])
                            (if (= len 3)
                                (orS (parse (second ls))
                                     (parse (third ls)))
                                (parse-error ls)))]
                         [(and)
                          (let ([len (length ls)])
                            (if (= len 3)
                                (andS (parse (second ls))
                                      (parse (third ls)))
                                (parse-error ls)))]
                         [(not)
                          (let ([len (length ls)])
                            (if (= len 2)
                                (notS (parse (second ls)))
                                (parse-error ls)))]
                         [(if)
                          (let ([len (length ls)])
                            (if (= len 4)
                                (ifS (parse (second ls))
                                     (parse (third ls))
                                     (parse (fourth ls)))
                                (parse-error ls)))]
                         [(zero?)
                          (let ([len (length ls)])
                            (if (= len 2)
                                (zeropS (parse (second ls)))
                                (parse-error ls)))]
                         [(fun)
                          (let ([len (length ls)])
                            (if (= len 3)
                                (let ([binding (second ls)]
                                      [body (third ls)])
                                  (if (s-exp-list? binding)
                                      (let ([binding (s-exp->list binding)])
                                        (if (= (length binding) 2)
                                            (let ([name (first binding)]
                                                  [value (second binding)])
                                              (if (s-exp-symbol? name)
                                                  (letS (s-exp->symbol name)
                                                        (parse value)
                                                        (parse body))
                                                  (parse-error ls)))
                                            (parse-error ls)))
                                      (parse-error ls)))
                                (parse-error ls)))]
                         
                         [(let)
                          (let ([len (length ls)])
                            (if (= len 3)
                                (let ([binding (second ls)]
                                      [body (third ls)])
                                  (if (s-exp-list? binding)
                                      (let ([binding (s-exp->list binding)])
                                        (if (= (length binding) 2)
                                            (let ([name (first binding)]
                                                  [value (second binding)])
                                              (if (s-exp-symbol? name)
                                                  (letS (s-exp->symbol name)
                                                        (parse value)
                                                        (parse body))
                                                  (parse-error ls)))
                                            (parse-error ls)))
                                      (parse-error ls)))
                                (parse-error ls)))])]
                      [else
                       (parse-error tag)]))]))]
    [(s-exp-symbol? in)
     (idS (s-exp->symbol in))]
    [else (parse-error in)]))

(define (parse-number in)
  (numS (s-exp->number in)))

(define (parse-string in)
  (strS (s-exp->string in)))

(define (eval [in : S-Exp]) : Value
  (interp (desugar (parse in)) empty-env))
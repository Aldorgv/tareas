#lang racket
;;Escribe aquí tus soluciones
;-------------------------Problema 1------------------------------- 
(define (countdown n)
  
  (if (= n 0)
      (list 0)
      (append (list n) (countdown (- n 1)))
   )
)
;-------------------------Problema 2-------------------------------
(define (insertL s1 s2 ls)

  (if(empty? ls)
     ls
     (if(eqv? s1 (first ls))
        (append (append(list s2)(list (first ls))) (insertL s1 s2 (rest ls)))
        (append (list (first ls)) (insertL s1 s2 (rest ls)))
      )
   )
)
;-------------------------Problema 3-------------------------------
(define (remv-1st s ls)
  (if (member s ls)
      (if (eqv? (first ls) s)
          (rest ls)
          (append (list(first ls)) (remv-1st s(rest ls)))
       )
      ls
   )
)
;-------------------------Problema 4------------------------------- 
(define (map p ls)
  (if (empty? ls)
      (list)
      (append (list(p (first ls))) (map p (rest ls)))
   )
)
;-------------------------Problema 5------------------------------- 
(define (filter p ls)
  (if (empty? ls)
      (list)
      (if(p(first ls))
         (append (list(first ls)) (filter p (rest ls)))
         (filter p (rest ls))
      )
   )
)
;-------------------------Problema 6-------------------------------++++++++++++++++++++++++++++++++++++

(define (zip ls1 ls2)

  (if(empty? ls1)
     null
     (append(list(append(list(first ls1)(first ls2))))(zip (rest ls1)(rest ls2)))
   )
)
;-------------------------Problema 7-------------------------------
(define (list-index-ofv e ls)

  (if (empty? ls)
      ls
      (if(eqv? e (first (reverse ls)))
         (length(rest (reverse ls)))
         (list-index-ofv e (rest (reverse ls)))
      )
   )
)
;-------------------------Problema 8-------------------------------




;-------------------------Problema 9-------------------------------
(define (rev ls)

  (if (empty? ls)
      ls
      (append (rev (rest ls))(list (first ls)))
   )
)
;-------------------------Problema 13------------------------------
(define (binary-natural ls)

  (if (empty? ls)
       0      
      (+ (*(first (reverse ls))(expt 2 (- (length (reverse ls)) 1))) (binary-natural (rest (reverse ls))))
   )
)
;-------------------------Problema 18------------------------------versión del profe
(define (powerset ls)

  (if(empty? ls)
     (list ls)
     (let([ps (powerset(rest ls))])
       (append (f(first ls)ps)ps))
   )
)
(define (f x ls)

  (if (null? ls)
      null
      (cons (cons x (first ls))
            (f x (rest ls)))
   )
)
(provide (all-defined-out))

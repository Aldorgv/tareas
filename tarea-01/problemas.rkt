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
     (append(list(cons(first ls1)(first ls2)))(zip (rest ls1)(rest ls2)))
     )
  )
;-------------------------Problema 7-------------------------------
(define (list-index e ls n)
  (if(eqv? e (first ls))
     n
     (list-index e (rest ls) (+ n 1))
     )
  )
(define (list-index-ofv e ls)
  (list-index e ls 0)
  )
;-------------------------Problema 8-------------------------------
(define (append2 ls1 ls2)
  (if (empty? ls1)
      ls2
      (cons (first ls1) (append2 (rest ls1) ls2))))
;-------------------------Problema 9-------------------------------
(define (rev ls)

  (if (empty? ls)
      ls
      (append (rev (rest ls))(list (first ls)))
      )
  )
;-------------------------Problema 10------------------------------
(define (repeat ls n)
  (if(= n 0)
     null
     (append ls (repeat ls (- n 1)))
     )
  )
;-------------------------Problema 11------------------------------
(define (same-lists* ls1 ls2)
  (if(and(empty? ls1)(empty? ls2))
     #t
     (if (=(length ls1) (length ls2))
         (if(equal? (first ls1) (first ls2))
            (same-lists* (rest ls1) (rest ls2))
            #f
            )
         #f
         )
     )
  )
;-------------------------Problema 12------------------------------
;(equal? '(a b)'(a . (b . ())))
;(equal? '(w x)'(w .... (x .... ())))
;(equal? '(z) '(....z))
;-------------------------Problema 13------------------------------
(define (binary->naturalr ls)

  (if (empty? ls)
      0
      (+ (*(first ls)(expt 2 (- (length ls) 1))) (binary->naturalr (rest ls)))
      )
  )

(define (binary->natural ls)
  (binary->naturalr (reverse ls))
  )

;-------------------------Problema 14------------------------------
(define (div n1 n2)
  
  (if (< n1 n2)
      0
      (+ (div (- n1 n2) n2) 1)))
;-------------------------Problema 15------------------------------
(define (union ls)

  (if(empty? ls)
     null
     (append (first ls) (union (rest ls)))
   )

)
(define (append-map arg1 arg2)
  (union(map arg1 (eval arg2)))
)
;-------------------------Problema 16------------------------------
(define(set-difference ls1 ls2)

  (if(empty? ls1)
     null
     (if(member(first ls1) ls2)   
        (set-difference(rest ls1) ls2)
        (append(list(first ls1))(set-difference(rest ls1) ls2))
        )
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
;-------------------------Problema 19------------------------------
(define(dist s ls)
  (if(empty? ls)
     null
     (append  (list(list s (first ls))) (dist s (rest ls)))
     )
  )
(define(cartesian-product ls)
  (define ls1 (first ls))
  (define ls2 (second ls))
  (if(empty? ls1)
     null
     (append(dist (first ls1) ls2)(cartesian-product (list (rest ls1)ls2)))
     
     )
  )
(define (snowball n) 1) 
 

(provide (all-defined-out))
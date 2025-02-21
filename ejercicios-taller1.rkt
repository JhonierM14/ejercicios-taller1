;;-----------------------------------------
;; Jhonier Mendez Bravo 202372226
;; David Santiago Guerrero Delgado 202324594
;;-----------------------------------------

#lang eopl

;; <List> ::= ()
;;        ::= (<list> <list>)
;;        ::= (<Pair> <list>)
;; <Pair> ::= ()
;;        ::= (<Scheme-Value> <<Scheme-Value>)
;;        ::= (<list> <list>)

(define invert
  (lambda (L)
    (cond
      [(null? L) empty]
      ;[(list? (car L)) (invert (car L))(invert (cdr L))]
      [else (cons (list (car(cdr(car L))) (car(car L))) (invert (cdr L)))]
    )
  )
)

;; Pruebas 
(invert '((a 1) (a 2) (b 1) (b 2)))
(invert '((5 9) (10 91) (82 7) (a e) ("Hola" "Mundo")))
(invert '(("es" "racket") ("genial" "muy") (17 29) (81 o)))

(define down
  (lambda (L)
    (cond
    [(null? L) empty]
    [else (cons (list (car L)) (down (cdr L)))]
    )
  )
)

;; Pruebas
(down '(1 2 3))
(down '((una) (buena) (idea)))
(down '(un (objeto (mas)) complicado))

(define list-set
  (lambda (L n x)
    (cond
      [(> (+ n 1) (length L)) "Error: no se pudo reemplazar el valor x en la posicion n"]
      [(null? L) empty]
      [(= n 0) (cons x (cdr L))]
      [else (cons (car L) (list-set (cdr L) (- n 1) x))]
    )
  )
)

;; Pruebas
(list-set '(a b c d) 2 '(1 2))
(list-set '(a b c d) 3 '(1 5 10))

(define filter-in
  ( lambda (P L)
    ()

  )
)

(define list-index
  ( lambda (P L)
    ()

  )
)

(define swapper
  ( lambda (E1 E2 L)
    ()

  )
)

(define cartesian-product
  ( lambda (L1 L2)
    ()

  )
)

(define mapping
  ( lambda (F L1 L2)
    ()

  )
)

(define inversions
  ( lambda (L)
    ()

  )
)

(define up
  ( lambda (L)
    ()

  )
)

(define insert
  ( lambda (L)
    ()

  )
)

(define zip
  ( lambda (L)
    ()

  )
)

(define filter-acum
  (lambda (a b F acum filter)
    (cond
      [(> a b) acum]
      [(filter a) (filter-acum (+ a 1) b F (F acum a) filter)]
      [else (filter-acum (+ a 1) b F acum filter)]
    )
  )
)

;; Pruebas 
(filter-acum 1 10 + 0 odd?)
(filter-acum 1 10 + 0 even?)
(filter-acum 1 10 - 0 odd?)
(filter-acum 1 10 - 0 even?)

(define (operate lrators lrands)
  ( 

  )
)

(define path
  ( lambda (n BST)
    ()

  )
)



(define (count-odd-and-even arbol)
  (

  )
)

(define (prod-scalar-matriz mat vec)
  (

  )
)


(define (Operar-binarias operacionB) 
  (

  )
)

(define (pascal N) 
  (

  )
)

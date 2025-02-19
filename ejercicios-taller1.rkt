;;-----------------------------------------
;; Jhonier Mendez Bravo 202372226
;; David Santiago Guerrero Delgado 202324594
;;-----------------------------------------

#lang eopl

; <List> ::= ()
;        ::= (<list> <list>)
;        ::= (<Pair> <list>)
; <Pair> ::= (<<Scheme-Value> <<Scheme-Value>)

(define invert
  (lambda (L)
    (cond
      [(null? L) empty]
      [(list? (car L)) (invert (car L))
                 (invert (cdr L))
                 ]
      [else (cons (list (car(cdr(car L))) (car(car L))) (invert (cdr L)))]
    )
  )
)

;; Pruebas 
(invert '((a 1) (a 2) (b 1) (b 2)))
(invert '((5 9) (10 91) (82 7) (a e) ("Hola" "Mundo")))
(invert '(("es" "racket") ("genial" "muy") (17 29) (81 o)))

(define down
  ( lambda (L)
    ()

  )
)

(define list-set
  ( lambda (L n x)
    ()

  )
)

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
  ( lambda (a b F acum filter)
    ()

  )
)

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

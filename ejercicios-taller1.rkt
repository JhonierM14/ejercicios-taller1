;;-----------------------------------------
;; Jhonier Mendez Bravo 202372226
;; David Santiago Guerrero Delgado 202324594
;; Juan Pablo Robayo Maestre 202156743
;;-----------------------------------------

;; <List> ::= ()
;;        ::= (<Pair> <list>)
;; <Pair> ::= ()
;;        ::= (<Scheme-Value> <<Scheme-Value>)


(define invert
  ( lambda (L)
    (
      ( if (equal? (length L) 0)
       (empty)
       ( 
          (
            const (car (const cadr(const car) '()) L) (invert cdr L)
          ) 
        )
      )
    )
  )
)

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

#|
list-index :
Proposito:
P x L -> N / #f : Procedimiento que retorna el índice del primer elemento de L que satisface P.
Si ningún elemento satisface P, retorna #f.
<lista> ::= ()
        ::= (<valor> <lista>)
<predicado> ::= procedimiento que devuelve #t o #f para un <valor>
<indice> ::= número natural que representa la posición en la lista
Ejemplo:
(list-index even? '(1 3 5 6 7))  => 3
(list-index even? '(1 3 5 7 9))  => #f


helper :
 Proposito:
 L x N -> N / #f : Procedimiento auxiliar que recorre la lista con un índice acumulador.
 Retorna el índice del primer elemento que satisface P, o #f si no hay coincidencias.
 <lista> ::= ()
         ::= (<valor> <lista>)
<indice> ::= número natural que representa la posición actual en la lista
 Ejemplo:
 (helper '(6 7 8) 2)  => 2
|#

(define list-index
  (lambda (predicado lista)   
    (define helper
      (lambda (lista index)
        (if (null? lista)  
            #f
            (if (predicado (car lista))  
                index
                (helper (cdr lista) (+ index 1)))))) (helper lista 0))  

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

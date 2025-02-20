#|

Jhonier Mendez Bravo 202372226
Juan Pablo Robayo Maestre 202156743

|#


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
  ( lambda (L)
    ()

  )
)

(define list-set
  ( lambda (L n x)
    ()

  )
)

#|
filter-in :
 Proposito:
 P x L -> L' : Procedimiento que retorna una lista con los elementos de L que cumplen el predicado P.
 <lista> ::= ()
         ::= (<valor> <lista>)
 <predicado> ::= procedimiento que devuelve #t o #f para un <valor>
 Ejemplo:
(filter-in even? '(1 2 3 4 5 6))  => '(2 4 6)
|#

(define filter-in
  (lambda (predicado lista)
    (if (null? lista)  
        '()
        (if (predicado (car lista))  
            (cons (car lista) (filter-in predicado (cdr lista)))  
            (filter-in predicado (cdr lista))))))

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

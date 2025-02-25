;;-----------------------------------------
;; Jhonier Mendez Bravo 202372226
;; David Santiago Guerrero Delgado 202324594
;; Juan Pablo Robayo Maestre 202156743
;;-----------------------------------------

;; invert :
;; Proposito:
;; S x L -> L’ : Procedimiento que intercambia la pocision
;; de los elementos dentro de un tupla
;;  
;; <List> ::= ()
;;        ::= (<Pair> <list>)
;; <Pair> ::= ()
;;        ::= (<Scheme-Value> <<Scheme-Value>)


(define invert
  (lambda (L)
    (if (null? L)
        '()
        (if (pair? (car L))
            (cons (list car(cdr (car L)) (car(car L))) (invert(cdr L)) )
            '()
            ))))

;; Pruebas 
(invert '((a 1) (a 2) (b 1) (b 2)))
(invert '((5 9) (10 91) (82 7) (a e) ("Hola" "Mundo")))
(invert '(("es" "racket") ("genial" "muy") (17 29) (81 o)))

;; dowm :
;; Proposito:
;; S x L -> L’ : Procedimiento que a cada elemento de una lista
;; lo inserta a una lista y lo inserta nuevamente en la pocision
;; en la que estaba el elemento
;;  
;; <List> ::= ()
;;        ::= (<Scheme-Value> <list>)

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


;; list-set :
;; Proposito:
;; S x L -> L’ : Procedimiento que inserta en la pocision n
;; de una lista un elemento x
;;  
;; <List> ::= ()
;;        ::= (<Scheme-Value> <list>)

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

;; <list> ::= ()
;;        ::= (<Scheme-Value> <list>)

(define aux
  (lambda (E L2)
    (if (null? L2)
        '()
        (cons (list E (car L2)) (aux E (cdr L2)))
        )))

(define cartesian-product
  ( lambda (L1 L2)
    (if (null? L1)
        '()
        (if (null? L2)
            '()
            ( cons (aux (car L1) L2) (cartesian-product (cdr L1) L2) )
     ))))

(cartesian-product '(a b c) '(x y))
(cartesian-product '(p q r) '(5 6 7))


;; <list> ::= ()
;;        ::= (<Scheme-Value> <list>)

(define mapping
  ( lambda (F L1 L2)
     ( if (null? L1)
          '()
          (if (= (F (car L1)) (car L2) )
                 (cons (list (car L1) (car L2)) (mapping F (cdr L1) (cdr L2)))
                 '()
                 ))))

; (mapping (lambda (d) (* d 2)) (list 1 2 3) (list 2 4 6))
; (mapping (lambda (d) (* d 3)) (list 1 2 2) (list 2 4 6))
; (mapping (lambda (d) (* d 2)) (list 1 2 3) (list 3 9 12))

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

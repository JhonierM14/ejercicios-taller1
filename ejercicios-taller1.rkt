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
            (cons (list (car(cdr (car L))) (car(car L))) (invert(cdr L)) )
            '()
            ))))

;; Pruebas 
(invert '((a 1) (a 2) (b 1) (b 2)))
(invert '((5 9) (10 91) (82 7) (a e) ("Hola" "Mundo")))
(invert '(("es" "racket") ("genial" "muy") (17 29) (81 o)))

;; dowm :
;; Proposito:
;; S x L -> L’ : Procedimiento que a cada elemento de una lista
;; lo inserta en una lista y lo ingresa nuevamente en la pocision
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

;; filter-in :
;; Proposito:
;; S x L -> L’ : Procedimiento que 
;;
;;  
;; <List> ::= ()
;;        ::= (<Scheme-Value> <list>)

(define filter-in
  ( lambda (P L)
    ()

  )
)

;; Pruebas :
(filter-in number? '(a 2 (1 3) b 7))
(filter-in symbol? '(a (b c) 17 foo))
(filter-in string? '(a b u "univalle" "racket" "flp" 28 90 (1 2 3)))

#|
list-index :
Proposito:
P x L -> N / #f : Procedimiento que retorna el índice del primer elemento de L que satisface P.
Si ningún elemento satisface P, retorna #f.
<lista> ::= ()
        ::= (<valor> <lista>)
<predicado> ::= procedimiento que devuelve #t o #f para un <valor>
<indice> ::= número natural que representa la posición en la lista

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

;; Pruebas :
(list-index even? '(1 3 5 6 7))  => 3
(list-index even? '(1 3 5 7 9))  => #f

;; swapper :
;; Proposito:
;; S x L -> L’ : Procedimiento que cambia cada caracter 
;; E1 por un caracter E2 de una lista y viceversa
;;
;;<lista> := ()
;;            := (<char> <lista>)
;;            := ((<char> <lista>) <lista>)

(define swapper 
  (lambda (E1 E2 L)
    ( cond 
        [(null? L) '()]
        [(equal? E2 (car L)) (cons E1 (swapper E1 E2 (cdr L) ) )]
        [(equal? E1 (car L)) (cons E2 (swapper E1 E2 (cdr L) ) )]
        [else (cons (car L) (swapper E1 E2 (cdr L)))]
)))

;; Pruebas :
(swapper 'a 'd '(a b c d))
(swapper 'a 'd '(a d () c d))
(swapper 'x 'y '(y y x y x y x x y))

;; cartesian-product :
;; Proposito:
;; S x L -> P : Procedimiento que retorna el producto
;; cartesiano de una lista S y una lista L haciendo uso
;; de una funcion auxiliar
;;
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

;; Pruebas
(cartesian-product '(a b c) '(x y))
(cartesian-product '(p q r) '(5 6 7))

;; mapping :
;; Proposito:
;; S x L -> L’ : Procedimiento que aplica una operacion S
;; a cada elemento  de L.
;;
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

;; Pruebas :
(mapping (lambda (d) (* d 2)) (list 1 2 3) (list 2 4 6))
(mapping (lambda (d) (* d 3)) (list 1 2 2) (list 2 4 6))
(mapping (lambda (d) (* d 2)) (list 1 2 3) (list 3 9 12))

;; inversions :
;; Proposito:
;; S x L -> L’ : Procedimiento que 
;;
;;
;; <list> ::= ()
;;        ::= (<Scheme-Value> <list>)

(define inversions
  ( lambda (L)
    ()

  )
)

;; Pruebas :

;; up :
;; Proposito:
;; S x L -> L’ : Procedimiento que 
;;
;;
;; <list> ::= ()
;;        ::= (<Scheme-Value> <list>)

(define up
  ( lambda (L)
    ()

  )
)

;; Pruebas :

;; zip :
;; Proposito:
;; S x L x F -> L' : Procedimiento que aplica una operacion S
;; a cada elemento de pocision i de una lista L y F
;;
;;<lista> := ()
;;            := (<int> <lista>)

(define zip
   (lambda (F L1 L2 )
      (if (null? L1) 
       '() 
        (cons (F (car L1) (car L2)) (zip F (cdr L1) (cdr L2) ))
)))

;; Pruebas :
(zip + '(1 4) '(6 2))
(zip * '(11 5 6) '(10 9 8))

;; filter-acum :
;; Proposito:
;; S x L -> L’ : Procedimiento que 
;;
;;
;;<lista> := ()
;;        := (<int> <lista>)

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

;; operate :
;; Proposito:
;; S x L -> L’ : Procedimiento que 
;;
;; <operadores> := ()
;; <operadores> := (<operador> <list>)
;; <list> := ()
;; <list> := (<int> <list>)

(define (operate lrators lrands)
  ( 

  )
)

;; Pruebas :

;; path :
;; Proposito:
;; S x L -> L’ : Procedimiento que retorna el camino para llegar 
;; a un elemento S de un arbol binario L
;;
;;<lista> := ()
;;        := (<int> <lista>)

(define path 
   (lambda (n BTS)
      (cond
       [(null? BTS) '()]
       [(= n (car BTS)) '()]
       [(< n (car BTS) ) (cons 'left (path n (car (cdr BTS))))]
       [(> n (car BTS) ) (cons 'right (path n (car (cdr (cdr BTS)))))]
       )
))

;; Pruebas :
(path 17 '(14 (7 () (12 () ()))
(26 (20 (17 () ())
())
(31 () ()))))

;; <list> := ()
;; <list> := (<int> <list>)
;; <list> := (<list> <list>)

(define (count-odd-and-even arbol)
  (

  )
)

;; Operar-binarias :
;; Proposito:
;; S x L -> L’ : Procedimiento que 
;;
;; <OperacionB>::= <int>
;;             ::= (<OperacionB> 'suma <OperacionB>)
;;             ::= (<OperacionB> 'resta <OperacionB>)
;;             ::= (<OperacionB> 'multiplica <OperacionB>)

(define (Operar-binarias operacionB) 
  (

  )
)

;; Pruebas

;; prod-scalar-matriz :
;; Proposito: 
;; S x L -> S' : Procedimiento que multiplica una matriz S 
;; por un vector L
;;<lista> := ()
;;        := (<int> <lista>)

(define aux
  (lambda (fila vec)
    (if (null? fila) 
    '()
     (cons (* (car fila) (car vec) ) (aux (cdr fila) (cdr vec) ))
)))

 (define (prod-scalar-matriz mat vec)
   (if (null? mat)
   '()
    (cons (aux (car mat) vec) (prod-scalar-matriz (cdr mat) vec))
))

;; Pruebas :
(prod-scalar-matriz '((1 1) (2 2)) '(2 3))
(prod-scalar-matriz '((1 1) (2 2) (3 3)) '(2 3))

;; pascal :
;; Proposito:
;; S x L -> L’ : Procedimiento que 
;;

(define (pascal N) 
  (

  )
)

;; Pruebas :

;;-----------------------------------------
;; Jhonier Mendez Bravo 202372226
;; David Santiago Guerrero Delgado 202324594
;; Juan Pablo Robayo Maestre 202156743
;;-----------------------------------------

#lang eopl

;;--------------------------------Punto 1--------------------------------

;; invert :
;; Proposito:
;; L -> L' : Procedimiento que invierte cada par ordenado 
;; x, y perteneciente a una lista L
;;  
;; <List> ::= ()
;;        ::= (<Pair> <list>)
;; <Pair> ::= (<Scheme-Value> <Scheme-Value>)

(define invert
  (lambda (L)
    (if (null? L) '() (cons (list (car (cdr (car L))) (car (car L))) (invert(cdr L))))
  )
)

;; Pruebas 
(invert '((a 1) (a 2) (b 1) (b 2)))
(invert '((5 9) (10 91) (82 7) (a e) ("Hola" "Mundo")))
(invert '(("es" "racket") ("genial" "muy") (17 29) (81 o)))

;;--------------------------------Punto 2--------------------------------

;; dowm :
;; Proposito:
;; L -> L' : Procedimiento que a cada elemento de una lista L
;; lo inserta en una lista y lo ingresa nuevamente en la pocision
;; en la que estaba el elemento
;;  
;; <List> ::= ()
;;        ::= (<elemento> <List>)
;; <elemento> ::= <List> | <Scheme-Value>

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

;;--------------------------------Punto 3--------------------------------

;; list-set :
;; Proposito:
;; L x n x a -> L' : Procedimiento que inserta en la pocision n
;; de una lista L un elemento a
;;  
;; <List> ::= ()
;;        ::= (<elemento> <List>)
;; <elemento> ::= <List> | <Scheme-Value>

(define list-set
  (lambda (L n x)
    (cond
      [(null? L) empty]
      [(= n 0) (cons x (cdr L))]
      [else (cons (car L) (list-set (cdr L) (- n 1) x))]
    )
  )
)

;; Pruebas
(list-set '(a b c d) 2 '(1 2))
(list-set '(a b c d) 3 '(1 5 10))
(list-set '((a b) b c d) 1 '(b a))

;;--------------------------------Punto 4--------------------------------

#|filter-in :
Proposito:
P x L -> lista : Procedimiento que a cada elemento de una 
lista L le verifica si cumple un predicado P

<List> ::= ()
       ::= (<elemento> <List>)
<elemento> ::= <Scheme-Value> | <List>
|#

(define filter-in
  (lambda (predicado lista)
    (if (null? lista)  
        '()
        (if (predicado (car lista))  
            (cons (car lista) (filter-in predicado (cdr lista)))  
            (filter-in predicado (cdr lista))))))

;; Pruebas
(filter-in number? ’(a 2 (1 3) b 7))
(filter-in symbol? ’(a (b c) 17 foo))
(filter-in string? ’(a b u "univalle" "racket" "flp" 28 90 (1 2 3)))

;;--------------------------------Punto 5--------------------------------

#|
list-index :
Proposito:
P x L -> int / #f : Procedimiento que retorna el índice del 
primer elemento de L que satisface P. 
Si ningún elemento satisface P, retorna #f.

helper :
 Proposito:
 L x int -> int / #f : Procedimiento auxiliar que recorre la lista con 
 un índice acumulador. 

 Retorna el índice del primer elemento que satisface P, o #f si no 
 hay coincidencias.

<List> ::= ()
       ::= (<elemento> <List>)
<elemento> ::= <Scheme-Value> | <List>

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
(list-index even? '(1 3 5 6 7)) 
(list-index even? '(1 3 5 7 9))  
(list-index odd? '(2 4 6 7 8))

;;--------------------------------Punto 6--------------------------------

;; swapper :
;; Proposito:
;; E1 x E2 x L -> L’ : Procedimiento que cambia cada caracter 
;; E1 por un caracter E2 de una lista y viceversa
;;
;; <List> ::= ()
;;        ::= (<elemento> <List>)
;; <elemento> ::= <Scheme-Value> | <list>

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

;;--------------------------------Punto 7--------------------------------

;; cartesian-product :
;; Proposito:
;; L1 x L2 -> L1' : Procedimiento que retorna el producto
;; cartesiano de una lista L1 y una lista L2
;; haciendo uso de una funcion auxiliar
;;
;; cartesian-product-by-element :
;; Proposito:
;; E x L1 -> L1' : Procedimiento que retorna el producto
;; cartesiano de un elemento E y una lista L1
;;
;; <List> ::= ()
;;        ::= (<Scheme-Value> <List>)
;;
;; apend :
;; Propósito:
;; L1 x L2 -> L : Procedimiento que concatena dos listas sin 
;; usar la función append predefinida.
;;
;; Ejemplo:
;; (apend '(1 2 3) '(4 5 6)) => '(1 2 3 4 5 6)
;; (apend '() '(a b c)) => '(a b c)
;; (apend '(x y) '()) => '(x y)

(define apend
  (lambda (L1 L2)
    (if (null? L1)
      L2
    (cons (car L1) (apend (cdr L1) L2)))))

(define cartesian-product-by-element
  (lambda (E L2)
    (if (null? L2)
        '()
        (cons (list E (car L2)) (cartesian-product-by-element E (cdr L2)))
        )))

(define cartesian-product
  ( lambda (L1 L2)
    (if (null? L1)
        '()
        (if (null? L2)
            '()
            ( apend (cartesian-product-by-element (car L1) L2) (cartesian-product (cdr L1) L2) )
     ))))

;; Pruebas
(cartesian-product '(a b c) '(x y))
(cartesian-product '(p q r) '(5 6 7))
(cartesian-product '("Univalle") '("es" "genial"))

;;--------------------------------Punto 8--------------------------------

;; mapping :
;; Proposito:
;; S x L -> L’ : Procedimiento que aplica una operacion S
;; a cada elemento  de L.
;;
;; <List> ::= ()
;;        ::= (<int> <List>)

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

;;--------------------------------Punto 9--------------------------------

;; inversions :
;; Proposito:
;; L -> int : Procedimiento que cuenta la cantidad 
;; de veces que un elemento de pocision i es mayor 
;; que el resto de la lista L
;;
;; longitud :
;; Proposito:
;; L x a -> int : Procedimiento que cuenta la cantidad de 
;; elementos de una lista sin usar length
;;
;; <list> ::= ()
;;        ::= (<int> <List>)

;; Ejemplos:
;; (longitud '(1 2 4 5 6)) => 5
;; (longitud '()) => 0
;; (longitud '(1 11 9)) => 3

(define longitud
  (lambda (L a)
    (if (null? L) a (longitud (cdr L) (+ a 1)))
  )
)

(define inversions
  (lambda (L)
    (define aux
      (lambda (L l x)
        (cond
          [(null? L) x]
          [(= (longitud l 0) 1) (aux (cdr L) (cdr L) x)]
          [(> (car l) (cadr l)) (aux L (cons (car l) (cddr l)) (+ x 1))]
          [else (aux L (cons (car l) (cddr l)) x)]
        )
      )
    )
  (aux L L 0)
  )
)

;; Pruebas :
(inversions '(2 3 8 6 1))
(inversions '(1 2 3 4))
(inversions '(3 2 1))

;;--------------------------------Punto 10--------------------------------

#|
up :
Propósito:
L -> L' : Procedimiento que remueve un par de paréntesis 
a cada elemento del nivel más alto de la lista.

Si un elemento de este nivel no es una lista, se mantiene 
sin modificaciones.

<List> ::= ()
       ::= (<List> <List>)
       ::= (<Scheme-Value> <List>)
|#

(define up
  (lambda (L)
    (cond
      [(null? L) '()] 
      [(list? (car L)) (apend (car L) (up (cdr L)))] 
      [else (cons (car L) (up (cdr L)))])))

;; Pruebas :
(up '((1 2) (3 4)))
(up '((x (y)) z))
(up '((a (b)) (c)))

;;--------------------------------Punto 11--------------------------------

;; zip :
;; Proposito:
;; S x L x F -> L' : Procedimiento que aplica una operacion S
;; a cada elemento de pocision i de una lista L y F
;;
;; <List> ::= ()
;;        ::= (<int> <lista>)

(define zip
   (lambda (F L1 L2 )
      (if (null? L1) 
       '() 
        (cons (F (car L1) (car L2)) (zip F (cdr L1) (cdr L2) ))
)))

;; Pruebas :
(zip + '(1 4) '(6 2))
(zip * '(11 5 6) '(10 9 8))
(zip - '(3 5 11) '(1 2 8))

;;--------------------------------Punto 12--------------------------------

;; filter-acum :
;; Proposito:
;; intervalo x F x n x funcion -> int : Procedimiento que 
;; aplica una operación F a todos los valores de un intervalo  
;; [a, b] y que a su vez cumplen el predicado de la funcion filter
;; guardando el resultado en un valor n
;;
;; <list> ::= ()
;; <list> ::= (<int> <list>)

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

;;--------------------------------Punto 13--------------------------------

#|
operate :
Propósito:
L1 x L2 -> N : Procedimiento que aplica sucesivamente una lista
de funciones binarias sobre una lista de números.

<lista-operadores> ::= ()
                   ::= (<operador-binario> <lista-operadores>)
<lista-operandos>  ::= (<número> <lista-operandos>)

|#

(define operate
  (lambda (lrators lrands)
    (if (null? lrators)
        (car lrands)
        (operate (cdr lrators) (cons ((car lrators) (car lrands) (cadr lrands)) (cddr lrands))))))

;; Pruebas :
(operate (list + * + - *) '(1 2 8 4 11 6))
(operate (list *) '(4 5))
(operate (list * - +) '(3 10 4 6))

;;--------------------------------Punto 14--------------------------------

;; path :
;; Proposito:
;; S x L -> L’ : Procedimiento que retorna el camino para llegar 
;; a un elemento S de un arbol binario L
;;
;; <árbol-binario> ::= '()
;;                 ::= (<int> <árbol-binario> <árbol-binario>)

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

(path 31 '(14 (7 () (12 () ()))
(26 (20 (17 () ())
())
(31 () ()))))

;;--------------------------------Punto 15--------------------------------

;; count-odd-and-even :
;; Proposito:
;; L -> pair : Procedimiento que cuenta la cantidad de números
;; pares e impares en un árbol binario.
;;
;; <árbol-binario> ::= '()
;;                 ::= (<int> <árbol-binario> <árbol-binario>)

;; sumar-listas :
;; Proposito:
;; S x L -> L’ : Procedimiento que recibe dos listas de dos elementos 
;; y devuelve una nueva lista con la suma de sus elementos correspondientes.
;;
;; <Pair> ::= ()
;;        ::= (<int> <int>)

;; Ejemplos:
;; (sumar-listas '(3 4) '(3 4)) => (6 8)
;; (sumar-listas '(1 0) '(0 1)) => (1 1)
;; (sumar-listas '(11 2) '(3 4)) => (14 6)

(define sumar-listas
  (lambda (lista1 lista2)
    (list (+ (car lista1) (car lista2)) (+ (cadr lista1) (cadr lista2)))
  )
)

(define count-odd-and-even
  (lambda (arbol)
    (cond
      [(null? arbol) '(0 0)]
      [(number? arbol) (if (even? arbol) '(1 0) '(0 1))]
      [(pair? arbol) (sumar-listas (count-odd-and-even (car arbol)) (count-odd-and-even (cdr arbol)))]
    )
  )
)

;; Pruebas :
(count-odd-and-even '(14 (7 () (12 () ()))
                      (26 (20 (17 () ())
                            ())
                        (31 () ()))))

(count-odd-and-even '(2 (3 () ())
                      (19 (13 () ()) ()))

;;--------------------------------Punto 16--------------------------------

#|
Operar-binarias :
Propósito:
<OperacionB> -> <int> : Procedimiento que evalúa una expresión binaria representada como una lista anidada.

<OperacionB> ::= <int>
             ::= (<OperacionB> 'suma <OperacionB>)
             ::= (<OperacionB> 'resta <OperacionB>)
             ::= (<OperacionB> 'multiplica <OperacionB>)
|#

(define Operar-binarias
  (lambda (exp)
    (cond
      [(number? exp) exp] 
      [(list? exp) 
       (cond
         [(equal? (cadr exp) 'suma) (+ (Operar-binarias (car exp)) (Operar-binarias (caddr exp)))]
         [(equal? (cadr exp) 'resta) (- (Operar-binarias (car exp)) (Operar-binarias (caddr exp)))]
         [(equal? (cadr exp) 'multiplica) (* (Operar-binarias (car exp)) (Operar-binarias (caddr exp)))])])))
         
;; Pruebas
(Operar-binarias 4)
(Operar-binarias '(2 suma 9) )
(Operar-binarias '(2 resta 9) )
(Operar-binarias '(2 multiplica 9) )
(Operar-binarias '( (2 multiplica 3) suma (5 resta 1 ) ) )
(Operar-binarias '( (2 multiplica (4 suma 1) )
                      multiplica
                  ( (2 multiplica 4) resta 1 ) ) )

;;--------------------------------Punto 17--------------------------------

;; prod-scalar-matriz :
;; Proposito: 
;; S x L -> S' : Procedimiento que multiplica una matriz S 
;; por un vector L
;; <lista> ::= ()
;;         ::= (<int> <lista>)

(define multiplicar-fila
  (lambda (fila vec)
    (if (null? fila) 
    '()
     (cons (* (car fila) (car vec) ) (multiplicar-fila (cdr fila) (cdr vec) ))
)))

 (define (prod-scalar-matriz mat vec)
   (if (null? mat)
   '()
    (cons (multiplicar-fila (car mat) vec) (prod-scalar-matriz (cdr mat) vec))
))

;; Pruebas :
(prod-scalar-matriz '((1 1) (2 2)) '(2 3))
(prod-scalar-matriz '((1 1) (2 2) (3 3)) '(2 3))
(prod-scalar-matriz '((4 4) (2 2) (3 3)) '(1 2))

;;--------------------------------Punto 18--------------------------------

#|
pascal :
Propósito:
N -> L : Procedimiento que retorna la fila N del triángulo de Pascal.

<N> ::= número natural
<fila> ::= (<número> <fila>) | ()

generar-fila :
Propósito:
L -> L' : Procedimiento que recibe una fila del triángulo de Pascal y genera la siguiente fila.

<fila> ::= (<número> <fila>) | ()

Ejemplo:
(generar-fila '(1 1)) => '(1 2 1)
(generar-fila '(1 2 1)) => '(1 3 3 1)
|#

(define generar-fila
  (lambda (fila)
    (if (null? (cdr fila))
        '(1)  
        (cons (+ (car fila) (cadr fila)) (generar-fila (cdr fila))))))

(define pascal
  (lambda (n)
    (if (= n 1)
        '(1) 
        (cons 1 (generar-fila (pascal (- n 1)))))))

;; Pruebas :
(pascal 5)
(pascal 3)
(pascal 1)

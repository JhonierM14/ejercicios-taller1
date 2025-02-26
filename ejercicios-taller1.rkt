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

#|
up :
Propósito:
L -> L' : Procedimiento que remueve un par de paréntesis a cada elemento del nivel más alto de la lista.
Si un elemento de este nivel no es una lista, se mantiene sin modificaciones.

<lista> ::= ()
        ::= (<valor> <lista>)
        ::= ((<valor> <lista>) <lista>)

Ejemplo:
(up '((1 2) (3 4))) => '(1 2 3 4)
(up '((x (y)) z)) => '(x (y) z)

apend :
Propósito:
L1 x L2 -> L3 : Procedimiento que concatena dos listas sin usar la función append predefinida.

<lista> ::= ()
        ::= (<valor> <lista>)

Ejemplo:
(apend '(1 2 3) '(4 5 6)) => '(1 2 3 4 5 6)
(apend '() '(a b c)) => '(a b c)
(apend '(x y) '()) => '(x y)
|#

(define apend
  (lambda (L1 L2)
    (if (null? L1)
      L2
    (cons (car L1) (apend (cdr L1) L2)))))


(define up
  (lambda (L)
    (cond
      [(null? L) '()] 
      [(list? (car L)) (apend (car L) (up (cdr L)))] 
      [else (cons (car L) (up (cdr L)))]))) 



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

#|
operate :
Propósito:
L1 x L2 -> N : Procedimiento que aplica sucesivamente una lista de funciones binarias sobre una lista de números.

<lista-operadores> ::= ()
                   ::= (<operador-binario> <lista-operadores>)
<lista-operandos> ::= (<número> <lista-operandos>)

Ejemplo:
(operate (list + * + - *) '(1 2 8 4 11 6)) => 102
(operate (list *) '(4 5)) => 20
|#

(define operate
  (lambda (lrators lrands)
    (if (null? lrators)
        (car lrands)
        (operate (cdr lrators) (cons ((car lrators) (car lrands) (cadr lrands)) (cddr lrands))))))

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


#|
Operar-binarias :
Propósito:
<OperacionB> -> <int> : Procedimiento que evalúa una expresión binaria representada como una lista anidada.

<OperacionB> ::= <int>
              ::= (<OperacionB> 'suma <OperacionB>)
              ::= (<OperacionB> 'resta <OperacionB>)
              ::= (<OperacionB> 'multiplica <OperacionB>)

Ejemplo:
(Operar-binarias 4) => 4
(Operar-binarias '(2 suma 9)) => 11
(Operar-binarias '(2 resta 9)) => -7
(Operar-binarias '( (2 multiplica 3) suma (5 resta 1))) => 10
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


#|
pascal :
Propósito:
N -> L : Procedimiento que retorna la fila N del triángulo de Pascal.

<N> ::= número natural
<fila> ::= (<número> <fila>) | ()

Ejemplo:
(pascal 1) => '(1)
(pascal 2) => '(1 1)
(pascal 5) => '(1 4 6 4 1)


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



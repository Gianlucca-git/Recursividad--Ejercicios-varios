;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Ejercicios-Varios) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;;
;;<lista-de-simbolos> := ({<exp-simbolo>}*)
;;<exp-simbolo> := <lista-de-simbolos> | <posicion> | <lista-de-enteros>
;;
;;list-set : lista-de-simbolos * posicion * lista-de-enteros -> lista-de-simbolos
;;
;;Proposito:
;;Procedimiento que retorna una lista similar a lista, excepto que el n-simo elemento
;;(indexado desde cero) es x.
;;
;;primera


(define ( list-set  list1 number list2  )
  (if (>= number (length list1 )) "overflow" 
  
  (if (= number 0) (cons list2 (cdr list1 )) (cons (car list1 ) (list-set (cdr list1 )  (- number 1) list2 )
  ))))

;; pruebas
(write "EJERCICIO DE LISTA DE SIMBOLOS")(newline)
(write( list-set '(a b c d) 2 '(1 2)  ))(newline)
(write ( list-set '(a b c d) 3 '(1 5 10)))(newline)
(write( list-set '(a b c d) 8 '(3 2))) (newline)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;
;;<lista-de-enteros> := ({<exp-enteros>}*)
;;<exp-enteros> := <int>
;;
;;list-fact : int -> lista-de-enteros
;;
;;Proposito:
;;Procedimiento que recibe como parametro un entero n y terorna una lista incremental
;;de factorriales, comenzando en 1! hasta n!
;;

;;segunda
;;lista de factorial desde 1! hasta n!

(define fac  ( lambda (l) ( 
                            letrec (
 
                                    [f (lambda (a)    ( if (= a 0 ) 1  (* a (f (- a 1  )))  ))]
                                                            )
                             ;return
                          (f l )
                              )) 
                             )
                         
(define list-fact   (lambda (numero)  (letrec ([ aux   (lambda (cont  n ) (  if (> cont n ) empty 


                                        (cons (fac cont) (aux (+ cont 1 ) n ))))])
                                       (aux 1 numero)   ))
                                       )            
                                           

;;Pruebas
(write "EJERCICIO DE FACTORIAL")(newline)

(write ( list-fact  5 )) (newline)

(write ( list-fact   10 )) (newline)

(write ( list-fact   8 )) (newline)





;; <lista> := ({<exp-datos>}*)
;; <exp-datos> := Lista de listas
;;
;; up : lista -> lista
;;
;; Proposito:
;; Procedimiento que remueve un par de parentesis de cada elemento del nivel mas alto de lista.
;;
;;quinta

(define up
  (lambda (lista)
    (cond
      [(null? lista) empty]
      [(list? (car lista)) (append (car lista) (up (cdr lista)))]
      [else (append (list (car lista)) (up (cdr lista)))]
      )
    )
  )

;;Pruebas
(write "EJERCICIO DE REMOVER PARENTESIS")(newline)
(write(up '((1 2) (3 4))))(newline)
(write(up '((x (y)) z)))(newline)






;;
;;<lista-de-numeros> := ({<exp-numeros>}*)
;;<exp-numeros> := <lista-de-numeros>
;;
;; max-list : lista-de-numeros -> numero
;;
;; Proposito:
;; Procedimiento que recibe una lista de numeros y devuelve el numero mayor en dicha lista
;;
;;octava

(define max-list (lambda ( lista )
              { letrec ([primero (car lista)]

                        [recorrer (lambda (  lista  )
                                    {
                        if (null?   lista) primero
                        {
                         if ( > primero (car lista)) (recorrer  (cdr lista))  
                                                     (max-list  lista )}})])
                 (recorrer  (cdr lista ))}))

;;Pruebas
(write "EJERCICIO DE MAXIMO DE UNA LISTA")(newline)
(write ( max-list '(122222 4 221 5 34 2 1 88) )) (newline )
(write ( max-list '(1 4 2 88 2 88) )) (newline )
(write ( max-list '(1) )) (newline )

;;funcion que cuenta cuantos mayores existen en base a un puntero - inversions list
(define cuenta (lambda (lista ){
               letrec  ([punt (car lista )]
                        [punt2 (cdr lista )]
                        [funcion  (lambda (lista cont )

                           (   if (null?  lista ) cont
                           ( if (  > punt (car lista ) ) ( funcion (cdr lista)  (+ cont 1 ) )           
                                             ( funcion (cdr lista )  cont   ))))]
                       
                                      )
                 (funcion punt2 0)
                }
                    ))
;;funcion que cambia el puntero para enviarlo a "cuenta"
(define aux (lambda (lista ) (

         if (null? lista)  empty
        (cons (cuenta lista) (aux (cdr lista ))))))
  
    

(define inversions ( lambda ( lista ) (
                     letrec ([acc 0 ]
                             ;buble para sumar todos los que encontro mayores
                             [bucle (lambda (acc lista)
                     {  if (null? lista)  acc
                     (bucle  (+ acc (car (aux lista))) (cdr lista ) )})])
                      (bucle 0 lista ))))

                              
         
(write "EJERCICIO DE inversions list ")(newline)
(write (inversions   '(2 3 8 6 1 ) ))(newline)
(write ( inversions '(1 2 3 4) ))(newline)
(write ( inversions '(3 2 1) ))(newline)
;;;; laboratorio4.lisp
;;;; Disciplina de IA - 2022 / 2023
;;;; Laboratorio sobre funcoes recursivas e boas praticas de programacao em Lisp



;;; Exercicio Introdutorio.
;; comprimento de uma lista
(defun comprimento (lista)
"Recebe uma lista e retorna um valor inteiro que indica quantos elementos existem nesta lista"
  (cond
    ((null lista) 0)
    (t (1+ (comprimento (cdr lista))))
    )
  )
;; tamanho-das-sublistas
(defun tamanho-das-sublistas (lista)
"Recebe uma lista constituida por sublistas e retorna uma lista com valores inteiros que indicadam o comprimento das respetivas sublistas"
  (cond 
    ((null lista) nil)
    (T (cons (comprimento (car lista)) (tamanho-das-sublistas (cdr lista))))
  )
)

;;; Exercicio sobre funcoes recursivas

;; factorial
(defun factorial (n)
"Calcula o factorial de um número passado como argumento de entrada"
  (cond ((= n 1) 1)
        (T (* n (factorial (- n 1))))
  )
)

;; n-esimo
(defun n-esimo (position numberlist)
"Retorna o n-ésimo elemento de uma lista, ou seja, n-esimo replica o comportamento da
função Lisp nth: (nth n l)"
  (cond ((or (< position 0) (>= position (length numberlist))) nil)
        ((= position 0) (car numberlist))
        (T (n-esimo (1- position) (cdr numberlist)))
  )
)

;; soma-lista
(defun soma-lista (l)
"Calcula o valor correspondente à soma de todos os elementos de uma lista composta por
valores numéricos"
  (cond ((null l) 0)
        (T (+ (car l) (soma-lista (cdr l))))
  )
)

;; existe
(defun existe (n l)
"Verifica se um elemento procurado, passado como argumento de entrada, existe numa lista
passada como argumento de entrada, e caso exista devolve a cauda da lista em que esse elemento é a
cabeça; a função existe replica o comportamento da função Lisp member: (member o l)"
  (cond ((null l) nil)
        ((= n (car l)) l)
        (T (existe n (cdr l)))
  )
)

;; junta
(defun junta (l1 l2)
"Retorna a união entre duas listas, ou seja, junta replica o comportamento da função Lisp
append: (append l1 l2)"
  (cond ((null l1) l2)
        (T (cons (car l1) (junta (cdr l1) l2)))
  )
)

;; inverte
(defun inverte (l)
"Retorna a lista passada como argumento de entrada invertida, ou seja, inverte replica o
comportamento da função Lisp reverse: (reverse l)"
  (cond ((null l) '())
        (T (junta (inverte (cdr l)) (list (car l))))
  )
)

;; conta-atomos
(defun conta-atomos (l)
"Retorna o número de átomos que uma lista contém"
  (cond ((null l) 1)
        (T (1+ (conta-atomos(cdr l))))
  )
)

;; alisa
(defun alisa (l)
"Devolve todos os elementos de uma lista que poderá conter sub-listas, com todos os elementos
agregados numa única lista principal."
  (cond ((null l) '())
        ((listp (car l)) (alisa (junta (car l) (cdr l))))
        (T (cons (car l) (alisa (cdr l))))
  )
)
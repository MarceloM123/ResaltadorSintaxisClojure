(ns entregable3.core
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str])
  (:gen-class))

; Declare las funciones que se usaran en este codigo
(declare existe?, member?, acabo?, transicion, tokens, test-tokens, es-palRes?, span, htmlHaskell, comparacion)

; Parte que va siempre al incio de un archivo html
(def inicio "<!DOCTYPE html>
<html>
    
<head>
    <meta charset=\"UTF-8\">
    <title>Entregable3</title>
    <link rel=\"stylesheet\" href=\"haskell.css\">
</head>

<body>
<ul>
    <li>
        <span class=\"pR\">Palabras Reservadas</span>
    </li>
    <li>
        <span class=\"ide\">Identificadores</span>
    </li>
    <li>
        <span class=\"cte\">Constantes</span>
    </li>
    <li>
        <span class=\"ctef\">Constantes Flotantes</span>
    </li>
    <li>
        <span class=\"parBrac\">Parentesis y Brackets</span>
    </li>
    <li>
        <span class=\"comilla\">Quotes</span>
    </li>
    <li>
        <span class=\"sim\">Simbolos Especiales</span>
    </li>
    <li>
        <span class=\"com\">Comentarios</span>
    </li>
</ul>
    
<p> 
        ")

(let [wrtr (io/writer "/Users/marcelomarquezm/Desktop/Clojure/entregable3/src/entregable3/haskell.html")]
  (.write wrtr inicio)
  (.close wrtr))

  
; Parte que va al final de un archivo html
(def final "
</p>

</body>

</html>")

; O(1)
; Función para leer el archivo y separarlo caracter por caracter
(def lista-archivo (remove #{char} (slurp "/Users/marcelomarquezm/Desktop/Clojure/entregable3/src/entregable3/haskell.txt")))

; Automata de Haskell
(def automata '(((0 dig 1) (1 dig 1) (1 delim 101) (1 punto 2) (2 dig 2) (2 delim 100) (0 letra 3) (3 letra 3) (3 dig 3)
                           (3 sim 3) (3 delim 102) (0 com 4) (4 letra 4) (4 dig 4) (4 sim 4) (4 esp 4) (4 nL 103) (0 comilla 5)
                           (5 letra 5) (5 dig 5) (5 sim 5) (5 esp 5) (5 comilla 104) (0 sim 105) (0 parBrac 106) (0 delim 107))
                (100 101 102 103 104 105 106 107)))

; Palabras reservadas
(def palRes '("True" "False" "not" "let" "if" "then" "else" "do" "print" "apply" "map" "Int" "Integer" "Bool" "otherwise" "let" 
              "String" "where" "in" "filter" "do" "return" "Show" "Num" "Float"))

; Miembros
(def letras '(\a \b \c \d \e \f \g \h \i \j \k \l \m \n \o \p \q \r \s \t \u \v \w \x \y \z
              \A \B \C \D \E \F \G \H \I \J \K \L \M \N \O \P \Q \R \S \T \U \V \W \X \Y \Z))
(def dig '(\1 \2 \3 \4 \5 \6 \7 \8 \9 \0))
(def sim '(\+ \- \/ \* \= \: \^ \> \& \| \< \$ \# \, \?))
(def delim '(\space \newline \tab))
(def esp '(\space))
(def nL '(\newline))
(def parBrac '(\( \) \[ \]))
(def com '(\;))
(def comilla '(\"))
(def punto '(\.))

; O(1)
; Función de inicio para hacer las pruebas con el automata y la lista del archivo ** FUNCIONANDO **
(defn tokens [automata sec-archivo]
  (if (empty? (first automata))
    (println "ERROR: NO HAY AUTOMATA")
    (test-tokens automata sec-archivo (first (first (first automata))) nil)))

; O(n)
; Función donde enviamos el automata, lista del archivo, primer estado de transicion y nil como buffer
; Revisamos en que estatus esta la lista del archivo y revisar si los estados destino recibidos de la funcion de transicion
; Lentamente estaremos creando el string de la palabra/simbolo/numero/etc para que cuando llegemos a un estado aceptor, enviarlo a comparar
(defn test-tokens [automata sec-archivo estado fin]
  (cond
    (empty? sec-archivo) (acabo? estado automata)
    (existe? estado (first (rest automata))) 
    (test-tokens automata sec-archivo 0 (cond 
                                          (= " " fin) (comparacion nil " ")
                                          (= "\n" fin) (comparacion nil "\n")
                                          (= "\t" fin) (comparacion nil "\t")
                                          (existe? \newline fin) (do 
                                                                   (comparacion estado (apply str (remove #{\newline} fin)))
                                                                   (comparacion nil "\n"))
                                          (clojure.string/ends-with? fin " ") (do 
                                                                                (comparacion estado (clojure.string/trimr fin))
                                                                                (comparacion nil " "))
                                          (clojure.string/ends-with? fin "\t") (do
                                                                                 (comparacion estado (apply str (remove #{\tab} fin)))
                                                                                 (comparacion nil "\t"))
                                          :else (comparacion estado fin)))
    :else (test-tokens automata (rest sec-archivo) (transicion estado (first sec-archivo) (first automata)) (str fin (first sec-archivo))))) 
                                                        
; O(n)
; Funcion para comparar el estado y saber que "class" enviar a la funcion de span
(defn comparacion [estado palabra]
  (cond
    (= estado 100) (span "ctef" palabra)
    (= estado 101) (span "cte" palabra)
    (= estado 102) (es-palRes? palabra)
    (= estado 103) (span "com" palabra) 
    (= estado 104) (span "comilla" palabra)
    (= estado 105) (span "sim" palabra)
    (= estado 106) (span "parBrac" palabra)
    :else (span nil palabra)))

; O(n)
; Funcion de chequeo de transiciones, prueba si el estado enviado es igual al estado incial de la transicion enviada (0 dig 1), en caso de
; si serlo, hara una condicional para checar si el simbolo del medio existe en alguna de las transiciones del automata.
(defn transicion [estado letra transiciones]
  (cond
    (and (= (ffirst transiciones) estado) (cond
                                            (and (= (str (first (next (first transiciones)))) "letra") (existe? letra letras)) true
                                            (and (= (str (first (next (first transiciones)))) "dig") (existe? letra dig)) true
                                            (and (= (str (first (next (first transiciones)))) "sim") (existe? letra sim)) true
                                            (and (= (str (first (next (first transiciones)))) "delim") (existe? letra delim)) true
                                            (and (= (str (first (next (first transiciones)))) "nL") (existe? letra nL)) true
                                            (and (= (str (first (next (first transiciones)))) "esp") (existe? letra esp)) true
                                            (and (= (str (first (next (first transiciones)))) "parBrac") (existe? letra parBrac)) true
                                            (and (= (str (first (next (first transiciones)))) "com") (existe? letra com)) true
                                            (and (= (str (first (next (first transiciones)))) "comilla") (existe? letra comilla)) true
                                            (and (= (str (first (next (first transiciones)))) "punto") (existe? letra punto)) true
                                            :else false)) (first (next (next (first transiciones))))
    :else (transicion estado letra (rest transiciones))))

; O(n)
; Funcion para revisar si el ultimo caracter del archivo termina en un token
(defn acabo? [estado automata]
  (if (existe? estado (first (rest automata)))
    (do (htmlHaskell final) (println "Archivo Completado"))
    (println "Error en archivo")))

; O(n)
; Función para checar la existencia de un char en una lista
(defn existe? [letra lista]
  (cond
    (empty? lista) false
    (= letra (first lista)) true
    :else (existe? letra (rest lista))))

; O(n)
; Función para checar si el string creado es palabra reservada o no
(defn es-palRes? [concatenacion]
  (if (existe? concatenacion palRes)
    (span "pR" concatenacion)
    (span "ide" concatenacion)))

; O(1)
; Función para agregar cosas al archivo html 
(defn span [class concatenacion]
  (cond
    (= concatenacion "<") (htmlHaskell "<span class=\"sim\">&lt</span>") ; En caso de ser simbolo "menor que", enviar caracter especial
    (= concatenacion ">") (htmlHaskell "<span class=\"sim\">&gt</span>") ; En caso de ser simbolo "mayor que", enviar caracter especial
    (and (= concatenacion "\n") (= class nil)) (htmlHaskell "<br>\n")
    (and (= concatenacion " ") (= class nil)) (htmlHaskell "&nbsp;")
    (and (= concatenacion "\t") (= class nil)) (htmlHaskell "&nbsp;&nbsp;&nbsp;&nbsp;")
    :else (htmlHaskell (str "<span class=\"" class "\">" concatenacion "</span>"))))


; O(1)
; Función para agregar texto al archivo html
(defn htmlHaskell [parte]
  (with-open [wrtr (io/writer "/Users/marcelomarquezm/Desktop/Clojure/entregable3/src/entregable3/haskell.html" :append true)]
    (.write wrtr parte)
    (.close wrtr)))

; Configuración para archivo CSS de Haskell
(def css "
    .pR {
        color: #F05D5E;
        font-weight: bold;
    }

    .ide {
        color: #0F7173;
        font-style: italic;
    }

    .parBrac {
          color: blue;
    }
    
    .cte {
          color: #b028a5;
    }
          
    .comilla {
          color: #b09a87
    }
          
    .ctef {
        color: green;
    }

    .sim {
        color: maroon;
    }

    .com {
        color: orange;
    }")

; O(1)
; Archivo CSS para Haskell
(let [wrtr (io/writer "/Users/marcelomarquezm/Desktop/Clojure/entregable3/src/entregable3/haskell.css")]
  (.write wrtr css)
  (.close wrtr))

; O(1)
(let [x automata y lista-archivo]
  (time (tokens x y)))
;; ========================================================
;; Trabajo practico integrador Paradigmas y Lenguajes 2026
;; Integrantes: Dario Nahuel Gonzalez Machado
;; Tema: Sistema de semaforos inteligentes
;; ========================================================


;; REQUERIMIENTO 1

;; ========================================================
;; FUNCIÓN: Transicion
;; NATURALEZA: Pura 
;; ESTRATEGIA: Función Predicado
;; IMPACTO: No destructiva 
;; ========================================================

(defun Transicion (color-actual cambiar-a)
  (cond
    ((and (eq color-actual 'en-rojo) (eq cambiar-a 'verde))
     (list 'en-rojo "cambiar-a-verde"))
    ((and (eq color-actual 'en-verde) (eq cambiar-a 'amarillo))
     (list 'en-verde "cambiar-a-amarillo"))
    ((and (eq color-actual 'en-amarillo) (eq cambiar-a 'rojo))
     (list 'en-amarillo "cambiar-a-rojo"))
    (t (list color-actual 'accion-por-defecto))))


;; REQUERIMIENTO 2

;; ========================================================
;; FUNCIÓN: Mi-timer
;; NATURALEZA: Pura
;; ESTRATEGIA: Orden Superior y funcion predicado 
;; IMPACTO: No destructiva
;; ========================================================

(defun Mi-timer (tiempo)
  "La funcion position busca el primer valor que da true y nth obtiene el valor que esta en esa posicion"
  (nth (position t (mapcar (lambda (x)  (<= (mod tiempo 216) x)) 
        '(120 126 216)))
       '(en-verde en-amarillo en-rojo)))

;; ========================================================
;; FUNCIÓN: hora-local
;; NATURALEZA: Impura (tiene intencion de imprimir resultados en pantalla)
;; ESTRATEGIA: uso de local time
;; IMPACTO: No destructiva
;; ========================================================

(defun hora-local()
  (local-time:format-timestring nil (local-time:now) :format '(:year "-" (:month 2) "-" (:day 2) " " :hour ":" :min ":" :sec))
)  


;; REQUERIMIENTO 3

;; ========================================================
;; FUNCIÓN: Sistema-Auditoria
;; NATURALEZA: Impura (tiene intencion de imprimir resultados en pantalla)
;; ESTRATEGIA: Uso de funciones anteriores para verificar e imprimir estados
;; IMPACTO: No destructiva
;; ========================================================

(defun Sistema-auditoria (tiempo-anterior tiempo-nuevo)
  (if (equal (Mi-timer tiempo-anterior) (Mi-timer tiempo-nuevo))
      (format t "No hubo cambios, sigue ~a~%" (Mi-timer tiempo-nuevo))
      (format t "~a Tiempo ~a -> ~a: cambio de ~a a ~a~%"(hora-local)
              tiempo-anterior
              tiempo-nuevo 
              (Mi-timer tiempo-anterior) 
              (Mi-timer tiempo-nuevo)))
)

(print(sistema-auditoria 120 121))



;; REQUERIMIENTO 4a

; ========================================================
;; FUNCIÓN: duracion-ciclo
;; NATURALEZA: Pura 
;; ESTRATEGIA: Orden Superior (Implementada mediante Reduce)
;; IMPACTO: No destructiva
;; ========================================================

(defun duracion-ciclo (lista-semaforo)
  "Recibe una lista con la duracion de cada luz y calcula el tiempo total del ciclo"
  (reduce #'+ lista-semaforo))


;; REQUERIMIENTO 4b

;; ========================================================
;; FUNCIÓN: recomendacion-ciclo
;; NATURALEZA: Pura 
;; ESTRATEGIA: Funcion Predicado o condicional
;; IMPACTO: No destructiva
;; ========================================================

(defun recomendacion-ciclo (duracion-total)
  "Evalua si la duracion total es apta para el trafico o transeuntes"
  (cond
    ((< duracion-total 35) "No recomendado: Ciclo muy corto, para la seguridad de transito")
    ((> duracion-total 150) "No recomendado: Ciclo excesivo, no se recomienda hacer esperar mucho tiempo")
    (t "Recomendado: Rango optimo para el correcto flujo de transito")
  )
)

; ========================================================
;; FUNCIÓN: Analisis-ciclo
;; NATURALEZA: Pura
;; ESTRATEGIA: Composicion de funciones
;; IMPACTO: No destructiva
;; ========================================================

(defun Analisis-Ciclo (Lista-Semaforo)
    (recomendacion-ciclo (duracion-ciclo lista-semaforo))
)

;; REQUERIMIENTO 5

;; ========================================================
;; FUNCIÓN: ciclos-por-tiempo
;; NATURALEZA: Pura (Dado un número de minutos, siempre retorna el mismo calculo)
;; ESTRATEGIA: Funcion Aritmetica
;; IMPACTO: No destructiva
;; ========================================================

(defun ciclos-por-tiempo (lista-semaforo minutos)
    "La funcion calcula el tiempo en segundos, luego resta ese tiempo al modulo y lo divide para dar con el numero de ciclos"
    (/ (- (* minutos 60) (mod (* minutos 60) (duracion-ciclo lista-semaforo))) (duracion-ciclo lista-semaforo)))

;; ========================================================
;; FUNCIÓN: repartir-residuo
;; NATURALEZA: Pura
;; ESTRATEGIA: Funcion recursiva
;; IMPACTO: No destructiva
;; ========================================================

  (defun repartir-residuo (residuo lista-semaforo)
  (cond
    ((null lista-semaforo) nil)
    (t (cons (min (car lista-semaforo) residuo)
             (repartir-residuo
               (max 0 (- residuo (car lista-semaforo)))
               (cdr lista-semaforo))))))

;; ========================================================
;; FUNCIÓN: calcular-segundos-luces
;; NATURALEZA: Pura
;; ESTRATEGIA: Orden superior (mapcar) y aritmetica
;; IMPACTO: No destructiva
;; ========================================================

(defun calcular-segundos-luces (lista-semaforo minutos)
  (mapcar #'+ (mapcar (lambda (x)
                (* x (ciclos-por-tiempo lista-semaforo minutos)))
                  lista-semaforo)
          (repartir-residuo (mod (* minutos 60) (duracion-ciclo lista-semaforo))
            lista-semaforo)))


;; REQUERIMIENTO 6

;; ========================================================
;; FUNCIÓN: informe-distribucion
;; NATURALEZA: Pura
;; ESTRATEGIA: Orden superior y metodos aritmeticos
;; IMPACTO: No destructiva
;; ========================================================

(defun informe-distribucion (lista-semaforo)
  "Calcula los porcentajes de luces, usa formato 2f para que use solo dos decimales y 100.0 para transformarlo en flotante"
  (mapcar (lambda (x)
            (format nil "~,2F%"(/ (* x 100.0) (duracion-ciclo lista-semaforo))))
          lista-semaforo)
)

;; ========================================================
;; FUNCIÓN: ingresar-tiempo
;; NATURALEZA: Pura
;; ESTRATEGIA: Composición de funciones 
;; IMPACTO: No destructiva
;; ========================================================

(defun ingresar-tiempo (lista-semaforo minutos)
  (informe-distribucion (calcular-segundos-luces lista-semaforo minutos)))

;; ========================================================
;; FUNCIÓN: Mi-timer-intermitente
;; NATURALEZA: Pura
;; ESTRATEGIA: Función Predicado 
;; IMPACTO: No destructiva
;; ========================================================

(defun mi-timer-intermitente (tiempo)
  (cond
    ((< (mod tiempo 225) 120) 'en-verde)
    ((< (mod tiempo 225) 123) 'verde-intermitente)
    ((< (mod tiempo 225) 129) 'en-amarillo)
    ((< (mod tiempo 225) 132) 'amarillo-intermitente)
    ((< (mod tiempo 225) 222) 'en-rojo)
    (t 'rojo-intermitente)))

;; ========================================================
;; FUNCIÓN: Transicion-intermitente
;; NATURALEZA: Pura 
;; ESTRATEGIA: Función Predicado
;; IMPACTO: No destructiva 
;; ========================================================

(defun Transicion-intermitente (color-actual cambiar-a)
  (cond
    ((and (eq color-actual 'en-verde) (eq cambiar-a 'verde-int))
     (list 'en-verde "cambiar-a-verde-intermitente"))
    ((and (eq color-actual 'en-verde-int) (eq cambiar-a 'amarillo))
     (list 'en-verde-int "cambiar-a-amarillo"))
    ((and (eq color-actual 'en-amarillo) (eq cambiar-a 'amarillo-int))
     (list 'en-amarillo "cambiar-a-amarillo-intermitente"))
    ((and (eq color-actual 'en-amarillo-int) (eq cambiar-a 'rojo))
     (list 'en-amarillo-int "cambiar-a-rojo"))
    ((and (eq color-actual 'en-rojo) (eq cambiar-a 'rojo-int))
     (list 'en-rojo "cambiar-a-rojo-intermitente"))
    ((and (eq color-actual 'en-rojo-int) (eq cambiar-a 'verde))
     (list 'en-rojo-int "cambiar-a-verde"))
    (t (list color-actual 'accion-por-defecto))
  )
)

;; ========================================================
;; FUNCIÓN: informe
;; NATURALEZA: Impura 
;; ESTRATEGIA: Composición de funciones
;; IMPACTO: No destructiva
;; ========================================================

(defun informe (datos)
  (with-open-file (stream "informe-ejecucion-semaforo.txt"
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (format stream "Informe de Ejecución del Sistema Semafórico~%")
    (format stream "=========================================~%")
    (mapcar #'(lambda (tiempo)
                (let* ((fecha-hoy (local-time:format-timestring nil 
                                       (local-time:unix-to-timestamp tiempo)
                                       :format '(:year "-" :month "-" :day " " :hour ":" :min ":" :sec)))
                       (color-actual (mi-timer tiempo))
                       (color-anterior (cond
                                         ((equal (mi-timer tiempo) 'en-rojo) 'en-verde)
                                         ((equal (mi-timer tiempo) 'en-amarillo) 'en-rojo)
                                         ((equal (mi-timer tiempo) 'en-verde) 'en-amarillo))))
                  
                  (format stream "~a - Transición: ~a -> ~a~%" 
                          fecha-hoy
                          color-anterior 
                          color-actual)))
            datos)
    
    (format stream "~%--- Fin del Informe ---~%")))
;;*******************************
;;* DEFINICION DE LAS FUNCIONES *
;;*******************************


(deffunction ask-question (?question $?allowed-values)
    (printout t ?question)
    (bind ?answer (read))
    (if (lexemep ?answer)
        then (bind ?answer (lowcase ?answer)))
    (while (not (member$ ?answer ?allowed-values)) do
        (printout t ?question)
        (bind ?answer (read))
        (if (lexemep ?answer)
            then (bind ?answer (lowcase ?answer))))
    ?answer)

(deffunction question (?question)
    (format t "%s" ?question)
    (bind ?answer (read))
    ?answer
)

(deffunction yes-or-no-p (?question)
    (bind ?response (ask-question ?question yes no y n))
    (if (or (eq ?response yes) (eq ?response y)) then 1 else 0)
)

(deffunction lista-ingredientes (?platos)
  (bind ?lista-ingredientes (create$)) ; Crear una lista vacía para los ingredientes
  
  (foreach ?plato ?platos
    (foreach ?ingrediente (send ?plato get-formado_por)
        (bind ?lista-ingredientes (create$ ?lista-ingredientes ?ingrediente))
    )
  )
  (return ?lista-ingredientes)
)

(deffunction lista-ingredientes-principales (?platos)
    (bind ?i1 (first$ (send (nth$ 3 ?platos) get-formado_por)))
    (bind ?i2 (first$ (send (nth$ 4 ?platos) get-formado_por)))
    (bind ?i3 (first$ (send (nth$ 6 ?platos) get-formado_por)))
    (bind ?i4 (first$ (send (nth$ 7 ?platos) get-formado_por)))
    (bind ?lista-ingredientes-principales (create$ ?i1 ?i2 ?i3 ?i4))
  (return ?lista-ingredientes-principales)
)

(deffunction repetidos-ingredientes (?platos)
  (bind ?lista-ingredientes (create$)) ; Crear una lista vacía para los ingredientes
  
  (foreach ?plato ?platos
    (foreach ?ingrediente (send ?plato get-formado_por)
      (if (member$ ?ingrediente ?lista-ingredientes)
        then
        (return TRUE)
      )
      (bind ?lista-ingredientes (create$ ?lista-ingredientes ?ingrediente))
    )
  )
  (return FALSE)
)

(deffunction ingredientes-principales (?platos)
  (bind ?lista-ingredientes (create$)) ; Crear una lista vacía para los ingredientes
  (bind ?indices (create$ 3 4 6 7))
  (foreach ?idx ?indices
    (bind ?ing (first$ (send (nth$ ?idx ?platos) get-formado_por)))
    (if (member$ ?ing ?lista-ingredientes)
        then
        (return TRUE)
    )
    (bind ?lista-ingredientes (create$ ?lista-ingredientes ?ing))
  )
  (return FALSE)
)

(deffunction generar-combinacion (?des-plato ?pri-plato ?seg-plato ?pos-plato ?cal)

    ;Tamaño de la lista de platos
    (bind ?tam-des (length$ ?des-plato))
    (bind ?tam-pri (length$ ?pri-plato))
    (bind ?tam-seg (length$ ?seg-plato))
    (bind ?tam-pos (length$ ?pos-plato))


    ;Indices para escoger platos
    ;Desayuno
    (bind ?d1 (random 1 ?tam-des))
    (bind ?d2 (random 1 ?tam-des))
    (while (eq ?d1 ?d2) do (bind ?d2 (random 1 ?tam-des)))

    ;Comida
    (bind ?c1 (random 1 ?tam-pri))
    (bind ?c2 (random 1 ?tam-seg))
    (while (eq ?c1 ?c2) do (bind ?c2 (random 1 ?tam-seg)))
    (bind ?p1 (random 1 ?tam-pos))


    ;Cena
    (bind ?ce1 (random 1 ?tam-pri))
    (while (or (eq ?ce1 ?c2) (eq ?ce1 ?c1)) do (bind ?ce1 (random 1 ?tam-pri)))
    (bind ?ce2 (random 1 ?tam-seg))
    (while (or (eq ?ce2 ?c2) (eq ?ce2 ?ce1) (eq ?ce2 ?c1)) do (bind ?ce2 (random 1 ?tam-seg)))
    (bind ?p2 (random 1 ?tam-pos))
    (while (eq ?p2 ?p1) do (bind ?p2 (random 1 ?tam-pos)))


    ;(printout t crlf ?d1 ?d2 ?c1 ?c2 ?p1 ?ce1 ?ce2 ?p3 crlf)
    (bind ?menu (create$ (nth$ ?d1 ?des-plato) (nth$ ?d2 ?des-plato) (nth$ ?c1 ?pri-plato) (nth$ ?c2 ?seg-plato) (nth$ ?p1 ?pos-plato)
                         (nth$ ?ce1 ?pri-plato) (nth$ ?ce2 ?seg-plato) (nth$ ?p2 ?pos-plato)))
    ;(printout t crlf "Menu probado " ?menu)
    (return ?menu)
)

(deffunction contar-nutrientes ($?platos)
    (bind ?cal 0)
    (bind ?hid 0)
    (bind ?gra 0)
    (bind ?pro 0)
    (bind ?fib 0)
    (foreach ?plato ?platos
        (bind ?cal (+ ?cal (send ?plato get-calorias)))
        (bind ?hid (+ ?hid (send ?plato get-cantidad_hidratos)))
        (bind ?gra (+ ?gra (send ?plato get-cantidad_grasas)))
        (bind ?pro (+ ?pro (send ?plato get-cantidad_proteinas)))
        (bind ?fib (+ ?fib (send ?plato get-cantidad_fibras)))
    )
    (return (create$ ?cal ?hid ?gra ?pro ?fib))
)

; Comprueba que se el menu cumpla con las restricciones
; **************** Si quereis añadir mas simplemente añadir parentesis al if y ya
; ***************** Falta añadir que se quiten los platos usados pero no lo hago de momento porque hay pocas instancias

(deffunction menu-correcto (?posible-menu ?cal ?peso)
    ;(bind ?calorias-des (round (* 0.25 ?cal)))
    ;(bind ?calorias-com (round (* 0.4 ?cal)))
    ;(bind ?calorias-cen (round (* 0.35 ?cal)))
    ;(bind ?hidr (round (* ?calo 0.52)))
    ;(bind ?gras (round (* ?calo 0.27)))
    ;(bind ?prot (round (* 0.83 ?peso)))
    ;(bind ?fibra 25)
    (bind ?nutrientes-menu (contar-nutrientes ?posible-menu))

    (if (and
        (not (repetidos-ingredientes (subseq$ ?posible-menu 3 4)))
        (not (repetidos-ingredientes (subseq$ ?posible-menu 6 7)))
        (not (ingredientes-principales ?posible-menu))
        (>= (+ ?cal 150) (nth$ 1 ?nutrientes-menu))
        (<= (- ?cal 150) (nth$ 1 ?nutrientes-menu))
        ; Hidratos
        (>= (* ?cal 0.55) (* 4 (nth$ 2 ?nutrientes-menu)))
        (<= (* ?cal 0.45) (* 4 (nth$ 2 ?nutrientes-menu)))
        ; Grasas
        (>= (* ?cal 0.35) (* 9 (nth$ 3 ?nutrientes-menu)))
        (<= (* ?cal 0.3) (* 9 (nth$ 3 ?nutrientes-menu)))
        ; Proteinas
        (>= (* ?cal 0.2) (* 4 (nth$ 4 ?nutrientes-menu)))
        (<= (* ?cal 0.15) (* 4 (nth$ 4 ?nutrientes-menu)))
        ; Fibras
        (>= 30 (nth$ 5 ?nutrientes-menu))
        (<= 20 (nth$ 5 ?nutrientes-menu))
        ) then (return TRUE) else (return FALSE)
    )
)

(deffunction calcular-calorias-fun(?edad ?sexo ?actividad)
    (if(eq ?sexo m) then
        (if (and (eq ?actividad 0) (>= ?edad 65) (<= ?edad 69)) then
            (bind ?kcal 2006)
        )
        (if (and (eq ?actividad 1) (>= ?edad 65) (<= ?edad 69)) then
            (bind ?kcal 2293)
        )
        (if (and (eq ?actividad 2) (>= ?edad 65) (<= ?edad 69)) then
            (bind ?kcal 2603)
        )
        (if (and (eq ?actividad 0) (>= ?edad 70) (<= ?edad 79)) then
            (bind ?kcal 1982)
        )
        (if (and (eq ?actividad 1) (>= ?edad 70) (<= ?edad 79)) then
            (bind ?kcal 2268)
        )
        (if (and (eq ?actividad 2) (>= ?edad 70) (<= ?edad 79)) then
            (bind ?kcal 2555)
        )
        (if (> ?edad 79) then
            ; Calculado a partir de la media de MJ de actividades físicas 0 y 1, y se le resta 0.1 MJ que desciende por cada 10 años
            (bind ?kcal 2102)
        )
    )
    (if(eq ?sexo f) then
        (if (and (eq ?actividad 0) (>= ?edad 65) (<= ?edad 69)) then
            (bind ?kcal 1634)
        )
        (if (and (eq ?actividad 1) (>= ?edad 65) (<= ?edad 69)) then
            (bind ?kcal 1863)
        )
        (if (and (eq ?actividad 2) (>= ?edad 65) (<= ?edad 69)) then
            (bind ?kcal 2102)
        )
        (if (and (eq ?actividad 0) (>= ?edad 70) (<= ?edad 79)) then
            (bind ?kcal 1634)
        )
        (if (and (eq ?actividad 1) (>= ?edad 70) (<= ?edad 79)) then
            (bind ?kcal 1839)
        )
        (if (and (eq ?actividad 2) (>= ?edad 70) (<= ?edad 79)) then
            (bind ?kcal 2078)
        )
        (if (> ?edad 79) then
            ; Calculado a partir de la media de MJ de actividades físicas 0 y 1, y se le resta 0.1 MJ que desciende por cada 10 años
            (bind ?kcal 1708)
        )
    )
    ?kcal
)

(deffunction borrar-platos (?ing)
    (do-for-all-instances ((?plato plato)) (member$ ?plato (send ?ing get-esta_en))
        (printout t crlf "Plato (" ?plato:nombre ") eliminado porque lleva " ?ing )
        (send ?plato delete)
    )
    (printout t crlf)
)


(deffunction borrar-platos-temporada (?temporada_actual)
    (bind ?temporada_actual (str-cat ?temporada_actual ""))
    ; Por cada ingrediente que no esté en temporada
    (do-for-all-instances ((?ingrediente ingrediente)) (and (not (member$ "siempre" ?ingrediente:disponible_en)) (not (member$ ?temporada_actual ?ingrediente:disponible_en)))
        ; Por cada plato que lleve ese ingrediente
        (do-for-all-instances ((?plato plato)) (member$ ?plato ?ingrediente:esta_en)
            ; Por cada ingrediente que lleva ese plato
            (do-for-all-instances ((?ingrediente2 ingrediente)) (member$ ?plato ?ingrediente2:esta_en)
                (bind ?lista-platos ?ingrediente2:esta_en)
                ;(printout t crlf "Borro " ?plato " de " ?ingrediente2)
                (bind $?nueva-lista (delete-member$ ?lista-platos ?plato))
                ;(printout t crlf ?nueva-lista)
                (modify-instance ?ingrediente2 (esta_en ?nueva-lista))
            )
            (printout t crlf "Plato (" ?plato:nombre ") eliminado porque lleva " ?ingrediente )
            (send ?plato delete)
        )
    )
    (printout t crlf)
)

;;*******************************
;;* INICIO DEL CREADOR DE MENUS *
;;*******************************

(deftemplate Persona
    (slot estado)
    (slot edad (type INTEGER) (range 65 120))
    (slot sexo (type STRING) (allowed-values "m" "f"))
    (slot actividad-fisica (type INTEGER) (range 0 2))
    (slot peso (type INTEGER))
    ; Kilocalorias diarias recomendadas
    (slot calorias (type INTEGER))
)

(deftemplate Cantidades-rest
    ; Kilocalorias por consumir en el desayuno
    (slot calorias-des (type INTEGER))
    ; Kilocalorias por consumir en la comida
    (slot calorias-com (type INTEGER))
    ; Kilocalorias por consumir en la cena
    (slot calorias-cen (type INTEGER))
    ; Kilocalorias de hidratos por consumir para un día concreto
    (slot hidratos-rest (type INTEGER))
    ; Kilocalorias de grasas por consumir para un día concreto
    (slot grasas-rest (type INTEGER))
    ; Gramos de proteinas por consumir para un día concreto
    (slot proteinas-rest (type INTEGER))
    ; Gramos de fibras por consumir para un día concreto
    (slot fibras-rest (type INTEGER))
)

(deftemplate Menu-diario
    (slot dia (type INTEGER) (range 1 8))
    (multislot menu)
)

(deftemplate usados
    (multislot plato)
)

(deftemplate ing-usados
    (multislot ingrediente)
)



(deftemplate Enfermedades-persona
    (slot colesterol (type INTEGER) (allowed-integers 0 1))
    (slot diabetes (type INTEGER) (allowed-integers 0 1))
    (slot anemia (type INTEGER) (allowed-integers 0 1))
    (slot hipertension (type INTEGER) (allowed-integers 0 1))
    (slot osteoporosis (type INTEGER) (allowed-integers 0 1))
    (slot obesidad (type INTEGER) (allowed-integers 0 1))
)

(deftemplate Preferencias-persona
    (slot pescado (type INTEGER) (allowed-integers 0 1))
    (slot carne (type INTEGER) (allowed-integers 0 1))
    (slot frutos-secos (type INTEGER) (allowed-integers 0 1))
    (slot lacteos (type INTEGER) (allowed-integers 0 1))
    (slot frutas (type INTEGER) (allowed-integers 0 1))
    (slot cereales (type INTEGER) (allowed-integers 0 1))
    (slot verduras (type INTEGER) (allowed-integers 0 1))
)

(defrule initial
    (declare (salience 1001))
    (initial-fact)
    =>
    (printout t crlf "*** BIENVENIDOS AL CREADOR DE MENUS *** " crlf crlf)
)

;;;***********************
;;;* MÓDULO DE PREGUNTAS *
;;;***********************


(defrule crear-perfil ""
    (declare (salience 1000))
    =>
    (printout t "Introduzca sus datos personales." crlf)
    (bind ?edad (question "Que edad tiene? (Valor entre 65 y 120) "))
    (bind ?edad (integer ?edad))
    (if (or (> ?edad 120) (< ?edad 65) ) then (printout t crlf "La edad introducida no esta entre 65 y 120" crlf)(exit))
    (bind ?sexo (ask-question "Indique su sexo Masculino (m) o Femenino (f): " m f))
    (bind ?act (ask-question "Cuanta actividad física realiza (0=Poca, 1=Normal, 2=Mucha)? " 0 1 2))
    (bind ?calo (calcular-calorias-fun ?edad ?sexo ?act))
    (bind ?hidr (round (* ?calo 0.52)))
    (bind ?gras (round (* ?calo 0.27)))
    (bind ?peso (question "¿Cuál es su peso actual? (no use decimales) "))
    (bind ?prot (round (* 0.83 ?peso))) ;;; o pasar a kcal u operar todo en gramos
    (assert (Persona (edad ?edad) (sexo ?sexo) (actividad-fisica ?act) (peso ?peso) (calorias ?calo)))
    (assert (Cantidades-rest (calorias-des (round (* 0.3 ?calo))) (calorias-com (round (* 0.4 ?calo))) (calorias-cen (round (* 0.3 ?calo)))
                             (hidratos-rest ?hidr) (grasas-rest ?gras) (proteinas-rest ?prot) (fibras-rest 25)))
    (printout t crlf "Información personal guardada correctamente." crlf)
	(assert (usados))
    (assert (ing-usados))
	(assert (empieza 1))
 )

(defrule preguntar-enfermedades ""
	(declare (salience 999))
	=>
	(printout t crlf "Responde las siguientes preguntas para que el menú tenga en cuenta sus enfermedades con (yes/no) o (y/n)." crlf)
	(bind ?col (yes-or-no-p "¿Tiene el colesterol alto? "))
    (bind ?dia (yes-or-no-p "¿Tiene diabetes? "))
    (bind ?ane (yes-or-no-p "¿Tiene anemia? (deficiencia de hierro) "))
    (bind ?hip (yes-or-no-p "¿Tiene hipertensión arterial? "))
    (bind ?ost (yes-or-no-p "¿Tiene osteoporosis? "))
    (bind ?obe (yes-or-no-p "¿Tiene obesidad? "))
	(assert (Enfermedades-persona (colesterol ?col) (diabetes ?dia) (anemia ?ane) (hipertension ?hip) (osteoporosis ?ost)
			(obesidad ?obe)))
	(printout t crlf "Información de las enfermedades guardada correctamente." crlf)
)

(defrule preguntar-preferencias ""
	(declare (salience 998))
	=>
	(printout t crlf "Responde las siguientes preguntas para que el menú tenga en cuenta sus alergias y/o preferencias." crlf)
	(printout t crlf "Cada pregunta estara relacionada con un grupo de alimentos general, si en ese grupo existe algun alimento al cual tenga alergia o prefiera no comerlo indique con (yes/y), por el contrario indique (no/n)." crlf)
	(printout t "Si un grupo de alimentos queda muy restringido por sus alergias, las preferencias pasaran a un segundo plano." crlf)
	(bind ?pes (yes-or-no-p "Alergia/Prefiere evitar comer algun pescado? "))
    (bind ?roj (yes-or-no-p "Alergia/Prefiere evitar comer alguna carne? "))
    (bind ?sec (yes-or-no-p "Alergia/Prefiere evitar comer algun frutos seco? "))
    (bind ?lac (yes-or-no-p "Alergia/Prefiere evitar comer algun lacteo? "))
    (bind ?fru (yes-or-no-p "Alergia/Prefiere evitar comer alguna fruta? "))
    (bind ?cer (yes-or-no-p "Alergia/Prefiere evitar comer algun cereal? "))
    (bind ?ver (yes-or-no-p "Alergia/Prefiere evitar comer alguna verdura o legumbre? "))
	(assert (Preferencias-persona (pescado ?pes) (carne ?roj) (frutos-secos ?sec) (lacteos ?lac)
								  (frutas ?fru) (cereales ?cer) (verduras ?ver)))
    (assert (ingredientes-alergico))
	(printout t crlf "Información de las preferencias guardada correctamente." crlf)
)

(defrule preguntar-temporada ""
	(declare (salience 917))
	=>
	(printout t crlf "Información adicional para configurar el menú." crlf)
	(bind ?temp (ask-question "¿En qué estación del año estamos? (primavera, verano, otoño o invierno) -- Escriba aquí la estación: " primavera verano otoño invierno))
	(borrar-platos-temporada ?temp)
)

;;;********************************
;;;* MÓDULO DE FILTRADO DE PLATOS *
;;;********************************

;;; ***** POR ENFERMEDADES *****

;;; 25-35% grasas, 50-60% carbohidratos, 20-30 gramos fibra, > 15% proteinas
;;; Alimentos de origen vegetal, alto contenido en fibra, integrales y pescado. Evitar grasas saturadas y 5g max de sal/d.
(defrule tiene-colesterol
    (declare (salience 999))
    ?enf <- (Enfermedades-persona (colesterol 1))
    =>
    (bind ?lista (find-all-instances ((?plato plato)) (> (send ?plato get-cantidad_grasas) 20)))
    (foreach ?plato ?lista
    		(bind ?p (nth$ 1 (find-instance ((?pla plato)) (eq ?plato ?pla))))

    		(do-for-all-instances ((?ingrediente ingrediente)) (member$ ?p ?ingrediente:esta_en)
    		    (bind $?lista (send ?ingrediente get-esta_en))
    		    ;(printout t crlf "Borro " ?p " de " ?ingrediente)
    		    (bind $?nueva-lista (delete-member$ ?lista ?p))
                ;(printout t crlf ?nueva-lista)
    		    (send ?ingrediente put-esta_en ?nueva-lista)
            )

    		(printout t "Plato (" (send ?p get-nombre) ") eliminado porque tiene acceso de grasas." crlf)
    		(send ?p delete)
    )
	(modify ?enf (colesterol 0))
)

;;; > 50-60% hidratos de carbono, 30-40 gramos fibra, 12-20% proteinas, > 30% grasas,
;;; Hidratos de carbono sanos, mucha fibra, pescado, quitar azucares, 2.5-3 g  sal/d
(defrule tiene-diabetes
    (declare (salience 999))
	?enf <- (Enfermedades-persona (diabetes 1))
        =>
    	(borrar-platos [azucar])
        (modify ?enf (diabetes 0))

)

;;; Vitamina C mejora absorción hierro, calcio disminuye absorcion, aumentar consumo carne
;;; Proporciones normales de nutrientes
(defrule tiene-anemia
	?enf <- (Enfermedades-persona (anemia 1))
    =>
	(modify ?enf (anemia 0))
)

;;; Alimentos origen vegetal, menos 6g sal/d, poca grasa, mas de 2 veces pescado
;;; Incluir potasio, magnesio y calcio
(defrule tiene-hipertension
	?enf <- (Enfermedades-persona (hipertension 1))
    =>
	(modify ?enf (hipertension 0))
)

;;; Mejorar ingesta calcio, vitaminaD, fosforo, vitaminaK y potasio. Evitar mucha proteina, sodio y fosforo
;;; Proporciones normales de nutrientes
(defrule tiene-osteoporosis
	?enf <- (Enfermedades-persona (osteoporosis 1))
    =>
    (modify ?enf (osteoporosis 0))
)

;;;Aumentar ingesta de proteina, fibra y alimentos de origen vegetal
;;;
(defrule tiene-obesidad
	(declare (salience 999))
	?enf <- (Enfermedades-persona (obesidad 1))
    =>
    (bind ?lista (find-all-instances ((?plato plato)) (eq (send ?plato get-cocinado) [frito])))
    (foreach ?plato ?lista
    		(bind ?p (nth$ 1 (find-instance ((?pla plato)) (eq ?plato ?pla))))

    		(do-for-all-instances ((?ingrediente ingrediente)) (member$ ?p ?ingrediente:esta_en)
    		    (bind $?lista (send ?ingrediente get-esta_en))
    		    ;(printout t crlf "Borro " ?p " de " ?ingrediente)
    		    (bind $?nueva-lista (delete-member$ ?lista ?p))
                ;(printout t crlf ?nueva-lista)
    		    (send ?ingrediente put-esta_en ?nueva-lista)
            )

    		(printout t crlf "Plato (" (send ?p get-nombre) ") eliminado porque es un frito.")
    		(send ?p delete)
    )
	(modify ?enf (obesidad 0))
)

;;;*****************************

;;; ***** POR PREFERENCIAS *****

(defrule preferencia-pescado
	(declare (salience 957))
	?pre <- (Preferencias-persona (pescado 1))
    =>
    (printout t crlf "Indique con (1) si es alergico al alimento, con (2) si prefiere no comerlo o con (3) si no tiene problemas." crlf)
    (bind ?i (ask-question "Salmon? " 1 2 3))
    (if (eq ?i 1) then (borrar-platos [salmon]))
    (bind ?i (ask-question "Merluza? " 1 2 3))
    (if (eq ?i 1) then (borrar-platos [merluza]))
    (bind ?i (ask-question "Rape? " 1 2 3))
    (if (eq ?i 1) then (borrar-platos [rape]))
    (bind ?i (ask-question "Bacalao? " 1 2 3))
    (if (eq ?i 1) then (borrar-platos [bacalao]))
    (bind ?i (ask-question "Gamba? " 1 2 3))
    (if (eq ?i 1) then (borrar-platos [gamba]))
    (bind ?i (ask-question "Atun? " 1 2 3))
    (if (eq ?i 1) then (borrar-platos [atun]))
	(modify ?pre (pescado 0))
)

(defrule preferencia-carne
	(declare (salience 957))
	?pre <- (Preferencias-persona (carne 1))
    =>
    (printout t crlf "Indique con (1) si es alergico al alimento, con (2) si prefiere no comerlo o con (3) si no tiene problemas." crlf)
    (bind ?i (ask-question "Pollo? " 1 2 3))
    (if (eq ?i 1) then (borrar-platos [pollo]))
    (bind ?i (ask-question "Ternera? " 1 2 3))
    (if (eq ?i 1) then (borrar-platos [ternera]))
    (bind ?i (ask-question "Cerdo? " 1 2 3))
    (if (eq ?i 1) then (borrar-platos [cerdo]))
	(modify ?pre (carne 0))
)


(defrule preferencia-frutos-secos
	(declare (salience 957))
	?pre <- (Preferencias-persona (frutos-secos 1))
    =>
    (printout t crlf "Indique con (1) si es alergico al alimento, con (2) si prefiere no comerlo o con (3) si no tiene problemas." crlf)
    ;(bind ?i (ask-question "Pipas? " 1 2 3))
    ;(if (eq ?i 1) then (borrar-platos [pipas]))
    ;(bind ?i (ask-question "Cachuetes? " 1 2 3))
    ;(if (eq ?i 1) then (borrar-platos [cacahuete]))
    ;(bind ?i (ask-question "Almendras? " 1 2 3))
    ;(if (eq ?i 1) then (borrar-platos [almendras]))
    ;(bind ?i (ask-question "Anacardos? " 1 2 3))
    ;(if (eq ?i 1) then (borrar-platos [anacardos]))
	(modify ?pre (frutos-secos 0))
)

(defrule preferencia-lacteos
	(declare (salience 957))
	?pre <- (Preferencias-persona (lacteos 1))
    =>
    (printout t crlf "Indique con (1) si es alergico al alimento, con (2) si prefiere no comerlo o con (3) si no tiene problemas." crlf)
    (bind ?i (ask-question "Leche? " 1 2 3))
    (if (eq ?i 1) then (borrar-platos [leche]))
    (bind ?i (ask-question "Queso? " 1 2 3))
    (if (eq ?i 1) then (borrar-platos [queso]))
	(modify ?pre (lacteos 0))
)

(defrule preferencia-frutas
	(declare (salience 957))
	?pre <- (Preferencias-persona (frutas 1))
    =>
    (printout t crlf "Indique con (1) si es alergico al alimento, con (2) si prefiere no comerlo o con (3) si no tiene problemas." crlf)
    ;(bind ?i (ask-question "Platano? " 1 2 3))
    ;(if (eq ?i 1) then (borrar-platos [platano]))
    (bind ?i (ask-question "Naranja? " 1 2 3))
    (if (eq ?i 1) then (borrar-platos [naranja]))
    ;(bind ?i (ask-question "Fresas? " 1 2 3))
    ;(if (eq ?i 1) then (borrar-platos [fresas]))
    ;(bind ?i (ask-question "Melon? " 1 2 3))
    ;(if (eq ?i 1) then (borrar-platos [melon]))
    (bind ?i (ask-question "Tomate? " 1 2 3))
    (if (eq ?i 1) then (borrar-platos [tomate]))
	(modify ?pre (frutas 0))
)

(defrule preferencia-cereales
	(declare (salience 957))
	?pre <- (Preferencias-persona (cereales 1))
    =>
    (printout t crlf "Indique con (1) si es alergico al alimento, con (2) si prefiere no comerlo o con (3) si no tiene problemas." crlf)
    (bind ?i (ask-question "Pasta? " 1 2 3))
    (if (eq ?i 1) then (borrar-platos [pasta]))
    (bind ?i (ask-question "Arroz? " 1 2 3))
    (if (eq ?i 1) then (borrar-platos [arroz]))
    (bind ?i (ask-question "Pan? " 1 2 3))
    (if (eq ?i 1) then (borrar-platos [pan]))
    (bind ?i (ask-question "Harina? " 1 2 3))
    (if (eq ?i 1) then (borrar-platos [harina]))
	(modify ?pre (cereales 0))
)

(defrule preferencia-verduras-legumbres
	(declare (salience 957))
	?pre <- (Preferencias-persona (verduras 1))
    =>
    (printout t crlf "Indique con (1) si es alergico al alimento, con (2) si prefiere no comerlo o con (3) si no tiene problemas." crlf)
    ;(bind ?i (ask-question "Espinacas? " 1 2 3))
    ;(if (eq ?i 1) then (borrar-platos [espinaca]))
    ;(bind ?i (ask-question "Zanahoria? " 1 2 3))
    ;(if (eq ?i 1) then (borrar-platos [zanahoria]))
    (bind ?i (ask-question "Patata? " 1 2 3))
    (if (eq ?i 1) then (borrar-platos [patata]))
    (bind ?i (ask-question "Lechuga? " 1 2 3))
    (if (eq ?i 1) then (borrar-platos [lechuga]))
    (bind ?i (ask-question "Lentejas? " 1 2 3))
    (if (eq ?i 1) then (borrar-platos [lentejas]))
    (bind ?i (ask-question "Garbanzos? " 1 2 3))
    (if (eq ?i 1) then (borrar-platos [garbanzos]))
	(modify ?pre (verduras 0))
)


;;;****************************


;;;***********************************
;;;* MÓDULO DE ACTUALIZACIÓN DE MENÚ *
;;;***********************************

(deftemplate platos-apropiados
    (slot estado)
    (multislot desayuno)
    (multislot primeros)
    (multislot segundos)
    (multislot postre)
)


;;;(defrule resetear-cantidades-rest "Reseteamos las cantidades de los elementos diarios una vez se completa el menu de un día"
    ;;;(eq ?dia-acabado true)
    ;;;(not (Menu-diario (dia 7)))
    ;;;?c <- (Cantidades-rest)
    ;;;?p <- (Persona)
    ;;;=>
    ;;;(bind ?dia-acabado false)
    ;;;(modify ?c (calorias-rest (fact-slot-value ?p calorias)))
    ;;;(bind ?hidr-act (round (* (fact-slot-value ?p calorias) 0.52)))
    ;;;(modify ?c (hidratos-rest ?hidr-act))
    ;;;(bind ?gras-act (round (* (fact-slot-value ?p calorias) 0.27)))
    ;;;(modify ?c (grasas-rest ?gras-act))
    ;;;(bind ?prot-act (round (* 0.83 (fact-slot-value ?p peso))))
    ;;;(modify ?c (proteinas-rest ?prot-act))
    ;;;(modify ?c (fibras-rest 25))
;;;)

;;; Para probar

;(defrule listar-ingredientes
 ; (declare (salience 96))
 ; =>
 ; (bind ?ingredientes (find-all-instances ((?ingrediente plato)) TRUE))
 ; (foreach ?ingrediente ?ingredientes do
 ;     (printout t "Nombre: " (send ?ingrediente get-nombre) crlf)
      ;(if (eq (send ?ingrediente get-nombre) "Pollo") then (send ?ingrediente delete))
 ; )
;)

(deffunction filtrar-platos (?platos ?ingredientes-excluidos)
  (bind ?platos-filtrados (create$))
  (foreach ?plato ?platos
    (bind ?excluir FALSE)
    (foreach ?ingrediente ?ingredientes-excluidos
      (if (member$ ?ingrediente (send ?plato get-formado_por))
        then
        (bind ?excluir TRUE)
        (break)
      )
    )
    (if (not ?excluir)
      then
      (bind ?platos-filtrados (create$ ?plato ?platos-filtrados))
    )
  )
  (return ?platos-filtrados)
)


(defrule seleccionar-platos
    (declare (salience 90))
    ?usado <-(usados (plato $?p))
    ?ing-usado <-(ing-usados (ingrediente $?i))
    ?emp <-(empieza ?d)
    (test (< ?d 8))
    ?pe <- (Persona (peso ?peso) (calorias ?cal))
    =>
    (bind ?des-platos (find-all-instances ((?plato plato)) (and (not (member$ ?plato ?p)) (member$ "desayuno" ?plato:tipoComida))))
    (bind ?pri-platos (find-all-instances ((?plato plato)) (and (not (member$ ?plato ?p)) (member$ "primero" ?plato:tipoComida))))
    (bind ?pri-platos-fil (filtrar-platos ?pri-platos ?i))
    (bind ?seg-platos (find-all-instances ((?plato plato)) (and (not (member$ ?plato ?p)) (member$ "segundo" ?plato:tipoComida))))
    (bind ?seg-platos-fil (filtrar-platos ?seg-platos ?i))
    (bind ?pos-platos (find-all-instances ((?plato plato)) (and (not (member$ ?plato ?p)) (member$ "postre" ?plato:tipoComida))))

    (bind ?posible-menu (generar-combinacion ?des-platos ?pri-platos-fil ?seg-platos-fil ?pos-platos ?cal))
    (bind ?a 1)
    (while (and (not (menu-correcto ?posible-menu ?cal ?peso)) (< ?a 30000)) do
        (bind ?posible-menu (generar-combinacion ?des-platos ?pri-platos-fil ?seg-platos-fil ?pos-platos ?cal))
        (bind ?a (+ ?a 1))
    )
    (while (not (menu-correcto ?posible-menu ?cal ?peso)) do
        (bind ?posible-menu (generar-combinacion ?des-platos ?pri-platos ?seg-platos ?pos-platos ?cal))
    )

    (modify ?usado (plato ?posible-menu))
    (modify ?ing-usado (ingrediente (lista-ingredientes-principales ?posible-menu)))

    (assert (Menu-diario (dia ?d) (menu ?posible-menu)))
    (assert (printea-dia))
	(assert (Actualiza ?d))
	(retract ?emp)
)



(defrule actualizar-condiciones
    (declare (salience 89))
    ?act <- (Actualiza ?d)
    (test (< ?d 8))
    ?c <- (Cantidades-rest)
    ?p <- (Persona (calorias ?calo))
    =>
    ;;; Reseteamos cantidades diarias
    ;(modify ?c (calorias-des (round (* 0.3 ?calo))) (calorias-com (round (* 0.4 ?calo))) (calorias-cen (round (* 0.3 ?calo))))


    ;;;Reseteamos platos
    ;(printout t crlf "MENU DEL DIA " ?d " platos: " ?pos-menu crlf)
    ;(printout t  "Platos asignados al desayuno: "  ?desayuno crlf)
    ;(printout t  "Platos asignados a la comida: " ?comida crlf)
    ;(printout t  "Platos asignados a la cena: " ?cena crlf crlf)
    ;(assert (printea-dia))

    (if(> 7 ?d) then (assert (empieza (+ ?d 1))) else (assert (printea)))
    (retract ?act)
)

(defrule print-menu-dia
    (declare (salience 98))
    (printea-dia)
    ?dia <- (Menu-diario (dia ?d) (menu $?lista))
    ?pe <- (Persona (calorias ?cal))
    =>
    (printout t crlf "MENU DEL DIA " ?d crlf)
    ;(printout t crlf "MENU DEL DIA " crlf)

    (bind ?nutrientes (contar-nutrientes ?lista))

    (printout t crlf "Platos del desayuno: " (send (nth$ 1 ?lista) get-nombre) " y " (send (nth$ 2 ?lista) get-nombre) )
    (printout t crlf "Platos de la comida: " (send (nth$ 3 ?lista) get-nombre) " y " (send (nth$ 4 ?lista) get-nombre) )
    (printout t crlf "Postre de la comida: " (send (nth$ 5 ?lista) get-nombre) )
    (printout t crlf "Platos de la cena: " (send (nth$ 6 ?lista) get-nombre)  " y " (send (nth$ 7 ?lista) get-nombre) )
    (printout t crlf "Postre de la cena: " (send (nth$ 8 ?lista) get-nombre) )
    (printout t crlf crlf "Para su edad y nivel de actividad física se recomienda que consuma " ?cal " calorias.")
    (printout t crlf "Hoy ha consumido " (nth$ 1 ?nutrientes) " calorias en su menú.")
    (printout t crlf "De las cuales " (* 4(nth$ 2 ?nutrientes)) " son hidratos de carbono, " (* 9 (nth$ 3 ?nutrientes))
                        " son grasas, " (* 4 (nth$ 4 ?nutrientes)) " son proteinas y " (nth$ 5 ?nutrientes) " gramos de fibra" crlf)

)

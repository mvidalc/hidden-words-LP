;PRACTICA 1
;Miguel Vidal Coll y Antonio Rotger Lopez
;Juego de las palabras ocultas en Lisp: el jugador debera introducir
;letras para adivinar la palabra hasta que quede sin intentos o 
;la adivine


;Metodo para leer una letra por teclado
(defvar introducida)

(defun leerletra ()

	(setq introducida (read))

)


; método que dibuja el recuadro del menú de interacción 
;para obtener el nombre del usuario 
(defun inicio ()
	(cls)
	(pintarfondo)
	(color 0 0 0)
	(setq x 150)
	(setq y 300)
	(move x y)
	(draw x y 500 300)
	(draw x y 150 100)
	(move 500 100)
	(draw 500 100 150 100)
	(draw 500 100 500 300)
	(goto-xy 24 10)
	(princ "Introduce el nombre del jugador: ")
	(goto-xy 37 14)
	(setq jugador (string (leerletra)))	
	(setq puntos 0)
	(setq jugadas 0)
	(cls)
	(color 0 0 0)
	(mainloop jugador puntos jugadas)
	
	
)

;Método principal donde se lee la palabra a adivinar del fichero y llamando
; a otros metodos dibujará el nombre, puntuación y jugadas, además a cada
; jugada nueva se aumentará el número de jugadas y la puntuación
(defun mainloop (jugador puntos jugadas)

	(cls)

	(reset puntos)
	(setq jugadas (+ jugadas 1))
	;leer palabra del fichero según el nº de jugadas para leer la siguiente
	
	(let ((in (open "palabras.txt")))
		
		(dotimes(i jugadas)
		
			(setq palabra (read-line in nil :eof))
		)
		;Evalua el final de ficherp
		(cond ((eq palabra :eof);SI es final de fichero
				(final2 puntos jugadas jugador)
				(close in)
				)	
			(;NO es final de fichero
				(close in)
				(setq intento 0) 
				(setq puntos (+ puntos 255))

				;dibuja la interfaz 
				(paint palabra)
				(dibujartitulo)
				(printsec jugador 240)
				(setq numero (format nil "~A" puntos))	
				(printsec numero 210)
				(setq numero (format nil "~A" jugadas))	
				(printsec numero 179)

				;se dispone a empezar el juego
				(introducirLetra puntos jugadas jugador intento)
	
			)
		)
	)
)

;Método para mirar si existe una letra en una lista
(defun introducirNuevo (n l)
(cond ((not (member n l)) (cons n l))
)
)
;Método donde se lleva a cabo el juego
;En primer lugar mira si existe la letra en una lista para no 
;contar dos veces las acertadas. Si noexista la inserta.
;En segundo lugar compara la letra introducida con las de la palabra a adivinar
;Si la encuentra,se dibuja en la palabra, sino se dibuja en falladas y
;se reduce la puntuación. Una vez se ha acertado o se han acabado los 
;intentos (17) se acaba la jugada
(defun introducirLetra (puntos jugadas jugador intento )
	(setq adivinar (length palabra))
	(setq adivinadas 0)
	(setq encontrado 0)
	(setq lista nil)
	(loop 
		(goto-xy 1 23)
		(setq encontrado 0)
		(princ "introduce una letra: ")
		(setq insert (read))
		(setq guess (format nil "~A" insert))

		(cond ((not (introducirNuevo insert lista))
		(princ "letra repetida"))
		(
			(setf lista (cons insert lista ))
		(goto-xy 26 23)
		(setq long (length palabra))
		

		;compara cada letra de la palabra con la introducida
		(dotimes (i long)

			(setq letra (string(char palabra i)))

			;Existe
			(cond ((string-equal letra guess)

				(pintarpalabra letra i)
				(setq encontrado (+ encontrado 1))
				)
			)	
			)
			)
		) 
		;No existe
		(cond ((= encontrado 0)

				(pintarCaracter guess auxp 57 20)
				(setq auxp (+ auxp 22))
				(setq puntos (- puntos 15))
				(setq intento (+ intento 1))
				

				;Mientras queden intentos dibujará trozos de la imagen
				(cond ((< intento 17)
					(setq indice (random limite))
					(setq elegido (aref intentos indice))
					(setq imagen(concatenate 'string"fotosIMG/" palabra".img"))
					(dibujar imagen 439 174 elegido)
					(reorganizar indice)
					(color 0 0 0)
					)	
				)
				;Si se queda sin intentos sale del bucle
				(when (= intento 17)(return))
				)
		)
		;Si se adivinala palabra sale del bucle
		(when (= adivinadas adivinar)(return))
	)
	(final puntos jugadas palabra jugador)
)

;Método que dibuja el menú final donde aparece la puntuación
;conseguida y la imagen. Se pide si se desea volver a jugar con
;en otra jugada. Si la respuesta es afirmativa vuelve a "mainloop"
;donde volverá a empezar, sino acaba el programa y se escribe el resultado
;final en un fichero.
(defun final (puntos jugadas palabra jugador)

	;dibuja el menú
	(cls)
	(pintarfondo)
	(color 0 0 0)
	(setq x 219)
	(setq y 153)
	(move x y)
	(draw x y 421 153)
	(draw x y 219 355)
	(move 421 355)
	(draw 421 355 219 355)
	(draw 421 355 421 153)
	(setq imagen (concatenate 'string "fotosIMG/" palabra ".img"))
	(dibujarcolor imagen 220 154)

	(goto-xy 34 16)
	(print palabra)
	(goto-xy 32 18)
	(princ "Puntuacion: ")
	(print puntos)
	(goto-xy 26 20)
	(princ "Quieres volver a jugar? S/N")
	(goto-xy 38 22)
	(setq yesorno (read-line))
	(cond((string-equal yesorno "s")
		
		(mainloop jugador puntos jugadas);SI
		)
		(
		(escribir puntos jugador);NO
		)
	)
)

;Método para finalizar el juego una vez se han acabado las palabras
;del fichero
(defun final2 (puntos jugadas  jugador)

	;dibuja el menú
	(cls)
	(pintarfondo)
	(color 0 0 0)
	(setq x 150)
	(setq y 300)
	(move x y)
	(draw x y 500 300)
	(draw x y 150 100)
	(move 500 100)
	(draw 500 100 150 100)
	(draw 500 100 500 300)
	
	(goto-xy 25 10)
	(princ "Fin del juego: No hay mas palabras")
	(goto-xy 34 16)
	(princ "Puntuacion: ")
	(print puntos)
	(escribir puntos jugador)
)

;Método que reinicia las variables necesarias para seguir con una
;nueva jugada
(defun reset (puntos)
	(setq intentos (vector 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))
	(setq limite 16) 
	(setq elegido 0)
	(setq i 0)
	(setq auxp 200)
	(setq puntos (+ puntos 255))
)

;Método para escribir el resultado final en un fichero
(defun escribir (puntos jugador)
	(setq points (format nil "~A" puntos))
	(setq fichero (open "puntuacion.txt" :direction :output
										 :if-exists :append
										 :if-does-not-exist :create))
	(setq linea (concatenate 'string "[Jugador: " jugador " " "Puntuacion: " points " ]" ))
	(print linea fichero)

)
	

;---------------METODOS PARA PINTAR LA INTERFAZ----------------

;Método para dibujar el fondo del menú
(defun pintarfondo ()
	(setq i 0)
	(color 0 180 153)
	(dotimes (i 374)
		(move 0 i)
		(draw 0 i 640 i)
	)
)

;Método para dibujar las palabras del titulo y secciones de la interfaz
(defun dibujartitulo ()

(dibujarbn "IMG/P.img" 5  320 52)
(dibujarbn "IMG/A.img" 60  320 52)
(dibujarbn "IMG/L.img" 115 320 52)
(dibujarbn "IMG/A.img" 168 320 52)
(dibujarbn "IMG/B.img" 222 320 52)
(dibujarbn "IMG/R.img" 277 320 52)
(dibujarbn "IMG/A.img" 330 320 52)
(dibujarbn "IMG/S.img" 383 320 52)

(dibujarbn "IMG/O.img" 5  267 52)
(dibujarbn "IMG/C.img" 60  267 52)
(dibujarbn "IMG/U.img" 115 267 52)
(dibujarbn "IMG/L.img" 168 267 52)
(dibujarbn "IMG/T.img" 222 267 52)
(dibujarbn "IMG/A.img" 277 267 52)
(dibujarbn "IMG/S.img" 330 267 52)

(dibujarbn "IMG/J_BN.img" 12  240 20)
(dibujarbn "IMG/U_BN.img" 34  240 20)
(dibujarbn "IMG/G_BN.img" 56  240 20)
(dibujarbn "IMG/A_BN.img" 78  240 20)
(dibujarbn "IMG/D_BN.img" 100 240 20)
(dibujarbn "IMG/O_BN.img" 122 240 20)
(dibujarbn "IMG/R_BN.img" 144 240 20)

(dibujarbn "IMG/P_BN.img" 12  210 20)
(dibujarbn "IMG/U_BN.img" 34  210 20)
(dibujarbn "IMG/N_BN.img" 56  210 20)
(dibujarbn "IMG/T_BN.img" 78  210 20)
(dibujarbn "IMG/O_BN.img" 100 210 20)
(dibujarbn "IMG/S_BN.img" 122 210 20)

(dibujarbn "IMG/J_BN.img" 12  179 20)
(dibujarbn "IMG/U_BN.img" 34  179 20)
(dibujarbn "IMG/G_BN.img" 56  179 20)
(dibujarbn "IMG/A_BN.img" 78  179 20)
(dibujarbn "IMG/D_BN.img" 100 179 20)
(dibujarbn "IMG/A_BN.img" 122 179 20)
(dibujarbn "IMG/S_BN.img" 144 179 20)   

)  

;Método para dibujar los cuadros y línias de las letras de la interfaz
(defun paint (palabra)

	(move 0 263)
	(draw 0 263 439 263) ; Titulo
	(setq x 5)
	(setq y 320)
	(cuadricula x y 8 52)
	(setq x 5)
	(setq y (- y 54))
	(cuadricula x y 7 52)

	
	(move 0 235)
	(draw 0 235 439 235) ; jugador
	(setq x 12)
	(setq y 240)
	(cuadricula x y 18 20)
	
	(move 0 205)
	(draw 0 205 439 205) ; puntos
	(setq x 12)
	(setq y 210)
	(cuadricula x y 18 20)
	
	(move 0 174)
	(draw 0 174 639 174) ; jugadas
	(setq x 12)
	(setq y 179)
	(cuadricula x y 18 20 )

	(move 0 90)
	(draw 0 90 639 90) ; Palabra
	(setq x 100)
	(setq y 106)
	(setq longitud (length palabra))
	(cuadricula x y longitud 50)

	(move 0 50)
	(draw 0 50 639 50) 	 ; falladas
	(setq x 200)
	(setq y 57)
	(goto-xy 1 20)
	(princ "LETRAS FALLADAS --->")
	(cuadricula x y 17 20)

	(move 400 500)
	(draw 439 500 439 174) ; linea vertical

	
)


;Método para pintar las letras de la palabra que se van acertando
(defun pintarpalabra (letra i)

	(setq adivinadas (+ adivinadas 1))
	(setq apintar (concatenate 'string "IMG/" letra ".img"))
	(color 0 0 0)
	(setq x 100)
	(setq desp (* 52 i))
	(setq x (+ x desp))
	(dibujarbn apintar x 104 52)
	(setq x 100)

)

;Método para pintar una secuencia de carácteres de un string
(defun printsec (palabra pos)
		
	(setq long (length palabra))
	(setq x 406)
	(setq y pos)
	(setq desp (* 22 long))
	(setq x (- x desp))
	

	(dotimes (i long)
		(setq letra (string(char palabra i)))
		(pintarCaracter letra x y 20)
		(setq x (+ 22 x))
		(setq y pos)
		
	)

)

;Método para pintar un carácter de string
(defun pintarCaracter (letra x y tam)
		
	(setq apintar (concatenate 'string "IMG/" letra "_NB.img"))
	(color 0 0 0)
	(dibujarbn apintar x y 20)	

)

;Método para dibujar una cuadrícula para las letras de la interfaz
(defun Cuadricula (x y rept tam) 

	(dotimes (i rept)
	(move x y)
	(draw x y (+ x tam) y)
	(draw (+ x tam) y (+ x tam) (+ y tam))
	(draw x (+ y tam) x y)
	(setq x (+ x (+ tam 2)))
	)
)

;-------METODOS PARA DIBUJAR IMAGEN------------

;Método para dibujar imágenes en blanco y negro
(defun dibujarbn (imagen a b tam)
	(setq fichero (open imagen :direction :input 
	:element-type 'unsigned-byte))
	(setq pixel 1)
	(setq BN 0)
	(setq x a)
	(setq y b)
	(move x y)
	(loop 
		(setq BN (read-byte fichero nil))
		(if (null BN) (return ()))
		(cond ( (= BN 0)  (draw (+ 1 x) y) ) )
		(setq pixel (+ pixel 1))
		(setq x (+ x 1))
		(cond  ((> pixel tam) (setq pixel 1) (setq x a) (setq y (+ y 1)) ) )
		(move x y)
	) 
       (close fichero)
)

;Método para dibujar imágenes en color
(defun dibujarcolor (nombre a b )
	
	(setq fichero (open nombre :direction :input
	:element-type 'unsigned-byte))
	(setq pixel 1)
	(setq x a )
	(setq y b)
	(setq R 0 G 0 B 0)
	(move x y)
	(loop
			(setq B (read-byte fichero nil))
			(setq G (read-byte fichero nil))
			(setq R (read-byte fichero nil))
			(if (null B) (return()))
			(color R G B)(draw (+ 1 x) y)  
			(setq pixel (+ pixel 1))
			(setq x (+ x 1))
			(cond 	((> pixel 200)		(setq pixel 1)
										(setq x a)
										(setq y (+ y 1)) ) )
			(move x y)
	)
	(color 1 1 1)
	(close fichero)
)


; Métodos para dibujar trozos aleatorios sin repetir de una imágen en color	
(defun dibujar (nombre coordX coordY indice)
	(trozos coordX coordY)
	(setq fichero (open nombre :direction :input
	:element-type 'unsigned-byte))
	(setq pixel 1)
	(setq x (aref (aref limites 0) 0) )
	(setq y (aref (aref limites 0) 1) )
	(setq R 0 G 0 B 0)
	(move x y)
	(loop
			(setq B (read-byte fichero nil))
			(setq G (read-byte fichero nil))
			(setq R (read-byte fichero nil))
			(if (null B) (return()))
			(color R G B)
			(cond ((and 	(> x (aref (aref limites indice) 0))
							(< x (aref (aref limites indice) 2))
							(> y (aref (aref limites indice) 1))
							(< y (aref (aref limites indice) 3))
			) (draw (+ 1 x) y) ) )
			(setq pixel (+ pixel 1))
			(setq x (+ x 1))
			(cond 	((> pixel 200)		(setq pixel 1)
										(setq x (aref (aref limites 0) 0))
										(setq y (+ y 1)) ) )
			(move x y)
	) (color 1 1 1)
	(close fichero)
)

;Método para trozear una imagen
(defun trozos (x1 y1)

	(setq limites (vector

		(vector (+ x1   1) (+ y1   1) (+ x1  50) (+ y1  50) )
		(vector (+ x1  51) (+ y1   1) (+ x1 100) (+ y1  50) )
		(vector (+ x1 101) (+ y1   1) (+ x1 150) (+ y1  50) )
		(vector (+ x1 151) (+ y1   1) (+ x1 200) (+ y1  50) )

		(vector (+ x1   1) (+ y1  51) (+ x1  50) (+ y1 100) )
		(vector (+ x1  51) (+ y1  51) (+ x1 100) (+ y1 100) )
		(vector (+ x1 101) (+ y1  51) (+ x1 150) (+ y1 100) )
		(vector (+ x1 151) (+ y1  51) (+ x1 200) (+ y1 100) )	

		(vector (+ x1   1) (+ y1 101) (+ x1  50) (+ y1 150) )
		(vector (+ x1  51) (+ y1 101) (+ x1 100) (+ y1 150) )
		(vector (+ x1 101) (+ y1 101) (+ x1 150) (+ y1 150) )
		(vector (+ x1 151) (+ y1 101) (+ x1 200) (+ y1 150) )

		(vector (+ x1   1) (+ y1 151) (+ x1  50) (+ y1 200) )
		(vector (+ x1  51) (+ y1 151) (+ x1 100) (+ y1 200) )
		(vector (+ x1 101) (+ y1 151) (+ x1 150) (+ y1 200) )
		(vector (+ x1 151) (+ y1 151) (+ x1 200) (+ y1 200) )

		)
	)
)

;Método para no repetir el un trozo de imagen
(defun reorganizar (indice)
	(dotimes (j limite)
		(if (> j indice) (setf (aref intentos (- j 1)) (aref intentos j)))
	)
	(setq limite (1- limite))
)

(inicio)	
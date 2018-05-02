;;;; package.lisp

(defpackage #:escenario
  (:use #:clim
        #:clim-lisp
        #:mcclim-render
        #:3d-vectors
        #:3d-matrices)
  (:export #:escenario
           #:*ancho*
           #:*alto*
           #:regenera
           #:dibuja-mapa
           #:dibuja-manos
           #:dibuja-dialogo
           #:mapa
           #:imagen
           #:rota
           #:mueve
           #:posición
           #:sonidos
           #:sprites
           #:sprite-x
           #:sprite-y
           #:escenario
           #:escenario-revisa-eventos
           #:escenario-realiza-personajes
           #:crea-escenario
           #:carga-texturas
           #:carga-sprites
           #:personaje-realiza-comportamiento
           #:*mapas*
           #:*sprites*
           #:*sprites-maestros*
           #:inicia-hilos
           #:termina-hilos))

(defpackage #:mc-wolf3d
  (:nicknames :wol3d)
  (:use #:clim
        #:clim-lisp
        #:mcclim-render
        #:3d-vectors
        #:3d-matrices
        #:escenario)
  (:export #:wolf3d-main))


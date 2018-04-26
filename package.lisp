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
           #:mapa
           #:imagen
           #:rota
           #:mueve
           #:sonidos
           #:sprites
           #:sprite-x
           #:sprite-y
           #:escenario-revisa-eventos
           #:escenario-realiza-personajes
           #:crea-escenario
           #:carga-texturas
           #:carga-sprites
           #:*mapa*
           #:*mapa-3*
           #:inicia-hilos
           #:termina-hilos))

(defpackage #:personajes
  (:use #:clim
        #:clim-lisp
        #:mcclim-render
        #:3d-vectors
        #:3d-matrices)
  (:import-from #:escenario
                #:escenario
                #:sprites
                #:sprite-x
                #:sprite-y
                #:posición
                #:texturas)
  (:export #:personaje
           #:crea-personaje
           #:personaje-realiza-comportamiento))

(defpackage #:mc-wolf3d
  (:nicknames :wol3d)
  (:use #:clim
        #:clim-lisp
        #:mcclim-render
        #:3d-vectors
        #:3d-matrices
        #:escenario
        #:personajes)
  (:export #:wolf3d-main))


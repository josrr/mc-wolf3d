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
           #:escenario-revisa-eventos
           #:crea-escenario
           #:carga-texturas
           #:carga-sprites
           #:*mapa*
           #:*mapa-3*
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


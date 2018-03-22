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
           #:imagen
           #:rota
           #:mueve
           #:carga-texturas
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


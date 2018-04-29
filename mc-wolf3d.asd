;;;; mc-wolf3d.asd

(asdf:defsystem #:mc-wolf3d
  :description "Pequeño motor raycasting"
  :author "José Miguel Ronquillo Rivera <jose@rufina.link>"
  :license  "GPL Ver. 3"
  :version "0.0.1"
  :serial t
  :depends-on (:mcclim
               :mcclim-raster-image
               :3d-vectors
               :3d-matrices
               :local-time
               :mixalot
               :mixalot-vorbis)
  :components ((:file "package")
               (:file "texturas")
               (:file "personajes")
               (:file "sprites")
               (:file "mapas")
               (:file "sonidos")
               (:file "escenario")
               (:file "mc-wolf3d")))

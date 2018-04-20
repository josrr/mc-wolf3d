;;;; mc-wolf3d.asd

(asdf:defsystem #:mc-wolf3d
  :description "Pequeño motor raycasting"
  :author "José Ronquillo Rivera <jose@rufina.link>"
  :license  "GPL Ver. 3"
  :version "0.0.1"
  :serial t
  :depends-on (:mcclim
               :mcclim-raster-image
               :3d-vectors
               :3d-matrices
               :cl-colors
               :local-time
               :mixalot
               :mixalot-vorbis)
  :components ((:file "package")
               (:file "texturas")
               (:file "sprites")
               (:file "mapas")
               (:file "sonidos")
               (:file "escenario")
               (:file "mc-wolf3d")))

(in-package #:escenario)

(declaim (type (cons (simple-array fixnum (24 24))) *mapas*))

(defparameter *mapas*
  (list
   (make-array '(24 24)
               :element-type 'fixnum
               :initial-contents
               '((2 2 2 1 2 1 2 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2)
                 (7 0 0 0 0 0 0 0 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2)
                 (2 2 2 2 2 2 2 0 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2)
                 (2 2 2 2 2 2 2 0 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2)
                 (2 2 2 2 2 2 2 0 2 2 2 2 1 2 2 2 1 2 2 2 1 2 2 2)
                 (2 2 2 2 2 2 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)
                 (2 2 2 2 2 2 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2)
                 (2 2 2 2 2 2 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2)
                 (2 2 2 2 2 2 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2)
                 (2 2 2 2 2 2 1 2 2 2 2 2 2 2 0 0 2 2 2 2 2 2 2 1)
                 (2 2 2 2 2 2 2 0 0 0 0 0 0 2 0 0 2 0 0 0 0 0 0 2)
                 (2 2 2 2 2 2 2 0 0 0 0 0 0 2 0 0 2 0 0 0 0 0 0 2)
                 (2 2 2 2 2 2 2 0 0 0 0 0 0 2 0 0 2 0 0 0 0 0 0 2)
                 (2 2 2 2 2 2 1 0 0 0 0 0 0 2 0 0 2 0 0 0 0 0 0 1)
                 (2 2 2 2 2 2 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2)
                 (2 2 2 2 2 2 2 2 2 2 2 2 2 2 0 0 2 2 2 2 2 2 2 2)
                 (2 2 2 2 2 2 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2)
                 (2 2 2 2 2 2 1 0 0 0 0 0 0 2 0 0 2 0 0 0 0 0 0 1)
                 (2 2 2 2 2 2 2 0 0 0 0 0 0 2 0 0 2 0 0 0 0 0 0 2)
                 (2 2 2 2 2 2 2 0 0 0 0 0 0 2 0 0 2 0 0 0 0 0 0 2)
                 (2 2 2 2 2 2 2 0 0 0 0 0 0 2 0 0 2 0 0 0 0 0 0 2)
                 (2 2 2 2 2 2 1 0 0 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1)
                 (2 2 2 2 2 2 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 7)
                 (2 2 2 2 2 2 2 2 1 2 2 2 1 2 2 2 1 2 2 2 1 2 2 2)))))

(declaim (type (simple-vector 12) *colores-mapa*))
(defparameter *colores-mapa*
  (make-array 12 :initial-contents (list +royal-blue+ +gray10+ +gray20+ +gray30+
                                         +gray40+ +gray50+ +gray60+ +gray70+
                                         +gray80+ +gray90+ +gray100+ +blue+)))

(defun dibuja-mapa (escenario pane x y tamaño)
  (declare (optimize (speed 3) (safety 0))
           (type fixnum x y tamaño))
  (let ((y-fin (+ y tamaño)))
    (declare (type fixnum y-fin))
    (with-translation (pane x y-fin)
      (loop with mapa of-type (simple-array fixnum (24 24)) = (mapa escenario)
         and pos-x single-float = (vx2 (posición escenario))
         and pos-y single-float = (vy2 (posición escenario))
         and dir-x = (vx2 (dirección escenario))
         and dir-y = (vy2 (dirección escenario))
         with num-columnas fixnum = (array-dimension mapa 1)
         with Δx fixnum = (truncate (/ tamaño num-columnas))
         for j fixnum from 0 below (array-dimension mapa 0)
         do (loop for i fixnum from 0 below num-columnas
               do (draw-rectangle* pane
                                   (* j Δx) (- (* i Δx))
                                   (* (1+ j) Δx) (- (* (1+ i) Δx))
                                   :ink (aref *colores-mapa*
                                              (aref mapa j i))))
         finally (let ((centro (make-point (* pos-x Δx)
                                           (- (* pos-y Δx))))
                       (personajes (personajes escenario)))
                   (declare (type (simple-array personaje) personajes))
                   (with-rotation (pane (atan dir-x dir-y) centro)
                     (draw-arrow* pane
                                  (* pos-x Δx)
                                  (- (* (1- pos-y) Δx))
                                  (* pos-x Δx)
                                  (- (* (1+ pos-y) Δx))
                                  :line-thickness 1.75
                                  :head-length 8
                                  :head-width 10
                                  :ink +turquoise+))
                   (when personajes
                     (loop for p across personajes
                        for s = (personaje-sprite p) do
                          (draw-circle* pane
                                        (* (sprite-x s) Δx) (- (* (sprite-y s) Δx))
                                        (/ Δx 2) :ink +red+))))))))

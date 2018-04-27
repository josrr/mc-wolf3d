(in-package #:escenario)

(defparameter *comportamiento*
  (let (;(matrot (mrotation +vz+ (/ pi 16.0)))
        (velocidad 0.01)
        (x-ant 0.0)
        (y-ant 0.0))
    (declare (type single-float velocidad x-ant y-ant))
    (lambda (escenario personaje)
      (let ((pos (posición escenario))
            (sprite (personaje-sprite personaje))
            (mapa (mapa escenario)))
        (labels ((calcula-posicion (dir-x dir-y)
                   (let* ((nueva-x (+ (sprite-x sprite) (* (+ velocidad (- (random 0.01) 0.005)) dir-x)))
                          (nueva-y (+ (sprite-y sprite) (* (+ velocidad (- (random 0.01) 0.005)) dir-y)))
                          (x-i (floor nueva-x))
                          (y-i (floor nueva-y)))
                     (when (and (> x-i 0) (< x-i (array-dimension mapa 1))
                                (> y-i 0) (< y-i (array-dimension mapa 0))
                                (zerop (aref mapa x-i y-i))
                                (/= x-ant x-i)
                                (/= y-ant y-i))
                       (setf (sprite-x sprite) nueva-x
                             (sprite-y sprite) nueva-y
                             x-ant nueva-x
                             y-ant nueva-y)
                       t))))
          (loop for vecdir = (vec2 (- (vx2 pos) (sprite-x sprite))
                                   (- (vy2 pos) (sprite-y sprite)))
             then (vxy (m* (mrotation +vz+ (/ pi 1/2 (1+ (random 8.0)))) (vxy__ vecdir)))
             while (null (calcula-posicion (vx2 vecdir) (vy2 vecdir))))))
      t)))

(defclass personaje ()
  ((sprite :initform nil :initarg :sprite :accessor personaje-sprite)
   (direccion :initform (vec2 0 -1) :accessor personaje-direccion)
   (comportamiento :initform *comportamiento*
                   :initarg :comportamiento :accessor personaje-comportamiento)))

(defun crea-personaje (sprite &optional (comportamiento *comportamiento*))
  (make-instance 'personaje
                 :sprite sprite
                 :comportamiento (or comportamiento *comportamiento*)))

(defun personajes-redefine-comportamiento (escenario nuevo-comportamiento)
  (loop for p across (personajes escenario) do
       (setf (personaje-comportamiento p) nuevo-comportamiento)))

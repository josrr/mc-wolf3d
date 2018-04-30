(in-package #:escenario)

(declaim (type single-float *velocidad-personaje*))
(defparameter *velocidad-personaje* 0.005)

(defparameter *comportamiento*
  (lambda () (declare (optimize (speed 3) (safety 0)))
          (lambda (escenario personaje)
            (let* ((pos (posición escenario))
                   (sprite (personaje-sprite personaje))
                   (mapa (mapa escenario)))
              (declare (type (simple-array fixnum (24 24)) mapa))
              (labels ((gira-y-avanza (angulo paso)
                         (declare (type single-float angulo paso))
                         (values (+ (sprite-x sprite) (* paso (cos angulo)))
                                 (+ (sprite-y sprite) (* paso (sin angulo)))))
                       (calcula-posicion (dir-x dir-y)
                         (let* ((nueva-x (+ (sprite-x sprite) (* (+ *velocidad-personaje* (random 0.02) -0.01) dir-x)))
                                (nueva-y (+ (sprite-y sprite) (* (+ *velocidad-personaje* (random 0.02) -0.01) dir-y)))
                                (tipo (aref mapa
                                            (the fixnum (truncate nueva-x))
                                            (the fixnum (truncate nueva-y)))))
                           (declare (type single-float nueva-x nueva-y))
                           (when (zerop tipo)
                             (setf (sprite-x sprite) nueva-x
                                   (sprite-y sprite) nueva-y)
                             t))))
                (loop for vecdir = (vec2 (- (vx2 pos) (sprite-x sprite))
                                         (- (vy2 pos) (sprite-y sprite)))
                   then (multiple-value-bind (n-x n-y)
                            (gira-y-avanza (coerce (/ pi 4.0) 'single-float) 10.0)
                          (declare (type single-float n-x n-y))
                          (vec2 (- n-x (sprite-x sprite)) (- n-y (sprite-y sprite))))
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
                 :comportamiento (when comportamiento
                                   (if (functionp comportamiento)
                                       (funcall comportamiento)
                                       comportamiento))))

(defun personajes-redefine-comportamiento (escenario nuevo-comportamiento)
  (loop for p across (personajes escenario) do
       (setf (personaje-comportamiento p) (if (functionp nuevo-comportamiento)
                                              (funcall nuevo-comportamiento)
                                              nuevo-comportamiento))))

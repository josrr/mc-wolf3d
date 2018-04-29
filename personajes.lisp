(in-package #:escenario)

(declaim (type single-float *velocidad-personaje*))
(defparameter *velocidad-personaje* 0.005)

(defparameter *comportamiento*
  (lambda () ;;(declare (optimize (speed 3) (safety 0)))
    (lambda (escenario personaje)
      (let* ((pos (posición escenario))
             (sprite (personaje-sprite personaje))
             (mapa (mapa escenario))
             (x-max (1- (array-dimension mapa 0)))
             (y-max (1- (array-dimension mapa 1))))
        (declare (type (simple-array fixnum (24 24)) mapa)
                 (type fixnum x-max y-max))
        (labels ((calcula-posicion-vacia (x-i y-i)
                   (declare (type fixnum x-i y-i))
                   (let ((n-x) (n-y))
                     (loop named ciclo
                        with x-ini fixnum = (1- x-i) and y-ini fixnum = (1- y-i)
                        and x-fin fixnum = (1+ x-i) and y-fin fixnum = (1+ y-i)
                        for j fixnum from (if (minusp y-ini) 0 y-ini) to (if (> y-fin y-max) y-max y-fin)
                        do (loop for i fixnum from (if (minusp x-ini) 0 x-ini) to (if (> x-fin x-max) x-max x-fin)
                              if (and (/= j y-i) (/= i x-i) (zerop (aref mapa i j))) do
                                (setf n-x (coerce i 'single-float)
                                      n-y (coerce j 'single-float))
                                (return-from ciclo)))
                     (if (and n-x n-y)
                         (values n-x n-y)
                         (let ((angulo (coerce (random (* 2.0 pi)) 'single-float)))
                           (values (+ x-i (sin angulo))
                                   (+ y-i (cos angulo)))))))
                 (gira-y-avanza (x-i y-i angulo paso)
                   (declare (type fixnum x-i y-i)
                            (type single-float angulo paso))
                   (values (+ x-i (* paso (cos angulo)))
                           (+ y-i (* paso (sin angulo)))))
                 (calcula-posicion (dir-x dir-y)
                   (let* ((nueva-x (+ (sprite-x sprite) (* (+ *velocidad-personaje* (random 0.02) -0.01) dir-x)))
                          (nueva-y (+ (sprite-y sprite) (* (+ *velocidad-personaje* (random 0.02) -0.01) dir-y)))
                          (tipo (aref mapa (the fixnum (floor nueva-x)) (the fixnum (floor nueva-y)))))
                     (declare (type single-float nueva-x nueva-y))
                     ;;(log:info dir-x dir-y nueva-x nueva-y tipo)
                     (when (zerop tipo)
                       (setf (sprite-x sprite) nueva-x
                             (sprite-y sprite) nueva-y)
                       t))))
          (loop for vecdir = (vec2 (- (vx2 pos) (sprite-x sprite))
                                   (- (vy2 pos) (sprite-y sprite)))
             then (multiple-value-bind (n-x n-y)
                      (gira-y-avanza (the fixnum (round (the single-float (sprite-x sprite))))
                                     (the fixnum (round (the single-float (sprite-y sprite))))
                                     (coerce (/ pi 2.0) 'single-float) 10.0)
                    ;;(calcula-posicion-vacia (the fixnum (round (the single-float (sprite-x sprite)))) (the fixnum (round (the single-float (sprite-y sprite)))))
                    (declare (type single-float n-x n-y))
                    ;;(log:info "~S ~S [~S, ~S] -> [~S, ~S]" (personaje-id personaje) (sprite-nombre (sprite-maestro (personaje-sprite personaje))) (sprite-x sprite) (sprite-y sprite) n-x n-y)
                    (vec2 (- n-x (sprite-x sprite)) (- n-y (sprite-y sprite))))
             ;;(vxy (m* (mrotation +vz+ (/ pi 1/2 (1+ (random 8.0)))) (vxy__ vecdir)))
             while (null (calcula-posicion (vx2 vecdir) (vy2 vecdir))))))
      t)))

(defclass personaje ()
  ((sprite :initform nil :initarg :sprite :accessor personaje-sprite)
   (direccion :initform (vec2 0 -1) :accessor personaje-direccion)
   (comportamiento :initform *comportamiento*
                   :initarg :comportamiento :accessor personaje-comportamiento)))

(defun crea-personaje (sprite &optional (comportamiento *comportamiento*))
  (let ((comp (or comportamiento *comportamiento*)))
    (make-instance 'personaje
                   :sprite sprite
                   :comportamiento (if (functionp comp) (funcall comp) comp))))

(defun personajes-redefine-comportamiento (escenario nuevo-comportamiento)
  (loop for p across (personajes escenario) do
       (setf (personaje-comportamiento p) (if (functionp nuevo-comportamiento)
                                              (funcall nuevo-comportamiento)
                                              nuevo-comportamiento))))
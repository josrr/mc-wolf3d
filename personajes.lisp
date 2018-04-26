(in-package #:personajes)

(defparameter *comportamiento*
  (lambda (escenario sprite)
    (let ((pos (posición escenario)))
      (setf (sprite-x sprite) (+ (sprite-x sprite) (* 0.0075 (- (vx2 pos) (sprite-x sprite))))
            (sprite-y sprite) (+ (sprite-y sprite) (* 0.0075 (- (vy2 pos) (sprite-y sprite))))))))

(defclass personaje ()
  ((sprite :initform nil :initarg :sprite :accessor personaje-sprite)
   (comportamiento :initform *comportamiento*
                   :initarg :comportamiento :accessor personaje-comportamiento)))

(defun crea-personaje (sprite &optional (comportamiento *comportamiento*))
  (make-instance 'personaje
                 :sprite sprite
                 :comportamiento (or comportamiento *comportamiento*)))

(defgeneric personaje-realiza-comportamiento (personaje escenario))

(defmethod personaje-realiza-comportamiento ((personaje personaje) (escenario escenario))
  (with-slots (comportamiento sprite) personaje
    (when comportamiento
      (funcall comportamiento escenario sprite))))

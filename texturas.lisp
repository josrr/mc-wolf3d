(in-package #:escenario)

(defun color-a-entero (color)
  (multiple-value-bind (r g b) (color-rgb color)
    (+ (ash (floor (* 255.0 b)) 16)
       (ash (floor (* 255.0 g)) 8)
       (truncate (* 255.0 r)))))

(defun carga-archivo (archivo)
  (labels ((opticolor-a-entero (r g b)
             (+ (ash b 16) (ash g 8) r)))
    (let ((img (opticl:read-image-file archivo)))
      (when img
        (let* ((height (array-dimension img 0))
               (width (array-dimension img 1))
               (array (make-array (list height width)
                                  :element-type '(unsigned-byte 32))))
          (opticl:do-pixels (y x) img
            (setf (aref array y x)
                  (opticolor-a-entero (aref img y x 0)
                                      (aref img y x 1)
                                      (aref img y x 2))))
          array)))))

(defun carga-texturas (&optional (ruta #P"./pics/"))
  (let ((archivos (directory (merge-pathnames "*.png" (or ruta #P"./pics/")))))
    (make-array (length archivos)
                :element-type 'simple-array
                :initial-contents
                (mapcar #'carga-archivo archivos))))


(defclass escenario ()
  ((ancho :initarg :ancho :accessor ancho :initform *ancho* :type single-float)
   (alto :initarg :ancho :accessor alto :initform *alto* :type single-float)
   (posición :initarg :posicion :accessor posición :initform (vec2 1.0 1.0))
   (dirección :initarg :dirección :accessor dirección :initform (vec2 -1 0))
   (plano-camara :initarg :plano-camara :accessor plano-camara :initform (vec2 0 0.66))
   (rayos-pares :accessor rayos-pares :initform t :type boolean)
   (vel-mov :accessor vel-mov :initform 0.15 :type single-float)
   (vel-rot :accessor vel-rot :initform (aproxima-angulo (coerce (* 3.0 (/ pi 128.0)) 'single-float)) :type single-float)
   (zbuffer :accessor zbuffer :initform (make-array (truncate *ancho*) :element-type 'single-float :initial-element 0.0))
   (imagen :accessor imagen
           :initform (make-image :rgb (floor *ancho*) *alto-fix*
                                 :two-dim-array))
   (mapa :initarg :mapa :accessor mapa :initform nil :type (simple-array fixnum (24 24)))
   (sprites :initform nil :initarg :sprites :accessor sprites :type (simple-array sprite))
   (texturas :initarg :texturas :accessor texturas :initform nil :type (simple-array (simple-array (unsigned-byte 32)
                                                                                                   (*tex-ancho-fix* *tex-alto-fix*))
                                                                                     *))
   (sonidos :initform nil :initarg :sonidos :accessor sonidos :type (simple-array t))))

;;; Sprites
(defclass sprite ()
  ((x :initform 0.0 :type single-float :accessor sprite-x :initarg :x)
   (y :initform 0.0 :type single-float :accessor sprite-y :initarg :y)
   (frontera :initform  #(0 0 123 123):type (simple-array fixnum) :accessor sprite-frontera :initarg :frontera)
   (posición :initform (vec2 0.0 0.0) :accessor sprite-posición :initarg :posición)
   (textura :initform 0 :type fixnum :accessor sprite-textura :initarg :textura)
   (eventos :initform nil :accessor sprite-eventos :initarg :eventos)))

(defparameter *sprites* '((1.5 22.5 23)
                          (1.5 1.5 23)
                          (1.5 2.5 8)
                          (2.5 1.5 8)
                          (3.5 1.5 8)
                          (1.51 1.51 9)
                          (2.5 1.5 9)
                          (3.5 1.5 9)
                          (6.5 7.5 9)
                          (4.5 1.5 22)
                          (1.5 5.0 21)
                          (1.5 5.75 21)
                          (1.5 6.50 21)
                          (1.5 7.25 21)
                          (1.5 8.0 21)
                          (1.5 8.75 21)
                          (1.5 9.50 21)
                          (1.5 10.25 21)
                          (1.5 11.0 21)
                          (1.5 11.75 21)
                          (1.5 12.5 22)
                          (6.5 8.5 21)))

(defparameter *sprites-fronteras* '((8 . #(34 62 95 123))
                                    (9 . #(45 0 85 21))))

(defparameter *sprites-eventos* '((21 . ((:contacto . (:snd . 2))))
                                  (22 . ((:contacto . (:snd . 2))))))

(defun carga-sprites (sprites fronteras eventos)
  (make-array (length sprites)
              :element-type 'sprite
              :initial-contents (mapcar (lambda (s)
                                          (let ((obj (make-instance 'sprite
                                                                    :x (car s) :y (cadr s)
                                                                    :posición (vec2 (car s) (cadr s))
                                                                    :textura (caddr s))))
                                            (when fronteras
                                              (let ((def (cdr (assoc (caddr s) fronteras))))
                                                (setf (sprite-frontera obj)
                                                      (if def def `#(0 0 ,(1- *tex-ancho-fix*) ,(1- *tex-alto-fix*))))))
                                            (when eventos
                                              (let ((def (cdr (assoc (caddr s) eventos))))
                                                (when def
                                                  (setf (sprite-eventos obj) def))))
                                            obj))
                                        sprites)))

(defgeneric escenario-realiza-eventos (escenario mezclador))

(defmethod escenario-realiza-eventos ((escenario escenario) mezclador)
  (declare (optimize (speed 3)))
  (with-slots (sprites sonidos posición) escenario
    (when sprites
      (loop for sprite across sprites do
         ;;(log:info sprite)
           (loop for ev in (sprite-eventos sprite) do
                (case (car ev)
                  (:contacto (when (and (<= (abs (- (vx2 posición) (sprite-x sprite))) 0.35)
                                        (<= (abs (- (vy2 posición) (sprite-y sprite))) 0.35))
                               (case (cadr ev)
                                 (:snd (let ((snd (aref sonidos (cddr ev))))
                                         (if (mixalot:streamer-paused-p snd mezclador)
                                             (progn
                                               (mixalot:streamer-seek snd mezclador 0)
                                               (mixalot:streamer-unpause snd mezclador))
                                             (if  (member snd (mixalot:mixer-stream-list mezclador))
                                                  (when (> (mixalot:streamer-position snd mezclador)
                                                           22050)
                                                    (mixalot:streamer-seek snd mezclador 0))
                                                  (mixalot:mixer-add-streamer mezclador snd))))))))))))))

(in-package #:escenario)

(defun color-a-entero (color)
  (multiple-value-bind (r g b) (color-rgb color)
    (+ (ash (floor (* 255.0 b)) 16)
       (ash (floor (* 255.0 g)) 8)
       (truncate (* 255.0 r)))))

(defun opticolor-a-entero (r g b)
  (+ (ash b 16)
     (ash g 8)
     (truncate r)))

(defun carga-archivo (archivo)
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
        array))))

(defun carga-texturas (&optional (ruta #P"./pics/"))
  (let ((archivos (directory (merge-pathnames "*.png" (or ruta #P"./pics/")))))
    (make-array (length archivos)
                :element-type 'simple-array
                :initial-contents
                (mapcar #'carga-archivo archivos))))


;;; Sprites

(defclass sprite ()
  ((x :initform 0.0 :type single-float :accessor sprite-x :initarg :x)
   (y :initform 0.0 :type single-float :accessor sprite-y :initarg :y)
   (posición :initform (vec2 0.0 0.0) :accessor sprite-posición :initarg :posición)
   (textura :initform 0 :type fixnum :accessor sprite-textura :initarg :textura)))

(defparameter *sprites* '((1.5 2.5 8)
                          (2.5 1.5 8)
                          (3.5 1.5 8)
                          (1.5 1.5 9)
                          (2.5 1.5 9)
                          (3.5 1.5 9)
                          (6.5 7.5 9)
                          (1.35 1.35 19)))

(defun carga-sprites (&optional (sprites *sprites*))
  (make-array (length sprites)
              :element-type 'sprite
              :initial-contents (mapcar (lambda (s)
                                          (make-instance 'sprite
                                                         :x (car s) :y (cadr s)
                                                         :posición (vec2 (car s) (cadr s))
                                                         :textura (caddr s)))
                                        sprites)))

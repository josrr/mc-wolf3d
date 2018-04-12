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
   (frontera :initform  #(0 0 123 123):type (simple-array fixnum) :accessor frontera :initarg :frontera)
   (posición :initform (vec2 0.0 0.0) :accessor sprite-posición :initarg :posición)
   (textura :initform 0 :type fixnum :accessor sprite-textura :initarg :textura)))

(defparameter *sprites* '((1.5 2.5 8)
                          (2.5 1.5 8)
                          (3.5 1.5 8)
                          (1.5 1.5 9)
                          (2.5 1.5 9)
                          (3.5 1.5 9)
                          (6.5 7.5 9)
                          (4.5 1.5 22)
                          (2.0 5.0 21)
                          (2.0 5.75 21)
                          (2.0 6.50 21)
                          (2.0 7.25 21)
                          (2.0 8.0 21)
                          (2.0 8.75 21)
                          (2.0 9.50 21)
                          (2.0 10.25 21)
                          (2.0 11.0 21)
                          (2.0 11.75 21)
                          (2.0 12.5 22)
                          (6.5 8.5 21)))

(defparameter *sprites-fronteras* '((8 . #(34 62 95 123))
                                    (9 . #(45 0 85 21))))

(defun carga-sprites (&optional (sprites *sprites*) (fronteras *sprites-fronteras*))
  (make-array (length sprites)
              :element-type 'sprite
              :initial-contents (mapcar (lambda (s)
                                          (let ((obj (make-instance 'sprite
                                                                    :x (car s) :y (cadr s)
                                                                    :posición (vec2 (car s) (cadr s))
                                                                    :textura (caddr s))))
                                            (when (and fronteras)
                                              (let ((def (cdr (assoc (caddr s) fronteras))))
                                                (setf (frontera obj) (if def def `#(0 0 ,(1- *tex-ancho-fix*) ,(1- *tex-alto-fix*))))))
                                            obj))
                                        sprites)))

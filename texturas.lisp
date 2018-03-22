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

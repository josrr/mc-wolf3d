(in-package #:escenario)

(defparameter *sprites-maestros* '((:portal-a
                                    #(10 11 12 13 14) #(#(60 0 70 127)
                                                        #(20 0 107 127)
                                                        #(0 0 127 127)
                                                        #(20 0 107 127)
                                                        #(60 0 70 127))
                                    ((:contacto . (:sonido . 2))))))

(defparameter *sprites* '((:portal-a 5.0 5.0)
                          (:portal-a 5.75 5.0)
                          (:portal-a 6.5 5.0)))

(defclass sprite-maestro ()
  ((nombre :initform :obj-1 :type :keyword :initarg :nombre
           :accessor sprite-nombre)
   (texturas :initform #(0) :type (simple-array fixnum)
             :accessor sprite-texturas :initarg :texturas)
   (fronteras :initform  #((0 0 127 127)) :type (simple-array (simple-array fixnum (4)))
              :accessor sprite-fronteras :initarg :fronteras)
   (eventos :initform nil
            :accessor sprite-eventos :initarg :eventos)))

(defun carga-sprites-maestros (definiciones-sprites)
  (let ((sprites-ht (make-hash-table)))
    (dolist (s definiciones-sprites)
      (let ((obj (make-instance 'sprite-maestro
                                :nombre (car s)
                                :texturas (cadr s)
                                :fronteras (or (caddr s)
                                               `#(0 0 ,(1- *tex-ancho-fix*)
                                                  ,(1- *tex-alto-fix*)))))
            (eventos (cadddr s)))
        (when eventos
          (setf (sprite-eventos obj)
                (mapcar (lambda (ev)
                          (make-instance
                           (intern (concatenate 'string "EVENTO-"
                                                (symbol-name (car ev)))
                                   'escenario)
                           :accion (make-instance
                                    (intern (concatenate 'string
                                                         "ACCION-"
                                                         (symbol-name (cadr ev)))
                                            'escenario)
                                    :pars (cddr ev))))
                        eventos)))
        (setf (gethash (car s) sprites-ht) obj)))
    sprites-ht))

(defclass sprite ()
  ((x :initform 0.0 :type single-float :accessor sprite-x :initarg :x)
   (y :initform 0.0 :type single-float :accessor sprite-y :initarg :y)
   (sprite :initform nil :accessor sprite-maestro :initarg :sprite-maestro)))

(defun crea-sprites (sprites-maestros definiciones-sprites)
  (make-array (length definiciones-sprites)
              :element-type 'sprite
              :initial-contents (mapcar (lambda (def)
                                          (make-instance 'sprite
                                                         :x (cadr def) :y (caddr def)
                                                         :sprite-maestro (gethash (car def) sprites-maestros)))
                                        definiciones-sprites) ))

(declaim (inline sprites-ordena))
(defun sprites-ordena (posición sprites)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type (simple-array sprite) sprites))
  (sort sprites #'> :key (lambda (s)
                           (declare (optimize (speed 3) (safety 0) (debug 0)))
                           (let ((x (- (vx2 posición) (the single-float (sprite-x s))))
                                 (y (- (vy2 posición) (the single-float (sprite-y s)))))
                             (declare (type single-float x y))
                             (+ (* x x) (* y y))))))

(defun sprites-dibuja (pixels x-inicial ancho-franja ancho alto pos-x pos-y
                       plcam-x plcam-y dir-x dir-y sprites zbuffer texturas)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type (simple-array (unsigned-byte 32) *) pixels)
           (type (simple-array sprite) sprites)
           (type (simple-array single-float) zbuffer)
           (type (simple-array (simple-array (unsigned-byte 32))) texturas)
           (type single-float x-inicial ancho-franja ancho alto
                 pos-x pos-y plcam-x plcam-y dir-x dir-y))
  (let ((inv-det (/ (- (* plcam-x dir-y) (* dir-x plcam-y))))
        (alto/2 (/ alto 2.0)))
    (declare (type single-float inv-det alto/2))
    (labels ((dibuja (trans-x trans-y angulo sprite-texturas sprite-fronteras)
               (declare (type (simple-array fixnum) sprite-texturas)
                        (type (simple-array (simple-array fixnum (4))) sprite-fronteras))
               (let* ((sprite-screen-x (* (/ ancho 2.0) (1+ (divseg trans-x trans-y))))
                      (sprite-ancho (abs (divseg alto trans-y)))
                      (sprite-ancho/2 (/ sprite-ancho 2.0))
                      (y-ini (- alto/2 sprite-ancho/2))
                      (y-fin (+ alto/2 sprite-ancho/2))
                      (prop-ancho (divseg *tex-ancho* sprite-ancho)))
                 (declare (type single-float sprite-screen-x sprite-ancho
                                sprite-ancho/2 y-ini y-fin prop-ancho))
                 (loop for x single-float from x-inicial below (+ x-inicial ancho-franja)
                    for x-fix fixnum = (truncate x)
                    if (< trans-y (aref zbuffer x-fix))
                    do (let ((frontera (aref sprite-fronteras
                                             (if (> (length sprite-fronteras) 1)
                                                 (cond
                                                   ((<= (abs angulo) 0.1) 0)
                                                   ((and (> angulo 0.1) (<= angulo 1.17)) 1)
                                                   ((and (> angulo 1.17) (<= angulo 1.96)) 2)
                                                   (t 3))
                                                 0)))
                             (tex-x (the fixnum (truncate (* (- (+ x sprite-ancho/2) sprite-screen-x)
                                                             prop-ancho)))))
                         (declare (type fixnum tex-x)
                                  (type (simple-array fixnum (4)) frontera))
                         (when (and (>= tex-x (the fixnum (aref frontera 0)))
                                    (<= tex-x (the fixnum (aref frontera 2))))
                           (loop for y single-float from (if (minusp y-ini) 0.0 y-ini) below (if (>= y-fin alto) (1- alto) y-fin)
                              for tex-y fixnum = (truncate (divseg (* (- y y-ini) *tex-alto-fix*) sprite-ancho))
                              for color = (aref (aref texturas (aref sprite-texturas (if (> (length sprite-texturas) 1)
                                                                                         (cond
                                                                                           ((<= (abs angulo) 0.1) 0)
                                                                                           ((and (> angulo 0.1) (<= angulo 1.17)) 1)
                                                                                           ((and (> angulo 1.17) (<= angulo 1.96)) 2)
                                                                                           (t 3))
                                                                                         0)))
                                                tex-y tex-x)
                              if (and (>= tex-y (the fixnum (aref frontera 1)))
                                      (<= tex-y (the fixnum (aref frontera 3)))
                                      (/= 0 color))
                              do (let ((y-fix (truncate y)))
                                   (declare (type fixnum y-fix))
                                   (setf (aref pixels y-fix x-fix)
                                         (case color
                                           (#xFFE8AB (let ((a (aref pixels y-fix x-fix)))
                                                       (ash (- (+ a color) (logand (logxor a color)
                                                                                   #x010101))
                                                            -1)))
                                           (#xFFE8AC (logand (aref pixels y-fix x-fix) color))
                                           (t color)))))))))))
      (declare (inline dibuja))
      (loop for s across sprites
         for s-x single-float = (- (the single-float (sprite-x s)) pos-x)
         and s-y single-float = (- (the single-float (sprite-y s)) pos-y)
         for trans-x single-float = (* inv-det (- (* dir-y s-x) (* dir-x s-y)))
         and trans-y single-float = (* inv-det (- (* plcam-x s-y) (* plcam-y s-x)))
         and Θ single-float = (atan s-y s-x)
         if (plusp trans-y)
         do (log:info "Θ: ~S ~S" Θ (sprite-nombre (sprite-maestro s)))
           (dibuja trans-x trans-y Θ
                   (sprite-texturas (sprite-maestro s))
                   (sprite-fronteras (sprite-maestro s)))))))

;;(ash (+ (logand a #xfEfEfE) (logand color #xfEfEfE)) -1)

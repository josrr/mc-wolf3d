(in-package #:escenario)

(declaim (type single-float *ancho* *alto* *tex-ancho* *tex-alto*)
         (type fixnum *alto/2* *alto-fix* *tex-ancho-fix* *tex-alto-fix*))
(defparameter *ancho* 1100.0)
(defparameter *alto* 660.0)
(defparameter *tex-ancho* 128.0)
(defparameter *tex-alto* 128.0)
(defparameter *tex-ancho-fix* (truncate *tex-ancho*))
(defparameter *tex-alto-fix* (truncate *tex-alto*))
(defparameter *alto-fix* (truncate *alto*))
(defparameter *alto/2* (truncate *alto* 2))
(defparameter *colores*
  (let ((colores `(,+yellow+ ,+skyblue+ ,+turquoise+ ,+slateblue1+ ,+khaki3+ ,+gold+ ,+gray44+ ,+sea-green+ ,+purple+)))
    (list :horizontal (make-array 9 :element-type '(unsigned-byte 32)
                                  :initial-contents (mapcar #'color-a-entero colores))
          :vertical (make-array 9 :element-type '(unsigned-byte 32)
                                :initial-contents
                                (mapcar #'color-a-entero
                                        `(,@(loop for c in colores collect
                                                 (apply #'make-rgb-color
                                                        (mapcar (lambda (v)
                                                                  (declare (type single-float v))
                                                                  (/ v 2.0))
                                                                (multiple-value-list (color-rgb c)))))))))))

(defun aproxima-angulo (angulo)
  (declare (optimize (speed 3) (safety 0))
           (type single-float angulo))
  (let ((fraccion (coerce (/ pi *tex-ancho*) 'single-float)))
    (declare (type single-float fraccion))
    (* (the fixnum (truncate angulo fraccion)) fraccion)))

(defconstant +valmax+ 10000000.0)
(declaim (inline divseg))
(defun divseg (x y)
  (declare (optimize (speed 3) (safety 1))
           (type single-float x y))
  (if (zerop y)
      10000.0
      (/ x y)))

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

(defun crea-escenario (mapa sprites ruta-sonidos &optional (ruta-texturas #P"./pics/")
                                                   (sprites-fronteras *sprites-fronteras*)
                                                   (sprites-eventos *sprites-eventos*))
  (let ((obj (make-instance 'escenario
                            :texturas (carga-texturas ruta-texturas)
                            :mapa mapa
                            :sonidos (carga-sonidos ruta-sonidos))))
    (setf (sprites obj) (carga-sprites sprites sprites-fronteras sprites-eventos))
    obj))

(defgeneric rota (escenario &optional dir))
(defmethod rota (escenario &optional (dir 1))
  (declare (optimize (speed 3) (safety 0))
           (type fixnum dir))
  (with-slots (dirección plano-camara vel-rot) escenario
    (declare (type single-float vel-rot))
    (let ((mat-rot (mrotation +vz+ (* dir vel-rot))))
      (setf dirección (vxy (m* mat-rot (vxy__ dirección)))
            plano-camara (vxy (m* mat-rot (vxy__ plano-camara)))))))

(defun mueve (escenario &optional (dir 1))
  (declare (optimize (speed 3) (safety 0))
           (type fixnum dir))
  (with-slots (mapa posición dirección vel-mov) escenario
    (declare (type single-float vel-mov)
             (type (simple-array fixnum (24 24)) mapa))
    (let* ((nueva-posicion (v+ posición (v* (* dir vel-mov) dirección)))
           (x-i (truncate (vx2 nueva-posicion)))
           (y-i (truncate (vy2 nueva-posicion))))
      (declare (type fixnum x-i y-i))
      (when (and (> x-i 0) (< x-i (array-dimension mapa 1))
                 (> y-i 0) (< y-i (array-dimension mapa 0))
                 (zerop (aref mapa x-i y-i)))
        (setf posición nueva-posicion)))))

(proclaim '(ftype (function ((simple-array (unsigned-byte 32) *)
                             keyword
                             fixnum fixnum fixnum fixnum
                             (simple-array (unsigned-byte 32) *) fixnum)
                   null)
            dibuja-linea-vertical))

(declaim (inline dibuja-linea-vertical borra-imagen aproxima-angulo dibuja-piso))
(defun dibuja-linea-vertical (arreglo modo x y-ini y-fin largo-linea textura x-tex)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type (simple-array (unsigned-byte 32) *) arreglo)
           (type (or null (simple-array (unsigned-byte 32) *)) textura)
           (type fixnum largo-linea x y-ini y-fin))
  (loop for y fixnum from y-ini below y-fin
     for valor of-type (unsigned-byte 32) = (aref textura
                                                  (the fixnum
                                                       (truncate (* *tex-alto* (+ y (* 0.5 (- largo-linea *alto*))))
                                                                 largo-linea))
                                                  x-tex)
     do (setf (aref arreglo y x) (if (eq :vertical modo)
                                     (logand (ash valor -1) #x7F7F7F)
                                     valor))))
;;(aref arreglo y (if (oddp x) (1- x) (1+ x))) valor

(declaim (type (simple-array single-float *) *distancias*))
(defparameter *distancias* (make-array *alto/2*
                                       :element-type 'single-float
                                       :initial-contents (loop for y from *alto/2* to (1- *alto*)
                                                            collect (divseg *alto* (- (* 2.0 y) *alto*)))))

(defun dibuja-piso (arreglo pos-x pos-y x y-fin dist-pared-perp lado pared-x rayo-dir-x rayo-dir-y mapa-x mapa-y textura-piso textura-techo &optional textura-piso-2)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type fixnum x y-fin)
           (type single-float pared-x rayo-dir-x rayo-dir-y dist-pared-perp pos-x pos-y mapa-x mapa-y)
           (type (simple-array (unsigned-byte 32) *) arreglo textura-piso textura-techo)
           (type (or null (simple-array (unsigned-byte 32) *)) textura-piso-2))
  (multiple-value-bind (piso-pared-x piso-pared-y) (if (eq :horizontal lado)
                                                       (values (if (plusp rayo-dir-x)
                                                                   mapa-x
                                                                   (1+ mapa-x))
                                                               (+ mapa-y pared-x))
                                                       (values (+ mapa-x pared-x)
                                                               (if (plusp rayo-dir-y)
                                                                   mapa-y
                                                                   (1+ mapa-y))))
    (declare (type single-float piso-pared-x piso-pared-y))
    (loop for y fixnum from (1+ (if (minusp y-fin) *alto-fix* y-fin)) below *alto-fix*
       for peso single-float = (divseg (aref *distancias* (- y *alto/2*)) dist-pared-perp)
       for c-x single-float = (+ (* peso (- piso-pared-x pos-x)) pos-x)
       and c-y single-float = (+ (* peso (- piso-pared-y pos-y)) pos-y)
       for c-x-fix fixnum = (the fixnum (truncate c-x))
       and c-y-fix fixnum = (the fixnum (truncate c-y))
       for tex-x fixnum = (mod (the fixnum (truncate (* *tex-ancho* c-x))) *tex-ancho-fix*)
       and tex-y fixnum = (mod (the fixnum (truncate (* *tex-ancho* c-y))) *tex-alto-fix*)
       do (setf (aref arreglo y x)
                (aref (if textura-piso-2
                          (if (zerop (mod (+ c-x-fix c-y-fix) 2)) textura-piso-2 textura-piso)
                          textura-techo)
                      tex-y tex-x)
                (aref arreglo (- *alto-fix* y) x)
                (aref textura-techo tex-y tex-x)))))

(defun borra-imagen (arreglo)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type (simple-array (unsigned-byte 32) (768 1280)) arreglo))
  (loop for y from 0 below (array-dimension arreglo 0)
     do (loop for x from 0 below (array-dimension arreglo 1)
           do (setf (aref arreglo y x)
                    (if (< y *alto/2*) #xFFaaaaaa #xFFdd6666)))))

#+sbcl
(declaim (type (simple-array (or null function)) *tareas*)
         (type (simple-array sb-thread:mutex) *bloqueos*)
         (type (simple-array sb-thread:waitqueue) *conds*))

(defparameter *num-hilos* 4)
(defparameter *tareas* (make-array *num-hilos* :initial-element nil))
(defparameter *conds* (make-array *num-hilos* :initial-contents (loop repeat *num-hilos* collect (bt:make-condition-variable))))
(defparameter *bloqueos* (make-array *num-hilos* :initial-contents (loop repeat *num-hilos* collect (bt:make-lock))))
(defparameter *hilos* nil)
(defparameter *bloqueo-fin* (bt:make-lock "fin"))
(defparameter *cond-fin* (bt:make-condition-variable))

(defun inicia-hilos ()
  (unless *hilos*
    (setf *hilos*
          (loop for i from 0 below *num-hilos* collect
               (bt:make-thread (funcall (lambda (i)
                                          (lambda ()
                                            (loop with cv = (aref *conds* i)
                                               and bl = (aref *bloqueos* i)
                                               while t
                                               do (bt:thread-yield)
                                                 (bt:with-lock-held (bl)
                                                   (loop until (aref *tareas* i)
                                                      do (bt:condition-wait cv bl)))
                                                 (funcall (aref *tareas* i))
                                                 (setf (aref *tareas* i) nil)
                                                 (bt:with-lock-held (*bloqueo-fin*)
                                                   (when (every #'null *tareas*)
                                                     (bt:condition-notify *cond-fin*))))))
                                        i))))))

(defun termina-hilos ()
  (map 'nil #'bt:destroy-thread *hilos*)
  (setf *hilos* nil))

(declaim (inline genera-escenario))
(defun genera-escenario (x-ini x-fin pixels dirección posición plano-camara mapa texturas zbuffer)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type single-float x-ini x-fin)
           (type (simple-array (unsigned-byte 32) *) pixels)
           (type (simple-array fixnum (24 24)) mapa)
           (type (simple-array (simple-array (unsigned-byte 32))) texturas)
           (type (simple-array single-float) zbuffer))
  (loop with paso-x fixnum = 0 and paso-y fixnum = 0
     and lado-dist = (vec2 0 0) and lado keyword = :horizontal
     for x single-float from x-ini below x-fin by 1.0
     and mapa-x fixnum = (the fixnum (truncate (vx2 posición)))
     and mapa-y fixnum = (the fixnum (truncate (vy2 posición)))
     for camara-x single-float = (1- (/ (* 2.0 x) *ancho*))
     for rayo-dir = (v+ dirección (v* camara-x plano-camara))
     for Δ-dist = (vec2 (abs (divseg 1.0 (vx2 rayo-dir)))
                        (abs (divseg 1.0 (vy2 rayo-dir))))
     if (minusp (vx2 rayo-dir)) do
       (setf paso-x -1
             (vx2 lado-dist) (* (- (vx2 posición) mapa-x) (vx2 Δ-dist)))
     else do
       (setf paso-x 1
             (vx2 lado-dist) (* (- (the fixnum (1+ mapa-x)) (vx2 posición)) (vx2 Δ-dist)))
     end if (minusp (vy2 rayo-dir)) do
       (setf paso-y -1
             (vy2 lado-dist) (* (- (vy2 posición) mapa-y) (vy2 Δ-dist)))
     else do
       (setf paso-y 1
             (vy2 lado-dist) (* (- (the fixnum (1+ mapa-y)) (vy2 posición)) (vy2 Δ-dist)))
     end
     do (loop if (< (vx2 lado-dist) (vy2 lado-dist))
           do (incf (vx2 lado-dist) (vx2 Δ-dist))
             (incf mapa-x paso-x)
             (setf lado :horizontal)
           else do
             (incf (vy2 lado-dist) (vy2 Δ-dist))
             (incf mapa-y paso-y)
             (setf lado :vertical) end
           while (zerop (aref mapa mapa-x mapa-y)))
       (let* ((dist-pared-perp (if (eq :horizontal lado)
                                   (divseg (+ mapa-x (- (vx2 posición)) (/ (- 1 paso-x) 2.0))
                                           (vx2 rayo-dir))
                                   (divseg (+ mapa-y (- (vy2 posición)) (/ (- 1 paso-y) 2.0))
                                           (vy2 rayo-dir))))
              (pared-x (if (eq :horizontal lado)
                           (+ (vy2 posición) (* dist-pared-perp (vy2 rayo-dir)))
                           (+ (vx2 posición) (* dist-pared-perp (vx2 rayo-dir)))))
              (tex-x (truncate (decf pared-x (the fixnum (truncate pared-x)))
                               (/ *tex-ancho*)))
              (largo-linea (truncate (divseg *alto* dist-pared-perp)))
              (y-fin (let ((tmp (/ (+ *alto* largo-linea) 2.0)))
                       (declare (type single-float tmp))
                       (if (> tmp *alto*)
                           *alto* tmp)))
              (x-fix (truncate x))
              (mapa-x-f (coerce mapa-x 'single-float))
              (mapa-y-f (coerce mapa-y 'single-float)))
         (declare (type fixnum tex-x largo-linea x-fix)
                  (type single-float dist-pared-perp pared-x y-fin mapa-x-f mapa-y-f))
         (when (and (eq :horizontal lado) (plusp (vx2 rayo-dir)))
           (setf tex-x (- *tex-ancho-fix* tex-x 1)))
         (when (and (eq :vertical lado) (minusp (vy2 rayo-dir)))
           (setf tex-x (- *tex-ancho-fix* tex-x 1)))
         (dibuja-linea-vertical pixels lado x-fix
                                (let ((tmp (truncate (- *alto* largo-linea) 2)))
                                  (declare (type fixnum tmp))
                                  (if (minusp tmp) 0 tmp))
                                (the fixnum (truncate y-fin))
                                largo-linea
                                (aref texturas
                                      (1- (aref mapa mapa-x mapa-y)))
                                tex-x)
         (setf (aref zbuffer x-fix) dist-pared-perp)
         (dibuja-piso pixels
                      (vx2 posición) (vy2 posición)
                      x-fix (the fixnum (truncate y-fin))
                      dist-pared-perp lado pared-x
                      (vx2 rayo-dir) (vy2 rayo-dir)
                      mapa-x-f mapa-y-f
                      (aref texturas 14)
                      (aref texturas 14)
                      (aref texturas 14)))))

(defgeneric regenera (escenario))

(defmethod regenera ((escenario escenario))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (with-slots (imagen ancho alto dirección posición plano-camara mapa sprites texturas zbuffer) escenario
    (declare (type single-float ancho))
    (sprites-ordena posición sprites)
    (loop with paso single-float = (/ ancho (the fixnum *num-hilos*))
       and pixels = (image-pixels imagen)
       for x single-float from 0.0 below ancho by paso
       and i fixnum from 0
       do (bt:with-lock-held ((aref *bloqueos* i))
            (setf (aref *tareas* i)
                  (let ((x x))
                    (lambda ()
                      (genera-escenario x (+ x paso) pixels dirección
                                        posición plano-camara
                                        mapa texturas zbuffer)
                      (sprites-dibuja pixels
                                      x paso
                                      ancho alto
                                      (vx2 posición)     (vy2 posición)
                                      (vx2 plano-camara) (vy2 plano-camara)
                                      (vx2 dirección)    (vy2 dirección)
                                      sprites zbuffer texturas))))
            (bt:condition-notify (aref *conds* i))))
    (bt:with-lock-held (*bloqueo-fin*)
      (loop until (every #'null *tareas*)
         do (bt:condition-wait *cond-fin* *bloqueo-fin*)))))

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

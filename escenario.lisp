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
   (mapa :initarg :mapa :accessor mapa :initform nil :type (simple-array fixnum (24 24)))
   (posición :initarg :posicion :accessor posición :initform (vec2 2.0 2.0))
   (dirección :initarg :dirección :accessor dirección :initform (vec2 -1 0))
   (plano-camara :initarg :plano-camara :accessor plano-camara :initform (vec2 0 0.66))
   (rayos-pares :accessor rayos-pares :initform t :type boolean)
   (vel-mov :accessor vel-mov :initform 0.25 :type single-float)
   (vel-rot :accessor vel-rot :initform (aproxima-angulo (coerce (* 4.0 (/ pi 128.0)) 'single-float)) :type single-float)
   (zbuffer :accessor zbuffer :initform (make-array (truncate *ancho*) :element-type 'single-float :initial-element 0.0))
   (imagen :accessor imagen
           :initform (make-image :rgb (floor *ancho*) *alto-fix*
                                 :two-dim-array))
   (texturas :initarg :texturas
             :accessor texturas
             :initform nil
             :type (simple-array (simple-array (unsigned-byte 32) (*tex-ancho-fix* *tex-alto-fix*)) *))))

(defgeneric rota (escenario &optional dir))
(defmethod rota (escenario &optional (dir 1))
  (declare (optimize (speed 3) (safety 0))
           (type fixnum dir))
  (with-slots (dirección plano-camara vel-rot) escenario
    (declare (type single-float vel-rot))
    (let ((mat-rot (mrotation +vz+ (* dir vel-rot))))
      (setf dirección (vxy (m* mat-rot (vxy__ dirección)))
            plano-camara (vxy (m* mat-rot (vxy__ plano-camara)))))))

(defun mueve (frame &optional (dir 1))
  (declare (optimize (speed 3) (safety 0))
           (type fixnum dir))
  (with-slots (mapa posición dirección vel-mov) frame
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
  (declare (optimize (speed 3) (safety 0))
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
                                     (logand (ash valor -1) 8355711)
                                     valor))))
;;(aref arreglo y (if (oddp x) (1- x) (1+ x))) valor

(declaim (type (simple-array single-float *) *distancias*))
(defparameter *distancias* (make-array *alto/2*
                                       :element-type 'single-float
                                       :initial-contents (loop for y from *alto/2* to (1- *alto*)
                                                            collect (divseg *alto* (- (* 2.0 y) *alto*)))))

(defun dibuja-piso (arreglo pos-x pos-y x y-fin dist-pared-perp lado pared-x rayo-dir-x rayo-dir-y mapa-x mapa-y textura-piso textura-techo &optional textura-piso-2)
  (declare (optimize (speed 3) (safety 0))
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
  (declare (optimize (speed 3) (safety 0))
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
(defun genera-escenario (x-ini x-fin imagen dirección posición plano-camara mapa texturas zbuffer)
  (declare (optimize (speed 3) (safety 0))
           (type single-float x-ini x-fin)
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
                       (if (>= tmp *alto*)
                           (1- *alto*) tmp)))
              (x-fix (truncate x))
              (mapa-x-f (coerce mapa-x 'single-float))
              (mapa-y-f (coerce mapa-y 'single-float)))
         (declare (type fixnum tex-x largo-linea x-fix)
                  (type single-float dist-pared-perp pared-x y-fin mapa-x-f mapa-y-f))
         (when (and (eq :horizontal lado) (plusp (vx2 rayo-dir)))
           (setf tex-x (- *tex-ancho-fix* tex-x 1)))
         (when (and (eq :vertical lado) (minusp (vy2 rayo-dir)))
           (setf tex-x (- *tex-ancho-fix* tex-x 1)))
         (dibuja-linea-vertical (image-pixels imagen)
                                lado
                                x-fix
                                (let ((tmp (truncate (- *alto* largo-linea) 2)))
                                  (declare (type fixnum tmp))
                                  (if (minusp tmp) 0 tmp))
                                (the fixnum (truncate y-fin))
                                largo-linea
                                (aref texturas
                                      (1- (aref mapa mapa-x mapa-y)))
                                tex-x)
         (setf (aref zbuffer x-fix) dist-pared-perp)
         (dibuja-piso (image-pixels imagen)
                      (vx2 posición) (vy2 posición)
                      x-fix (the fixnum (truncate y-fin))
                      dist-pared-perp lado pared-x
                      (vx2 rayo-dir) (vy2 rayo-dir)
                      mapa-x-f mapa-y-f
                      (aref texturas 14)
                      (aref texturas 14)
                      (aref texturas 14)))))

(defgeneric regenera (escenario sprites))

(declaim (inline sprites-ordena))
(defun sprites-ordena (posición sprites)
  (declare (optimize (speed 3))
           (type (simple-array sprite) sprites))
  (sort sprites #'> :key (lambda (s)
                           (declare (optimize (speed 3)))
                           (let ((x (- (vx2 posición) (the single-float (sprite-x s))))
                                 (y (- (vy2 posición) (the single-float (sprite-y s)))))
                             (declare (type single-float x y))
                             (+ (* x x) (* y y))))))

(defun sprites-dibuja (pixels x-inicial ancho-franja ancho alto pos-x pos-y plcam-x plcam-y dir-x dir-y sprites zbuffer texturas)
  (declare (optimize (speed 3) (safety 0) (debug 1))
           (type (simple-array (unsigned-byte 32) *) pixels)
           (type (simple-array sprite) sprites)
           (type (simple-array single-float) zbuffer)
           (type (simple-array (simple-array (unsigned-byte 32))) texturas)
           (type single-float x-inicial ancho-franja ancho alto pos-x pos-y plcam-x plcam-y dir-x dir-y))
  (loop with inv-det single-float = (/ (- (* plcam-x dir-y) (* dir-x plcam-y)))
     for s across sprites
     for s-x single-float = (- (the single-float (sprite-x s)) pos-x)
     and s-y single-float = (- (the single-float (sprite-y s)) pos-y)
     and frontera of-type (simple-array fixnum) = (frontera s)
     for trans-x single-float = (* inv-det (- (* dir-y s-x) (* dir-x s-y)))
     and trans-y single-float = (* inv-det (- (* plcam-x s-y) (* plcam-y s-x)))
     for sprite-screen-x fixnum = (the fixnum (truncate (* (/ ancho 2.0) (1+ (divseg trans-x trans-y)))))
     and sprite-alto single-float  = (abs (divseg alto trans-y))
     for sprite-ancho single-float = sprite-alto
     for y-ini single-float = (/ (- alto sprite-alto) 2)
     and y-fin single-float = (/ (+ alto sprite-alto) 2)
     and x-ini single-float = (- sprite-screen-x (/ sprite-ancho 2))
     and x-fin single-float = (+ sprite-screen-x (/ sprite-ancho 2))
     do (loop for x single-float from (if (< x-ini x-inicial) x-inicial x-ini) below (let ((x-max (+ x-inicial ancho-franja)))
                                                                                       (if (> x-fin x-max) x-max x-fin))
           for x-fix fixnum = (truncate x)
           for tex-x fixnum = (truncate (* (- x (+ (/ (- sprite-ancho) 2) sprite-screen-x))
                                           (divseg *tex-ancho* sprite-ancho)))
           if (and (>= tex-x (aref frontera 0)) (<= tex-x (aref frontera 2))
                   (plusp trans-y)
                   (< trans-y (aref zbuffer x-fix))
                   (plusp x) (< x ancho))
           do (loop for y single-float from (if (minusp y-ini) 0 y-ini) below (if (>= y-fin alto) (1- alto) y-fin)
                 for y-fix fixnum = (truncate y)
                 for d single-float = (+ y (/ (- sprite-alto alto) 2))
                 for tex-y fixnum = (truncate (divseg (* d *tex-alto-fix*) sprite-alto))
                 for color = (aref (aref texturas (sprite-textura s)) tex-y tex-x)
                 if (and (>= tex-y (aref frontera 1)) (<= tex-y (aref frontera 3))
                         (/= 0 color))
                 do (setf (aref pixels y-fix x-fix)
                          (case color
                            (#xFFE8AB
                             (let ((a (aref pixels y-fix x-fix)))
                               (ash (- (+ a color) (logand (logxor a color) #x010101)) -1)))
                            (#xFFE8AC (logand (aref pixels y-fix x-fix) color))
                            (t color)))))))
;;(ash (+ (logand a #xfEfEfE) (logand color #xfEfEfE)) -1)

(defmethod regenera ((escenario escenario) (sprites array))
  (declare (optimize (speed 3) (safety 0)))
  ;;(declare (optimize (debug 3)))
  (with-slots (imagen ancho alto dirección posición plano-camara mapa texturas zbuffer) escenario
    (declare (type single-float ancho))
    (sprites-ordena posición sprites)
    (loop with paso single-float = (/ ancho (the fixnum *num-hilos*))
       for x single-float from 0.0 below ancho by paso
       and i fixnum from 0
       do (bt:with-lock-held ((aref *bloqueos* i))
            (setf (aref *tareas* i)
                  (let ((x x))
                    (lambda ()
                      (genera-escenario x (+ x paso) imagen dirección
                                        posición plano-camara
                                        mapa texturas zbuffer)
                      (sprites-dibuja (image-pixels imagen)
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

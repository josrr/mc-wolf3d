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

(declaim (inline aproxima-angulo))
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
   (manos :accessor manos :initform nil :initarg :manos)
   ;;(pane :accessor pane :initform nil :initarg :pane)
   (mapa :initarg :mapa :accessor mapa :initform nil :type (simple-array fixnum (24 24)))
   (sprites-maestros :initform (make-hash-table) :initarg :sprites-maestros :accessor sprites-maestros :type hash-table)
   (sprites :initform nil :initarg :sprites :accessor sprites :type (simple-array sprite))
   (personajes :initform nil :initarg :personajes :accessor personajes :type (simple-array personaje))
   (sprite-proximo :initform nil :accessor sprite-proximo :type cons)
   (texturas :initarg :texturas :accessor texturas :initform nil :type (simple-array (simple-array (unsigned-byte 32)
                                                                                                   (*tex-ancho-fix* *tex-alto-fix*))
                                                                                     *))
   (sonidos :initform nil :initarg :sonidos :accessor sonidos :type (simple-array t))))

(defun crea-escenario (mapa sprites-maestros sprites
                       ruta-sonidos &optional (ruta-texturas #P"./pics/"))
  (let* ((tabla-sprites (carga-sprites-maestros sprites-maestros))
         (arreglo-sprites (crea-sprites tabla-sprites sprites))
         (personajes (remove-if (complement
                                 (lambda (s)
                                   (member (sprite-nombre (sprite-maestro s))
                                           *personajes* :key #'car)))
                                arreglo-sprites)))
    (make-instance 'escenario
                   :texturas (carga-texturas ruta-texturas)
                   :mapa mapa
                   :sonidos (carga-sonidos ruta-sonidos)
                   :sprites-maestros tabla-sprites
                   :sprites arreglo-sprites
                   :personajes (make-array (length personajes) :element-type 'personaje
                                           :initial-contents (map 'list
                                                                  (lambda (s)
                                                                    (let ((comp (assoc (sprite-nombre (sprite-maestro s))
                                                                                       *personajes*)))
                                                                      (crea-personaje s (when comp (cdr comp)))))
                                                                  personajes))
                   :manos (carga-archivo #P"./pics/manos.png"))))

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

(declaim (inline dibuja-linea-vertical borra-imagen dibuja-piso))
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
                      (aref texturas 3)
                      (aref texturas 3)
                      (aref texturas 3)))))

(defun dibuja-manos (manos pixels x y)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type fixnum x y )
           (type (simple-array (unsigned-byte 32) *) manos pixels))
  (loop for j fixnum from 0 below (array-dimension manos 0) do
       (loop for i fixnum from 0 below (array-dimension manos 1)
          for color = (aref manos j i)
          if (> color 0) do
            (setf (aref pixels (+ y j) (+ x i))
                  color))))

(defgeneric regenera (escenario))

(defmethod regenera ((escenario escenario))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (with-slots (imagen ancho alto dirección posición pane mapa
                      manos plano-camara sprites texturas zbuffer)
      escenario
    (declare (type single-float ancho))
    (sprites-ordena posición sprites)
    (let ((pixels (image-pixels imagen)))
      (loop with paso single-float = (/ ancho (the fixnum *num-hilos*))
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
           do (bt:condition-wait *cond-fin* *bloqueo-fin*)))
      (dibuja-manos manos pixels
                    (truncate (- ancho (array-dimension manos 1)) 2)
                    (- (truncate alto)
                       (array-dimension manos 0))))))

(defclass accion ()
  ((pars :initarg :pars :accessor accion-pars)
   (realizada :initform nil :accessor accion-realizada)))
(defclass accion-sonido (accion)
  ())
(defclass accion-dialogo (accion)
  ())
(defclass evento ()
  ((accion :initarg :accion :accessor evento-accion :type accion)))
(defclass evento-contacto (evento) ())

(defgeneric accion-realiza (escenario accion sprite lienzo mezclador))

(defgeneric evento-reliza (escenario evento sprite lienzo mezclador))

(defmethod accion-realiza ((escenario escenario) (accion accion-sonido) (sprite sprite) lienzo mezclador)
  (declare (optimize (speed 3)) (ignore lienzo))
  (with-slots (sonidos) escenario
    (declare (type (simple-array mixalot-vorbis:vorbis-streamer) sonidos))
    (let ((snd (aref sonidos (accion-pars accion))))
      (declare (type mixalot-vorbis:vorbis-streamer snd))
      (if (mixalot:streamer-paused-p snd mezclador)
          (progn
            (mixalot:streamer-seek snd mezclador 0)
            (mixalot:streamer-unpause snd mezclador))
          (if (member snd (mixalot:mixer-stream-list mezclador))
              (when (> (the fixnum (mixalot:streamer-position snd mezclador))
                       22050)
                (mixalot:streamer-seek snd mezclador 0))
              (mixalot:mixer-add-streamer mezclador snd))))))

(defparameter *tipo-dialogo* (make-text-style "Edit Undo BRK" "Regular" 24))
(defun dibuja-dialogo (escenario lienzo sprite texto)
  (declare (ignore sprite))
  (with-slots (alto ancho) escenario
    (draw-rectangle* lienzo
                     256 (- (bounding-rectangle-height lienzo) 256)
                     (- (bounding-rectangle-width lienzo) 320)
                     (- (bounding-rectangle-height lienzo) 24)
                     :filled nil
                     :ink +turquoise+)
    (draw-rectangle* lienzo
                     258 (- (bounding-rectangle-height lienzo) 248)
                     (- (bounding-rectangle-width lienzo) 324)
                     (- (bounding-rectangle-height lienzo) 28)
                     :filled t
                     :ink +black+)
    (loop for linea in texto
       for y-ini from (- (bounding-rectangle-height lienzo) 230) by 26 do
         (draw-text* lienzo linea
                     264 y-ini
                     :align-x :left
                     :text-style *tipo-dialogo*
                     :ink +turquoise+))))

(defmethod accion-realiza ((escenario escenario) (accion accion-dialogo) (sprite sprite) lienzo mezclador)
  (declare (optimize (speed 3)) (ignore mezclador))
  (unless (accion-realizada accion)
    (setf (accion-realizada accion) t)
    (dibuja-dialogo escenario lienzo sprite (accion-pars accion))))

(defmethod evento-reliza ((escenario escenario) (evento evento-contacto)
                          (sprite sprite) lienzo mezclador)
  (declare (optimize (speed 3)))
  (with-slots (posición) escenario
    (with-slots (x y) sprite
      (declare (type single-float x y))
      (let ((Δᵤ (abs (the single-float (- (vx2 posición) x))))
            (Δᵥ (abs (the single-float (- (vy2 posición) y)))))
        (declare (type single-float Δᵤ Δᵥ))
        (when (and (<= Δᵤ 0.75) (<= Δᵥ 0.35))
          (accion-realiza escenario (evento-accion evento) sprite lienzo mezclador))))))

(defgeneric escenario-revisa-eventos (escenario lienzo mezclador))

(defmethod escenario-revisa-eventos ((escenario escenario) lienzo mezclador)
  (declare (optimize (speed 3)))
  (with-slots (sprites) escenario
    (declare (type (simple-array sprite) sprites))
    (when sprites
      (loop for sprite across sprites do
           (loop for ev in (sprite-eventos (sprite-maestro sprite)) do
                (evento-reliza escenario ev sprite lienzo mezclador))))))

(defgeneric personaje-realiza-comportamiento (personaje escenario))

(defmethod personaje-realiza-comportamiento ((personaje personaje) (escenario escenario))
  (with-slots (comportamiento) personaje
    (when comportamiento
      (funcall comportamiento escenario personaje))))

(defgeneric escenario-realiza-personajes (escenario))

(defmethod escenario-realiza-personajes ((escenario escenario))
  (declare (optimize (speed 3)))
  (with-slots (personajes) escenario
    (declare (type (simple-array personaje) personajes))
    (when personajes
      (some #'identity
            (loop for personaje across personajes collect
                 (personaje-realiza-comportamiento personaje escenario))))))

;;;; mc-wolf3d.lisp
(in-package #:mc-wolf3d)

(defparameter *frame* nil)
(defparameter *ruta-del-sistema* (asdf:component-pathname (asdf:find-system 'mc-wolf3d)))

(defclass canvas-gadget (climi::never-repaint-background-mixin basic-gadget)
  ((bloqueo-cuadro :accessor bloqueo-cuadro :initform (clim-sys:make-lock "Bloqueo cuadro"))))


(define-application-frame mc-wolf3d ()
  ((escenario :initarg :escenario :accessor escenario :initform (make-instance 'escenario) :type escenario)
   (mezclador :initarg :mezclador :accessor mezclador :initform nil)
   (tiempo :accessor tiempo :initform (local-time:now))
   (tiempo-anterior :accessor tiempo-anterior :initform 0)
   (periodo-cuadros :accessor periodo-cuadros :initform 0.0 :type single-float)
   (modo-mov :accessor modo-mov :initform nil)
   (modo-rot :accessor modo-rot :initform nil)
   (jugando :accessor jugando :initform nil)
   (portada :initform (make-pattern-from-bitmap-file (merge-pathnames #P"fondo.png" *ruta-del-sistema*))))
  (:pane (make-pane 'canvas-gadget
                    :background +black+
                    :min-width 1280
                    :min-height 1024)))

(defparameter *tipo-titulos* (make-text-style :serif :bold 18))
(defparameter *tipo-subtitulos* (make-text-style "Wargames" "Regular" 24))
(defparameter *tipo-normal* (make-text-style "Edit Undo BRK" "Regular" 48))

(defun redibuja-cuadro (frame &optional gadget-arg)
  (with-slots (escenario tiempo tiempo-anterior periodo-cuadros portada) frame
    (declare (type single-float periodo-cuadros))
    (let ((gadget (or gadget-arg (car (frame-current-panes frame)))))
      (labels ((dibuja-portada ()
                 (let* ((ancho-portada (pattern-width portada))
                        (alto-portada (pattern-height portada))
                        (desp (/ (- (bounding-rectangle-width (sheet-region gadget)) ancho-portada) 2.0)))
                   (with-translation (gadget desp desp)
                     (draw-pattern* gadget portada 0 0)
                     (let ((cadena "mc-wolf3d"))
                       (multiple-value-bind (t-ancho t-alto) (text-size gadget cadena :text-style *tipo-normal*)
                         (draw-text* gadget cadena
                                     (- ancho-portada t-ancho 12)
                                     (- alto-portada t-alto 96)
                                     :text-style *tipo-normal*)))
                     (let ((cadena "Por José M. Ronquillo Rivera"))
                       (multiple-value-bind (t-ancho t-alto) (text-size gadget cadena :text-style *tipo-titulos*)
                         (draw-text* gadget cadena
                                     (- ancho-portada t-ancho 12)
                                     (- alto-portada t-alto 32)
                                     :text-style *tipo-titulos*)))
                     (let ((cadena "Arte por Alexis Ruíz Martínez"))
                       (multiple-value-bind (t-ancho t-alto) (text-size gadget cadena :text-style *tipo-titulos*)
                         (draw-text* gadget cadena
                                     (- ancho-portada t-ancho 12)
                                     (- alto-portada t-alto)
                                     :text-style *tipo-titulos*)))))
                 (dibuja-dialogo escenario gadget '("" "  Presiona la tecla 'n' para comenzar el juego;"
                                                    "" "  'q' para salir del juego;"
                                                    "" "  'arriba', 'abajo', 'izquierda' y 'derecha' para moverte.")))
               (dibuja-juego ()
                 (declare (optimize (speed 3)))
                 (regenera escenario)
                 (setf tiempo-anterior tiempo
                       tiempo (local-time:now)
                       periodo-cuadros (coerce (the double-float
                                                    (local-time:timestamp-difference tiempo
                                                                                     tiempo-anterior))
                                               'single-float))
                 (let* ((ancho-gadget (bounding-rectangle-width (sheet-region gadget)))
                        (alto-gadget (bounding-rectangle-height (sheet-region gadget)))
                        ;;(desp (/ (- ancho-gadget *ancho*) 2.0))
                        (cadena (format nil "~5,2F fps" (/ periodo-cuadros))))
                   (declare (type fixnum ancho-gadget alto-gadget))
                   (draw-design gadget (imagen escenario))
                   (draw-rectangle* gadget
                                    0 1003
                                    (the fixnum (+ 40 (the fixnum (text-size gadget cadena :text-style *tipo-normal*)))) 900
                                    :ink +black+)
                   (dibuja-mapa escenario gadget (- ancho-gadget 248) (- alto-gadget 256) 248)
                   (draw-text* gadget cadena 10 990 :text-style *tipo-normal* :ink +turquoise+))))
        (if (jugando frame)
            (dibuja-juego)
            (dibuja-portada))))))

(defmethod handle-repaint ((gadget canvas-gadget) region)
  (declare (ignore region))
  (redibuja-cuadro *frame* gadget))

(defparameter *tipografia* nil)
(defun carga-tipografia ()
  (mcclim-truetype::register-all-ttf-fonts (find-port) *ruta-del-sistema*)
  (setf *tipografia* t))

(defun wolf3d-main (&optional (mapa (car *mapas*))
                      (sprites (car *sprites*)))
  (escenario:inicia-hilos)
  (unless *tipografia* (carga-tipografia))
  (setf *frame* (make-application-frame 'mc-wolf3d)
        (escenario *frame*) (crea-escenario (or mapa (car *mapas*))
                                            *sprites-maestros*
                                            (or sprites (car *sprites*))
                                            (merge-pathnames #P"sonidos/" *ruta-del-sistema*)
                                            (merge-pathnames #P"pics/" *ruta-del-sistema*)))
  (bt:make-thread (lambda ()
                    (run-frame-top-level *frame*)
                    (setf *frame* nil)
                    (escenario:termina-hilos))
                  :name "mc-wolf3d-main"))

(define-mc-wolf3d-command (com-restablece-todo :name "restablece") ()
  (with-slots (posición dirección plano-camara) (escenario *frame*)
    (setf posición (vec 1.0 1.0)
          dirección (vec -1.0 0.0)
          plano-camara (vec 0 0.66)))
  (redibuja-cuadro *frame*))

(define-mc-wolf3d-command (com-terminar-juego :name "terminar-juego") ()
  (when (jugando *frame*)
    (clim-sys:destroy-process (jugando *frame*))
    (setf (jugando *frame*) nil)
    (mixalot:mixer-remove-all-streamers (mezclador *frame*))
    (mixalot:destroy-mixer (mezclador *frame*))
    (setf (mezclador *frame*) nil)
    (redibuja-cuadro *frame*)))

(define-mc-wolf3d-command (com-salir :name "salir") ()
  (let ((lienzo (car (frame-current-panes *frame*))))
    (clim-sys:with-lock-held ((bloqueo-cuadro lienzo))
      (when (jugando *frame*)
        (clim-sys:destroy-process (jugando *frame*))
        (setf (jugando *frame*) nil))
      (frame-exit *application-frame*))))

(defun juega (frame)
  (clim-sys:make-process
   (lambda ()
     (declare (optimize (speed 3) (safety 0) (debug 0)))
     (with-slots (modo-rot modo-mov escenario mezclador) frame
       (loop while t
             with sonidos of-type (simple-array t) = (sonidos escenario)
             and lienzo = (car (frame-current-panes frame))
             if modo-mov do (mueve escenario (if (eq :adelante modo-mov) 1 -1))
               if modo-rot do (rota escenario (if (eq :derecha modo-rot) 1 -1))
                 do (when sonidos
                      (loop for snd across (subseq sonidos 1)
                            if (and (not (mixalot:streamer-paused-p snd mezclador))
                                    (member snd (mixalot:mixer-stream-list mezclador))
                                    (< (the fixnum (- (the fixnum (mixalot:streamer-length snd mezclador))
                                                      (the fixnum (mixalot:streamer-position snd mezclador))))
                                       5512))
                              do (mixalot:streamer-pause snd mezclador)
                                 (mixalot:streamer-seek snd mezclador 0))
                      (when (< (- (the fixnum (mixalot:streamer-length (aref sonidos 0) mezclador))
                                  (the fixnum (mixalot:streamer-position (aref sonidos 0) mezclador)))
                               5512)
                        (mixalot:streamer-seek (aref sonidos 0) mezclador 0)))
                    (escenario-revisa-eventos escenario lienzo mezclador)
             if (or (escenario-realiza-personajes escenario) modo-rot modo-mov) do
               (clim-sys:with-lock-held ((bloqueo-cuadro lienzo))
                 (redibuja-cuadro frame lienzo))
             end do (clim-sys:process-yield))))))

(define-mc-wolf3d-command (com-nuevo :name "Nuevo juego")
    ()
  (when *frame*
    (with-slots (jugando mezclador escenario) *frame*
      (unless jugando
        (mixalot:main-thread-init)
        (setf mezclador (mixalot:create-mixer))
        (when (sonidos escenario)
          (mixalot:mixer-add-streamer mezclador (aref (sonidos escenario) 0)))
        (setf jugando (juega *frame*))))))

(defmethod handle-event ((gadget canvas-gadget) (evento key-press-event))
  (when *frame*
    (with-slots (escenario) *frame*
      (case (keyboard-event-key-name evento)
        ((:Q :|q|) (execute-frame-command *frame* `(com-salir)))
        ((:X :|x|) (execute-frame-command *frame* `(com-terminar-juego)))
        ((:N :|n|) (execute-frame-command *frame* `(com-nuevo)))
        ((:R :|r|) (execute-frame-command *frame* `(com-restablece-todo)))
        (:up (setf (slot-value *frame* 'modo-mov) :adelante))
        (:down (setf (slot-value *frame* 'modo-mov) :atras))
        (:right (setf (slot-value *frame* 'modo-rot) :izquierda))
        (:left (setf (slot-value *frame* 'modo-rot) :derecha))))))

(defmethod handle-event ((gadget canvas-gadget) (evento key-release-event))
  (when *frame*
    (case (keyboard-event-key-name evento)
      ((:up :down) (setf (slot-value *frame* 'modo-mov) nil))
      ((:right :left) (setf (slot-value *frame* 'modo-rot) nil)))))


;;;; mc-wolf3d.lisp
(in-package #:mc-wolf3d)

(defparameter *frame* nil)

(defclass canvas-pane (clim-stream-pane) ())
(defgeneric display (frame pane))

(define-application-frame mc-wolf3d ()
  ((escenario :initarg :escenario :accessor escenario :initform (make-instance 'escenario) :type escenario)
   (sprites :accessor sprites :initform (escenario:carga-sprites))
   (tiempo :accessor tiempo :initform (local-time:now))
   (tiempo-anterior :accessor tiempo-anterior :initform 0)
   (periodo-cuadros :accessor periodo-cuadros :initform 0.0 :type single-float)
   (modo-mov :accessor modo-mov :initform nil)
   (modo-rot :accessor modo-rot :initform nil)
   (jugando :accessor jugando :initform nil)
   (mezclador :accessor mezclador :initform nil)
   (sonidos :accessor sonidos :initform nil))
  (:panes (canvas (make-pane 'canvas-pane
                             :record nil
                             :background +black+
                             :min-width *ancho*
                             :min-height 1024
                             :display-time t
                             :display-function #'display)))
  (:layouts
   (:default canvas)))

(defmethod run-frame-top-level :before ((frame mc-wolf3d) &key &allow-other-keys)
  (let ((canvas (find-pane-named frame 'canvas)))
    (setf (pane-needs-redisplay canvas) :no-clear
          (stream-recording-p canvas) nil)))

(defmethod handle-repaint ((sheet canvas-pane) region)
  (window-clear sheet)
  (redisplay-frame-pane *frame* sheet))

(defparameter *tipografia* nil)
(defun carga-tipografia ()
  (mcclim-truetype::register-all-ttf-fonts (find-port) #P"./")
  (setf *tipografia* t))

(defun wolf3d-main (&optional (mapa *mapa*))
  (escenario:inicia-hilos)
  (mixalot:main-thread-init)
  (unless *tipografia* (carga-tipografia))
  (setf *frame* (make-application-frame 'mc-wolf3d
                                        :escenario (make-instance 'escenario
                                                                  :mapa (or mapa *mapa*)
                                                                  :texturas (carga-texturas))
                                        :calling-frame nil)
        (mezclador *frame*) (mixalot:create-mixer)
        (sonidos *frame*) (list (mixalot-vorbis:make-vorbis-streamer "./sonidos/22331__black-boe__wind.ogg")))
  (bt:make-thread (lambda ()
                    (run-frame-top-level *frame*)
                    (mixalot:destroy-mixer (mezclador *frame*))
                    (setf (mezclador *frame*) nil
                          *frame* nil)
                    (escenario:termina-hilos))
                  :name "mc-wolf3d-main"))

(define-mc-wolf3d-command (com-restablece-todo :name "restablece") ()
  (with-slots (posición dirección plano-camara) (escenario *frame*)
    (setf posición (vec 1.0 1.0)
          dirección (vec -1.0 0.0)
          plano-camara (vec 0 0.66)))
  (redisplay-frame-pane *frame* 'canvas :force-p t))

(define-mc-wolf3d-command (com-terminar-juego :name "terminar-juego") ()
  (when (jugando *frame*)
    (clim-sys:destroy-process (jugando *frame*))
    (setf (jugando *frame*) nil)))

(define-mc-wolf3d-command (com-salir :name "salir") ()
  (when (jugando *frame*)
    (clim-sys:destroy-process (jugando *frame*))
    (setf (jugando *frame*) nil))
  (escenario:termina-hilos)
  (frame-exit *application-frame*))

(defun juega%% (frame)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (with-slots (modo-rot modo-mov escenario) frame
    (loop while t
       if modo-mov do (mueve escenario (if (eq :adelante modo-mov) 1 -1))
       if modo-rot do (rota escenario (if (eq :derecha modo-rot) 1 -1))
       if (or modo-rot modo-mov) do (redisplay-frame-pane frame 'canvas))))

(defun juega (frame)
  (clim-sys:make-process
   (lambda ()
     (declare (optimize (speed 3) (safety 0) (debug 0)))
     (with-slots (modo-rot modo-mov escenario mezclador sonidos) frame
       (loop while t
          if modo-mov do (mueve escenario (if (eq :adelante modo-mov) 1 -1))
          if modo-rot do (rota escenario (if (eq :derecha modo-rot) 1 -1))
          if (or modo-rot modo-mov) do (redisplay-frame-pane frame 'canvas)
          else do
            (when (< (- (mixalot:streamer-length (car sonidos) mezclador)
                        (mixalot:streamer-position (car sonidos) mezclador))
                     100000)
              (mixalot:streamer-seek (car sonidos) mezclador 0))
          ;;(cond ((= (mixalot:streamer-length (car sonidos) mezclador) (mixalot:streamer-position (car sonidos) mezclador)) (mixalot:mixer-add-streamer mezclador (car sonidos))))
            (sleep 0.005)
            )))))

(define-mc-wolf3d-command (com-nuevo :name "Nuevo juego")
    ()
  (when *frame*
    (let ((frame *frame*))
      (unless (jugando frame)
        (mixalot:mixer-add-streamer (mezclador *frame*) (car (sonidos *frame*)))
        (setf (jugando frame) (juega frame))))))

(defmethod frame-standard-input ((frame mc-wolf3d)) (find-pane-named frame 'canvas))

(defmethod dispatch-event ((pane canvas-pane) (evento key-press-event))
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

(defmethod dispatch-event ((pane canvas-pane) (evento key-release-event))
  (when *frame*
    (case (keyboard-event-key-name evento)
      ((:up :down) (setf (slot-value *frame* 'modo-mov) nil))
      ((:right :left) (setf (slot-value *frame* 'modo-rot) nil)))))

;;;;; Presentación
(defparameter *tipo-titulos* (make-text-style :serif :bold 14))
(defparameter *tipo-subtitulos* (make-text-style "Wargames" "Regular" 48))
(defparameter *tipo-normal* (make-text-style "Edit Undo BRK" "Regular" 48))

(defmethod display ((frame mc-wolf3d) (pane canvas-pane))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (with-slots (escenario sprites tiempo tiempo-anterior periodo-cuadros) frame
    (declare (type single-float periodo-cuadros))
    (regenera escenario sprites)
    (setf tiempo-anterior tiempo
          tiempo (local-time:now)
          periodo-cuadros (coerce (the double-float
                                       (local-time:timestamp-difference tiempo
                                                                        tiempo-anterior))
                                  'single-float))
    (let ((desp (/ (- (bounding-rectangle-width (sheet-region pane)) *ancho*) 2.0)))
      (draw-design pane (make-image-design (imagen escenario)) :x desp :y desp))
    (let ((cadena (format nil "~5,2F fps" (/ periodo-cuadros))))
      (draw-rectangle* pane 0 1003 (the fixnum (+ 40 (the fixnum (text-size pane cadena :text-style *tipo-normal*)))) 900 :ink +black+)
      (draw-text* pane cadena 10 990 :text-style *tipo-normal* :ink +white+))))

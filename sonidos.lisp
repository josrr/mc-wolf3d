(in-package #:escenario)

(defun carga-sonidos (ruta)
  (let ((archivos (directory (merge-pathnames "*.ogg" ruta))))
    (make-array (length archivos)
                :initial-contents (mapcar (lambda (a)
                                            (mixalot-vorbis:make-vorbis-streamer (namestring a)))
                                          archivos))))

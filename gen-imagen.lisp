;; -*- coding: utf-8-unix; -*-
(in-package #:common-lisp-user)
(ql:quickload "mc-wolf3d")
;;
(uiop/image:register-image-restore-hook
 (lambda ()
   (mc-wolf3d:wolf3d-main escenario::*mapa-3*)
   (gc :full t)
   (sb-impl::toplevel-repl nil))
 nil)

(uiop/image:dump-image "mc-wolf3d" :executable t)



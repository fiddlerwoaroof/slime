;;; swank-buffer-streams.lisp --- Streams that output to a buffer
;;;
;;; Authors: Ed Langley  <el-github@elangley.org>
;;;
;;; License: This code has been placed in the Public Domain.  All warranties
;;;          are disclaimed.

(in-package :swank)

(defpackage :swank-buffer-streams
  (:use :cl)
  (:import-from :swank
                defslimefun
                add-hook
                encode-message
                send-event
                find-thread
                dcase
                current-socket-io
                send-to-emacs
                current-thread-id
                wait-for-event

                *emacs-connection*
                *event-hook*)
  (:export make-buffer-output-stream
           with-face
           #:with-variable-pitch))

(in-package :swank-buffer-streams)

(defun get-temporary-identifier ()
  (intern (symbol-name (gensym "BUFFER"))
          :keyword))

(defun make-buffer-output-stream (&optional (target-identifier (get-temporary-identifier)))
  (swank:ed-rpc '#:slime-make-buffer-stream-target (current-thread-id) target-identifier)
  (values (swank:make-output-stream-for-target *emacs-connection* target-identifier)
          target-identifier))


(defparameter +face-sym+ (intern "FACE" :swank-io-package))
(defparameter +font-lock-face-sym+ (intern "FONT-LOCK-FACE" :swank-io-package))

(defmacro with-face ((s face-spec) &body body)
  (let ((spec-sym (gensym "FACE-SPEC"))
        (spec (typecase )))
    `(let* ((,spec-sym ,face-spec)
            (swank::*output-context* (list* ',+face-sym+
                                            ,spec-sym
                                            ',+font-lock-face-sym+
                                            ,spec-sym
                                            swank::*output-context*)))
       ,@body
       (finish-output ,s))))

(defun call-with-variable-pitch (s cb)
  (with-face (s (intern "VARIABLE-PITCH" :swank-io-package))
    (funcall cb s)
    (finish-output s)))
(defmacro with-variable-pitch ((s) &body body)
  `(call-with-variable-pitch ,s
                             (lambda (,s)
                               ,@body)))

(provide :swank-buffer-streams)

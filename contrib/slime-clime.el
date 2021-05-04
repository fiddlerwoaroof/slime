(eval-and-compile
  (require 'slime))

(define-slime-contrib slime-clime
  "Display CLIM presentations in Emacs."
  (:authors "Luke Gorrie <luke@nuddy.co>")
  (:license "GPL")
  (:slime-dependencies slime-repl)
  (:swank-dependencies swank-clime)
  (:on-load
   (add-hook 'slime-event-hooks 'slime-dispatch-clime-event)))

(defun slime-dispatch-clime-event (event)
  (slime-dcase event
    ((:write-clime svg-data presentations)
     (with-current-buffer (slime-output-buffer)
       ;; Stolen mostly from slime-media.el, thanks Christophe!
       (let ((marker (slime-repl-output-target-marker :repl-result)))
         (goto-char marker)
         (slime-clime-insert-image (copy-seq svg-data) presentations)
         (insert "\n")
         ;; Move the input-start marker after the REPL result.
         (set-marker marker (point))))
     t)
    ((:accept-for-clime thread tag input-context)
     (slime-clime-set-input-context input-context))
    (t nil)))

(defun slime-clime-insert-image (svg-data presentations)
  (let ((map (slime-clime-presentations-map presentations)))
    (insert-image (slime-clime-create-image (copy-seq svg-data) map))
    ))
;;    (put-text-property (char-before) (point) 'slime-clime-connection (slime-connection))
;;    (put-text-property (char-before) (point) 'slime-clime-presentation-map map)))

(defun slime-clime-create-image (svg-data map)
  (create-image svg-data 'svg t :map map))

(defun slime-clime-presentations-map (presentations)
  "Return an image 'map' property for PRESENTATIONS."
  (mapcar (lambda (presentation)
            (cl-destructuring-bind (id area tooltip) presentation
              `(,area ,(gensym) (pointer arrow help-echo ,tooltip))))
          presentations))


;;;; Input

(defun slime-clime-set-input-context (slime-connection input-context)
  "Set INPUT-CONTEXT for SLIME-CONNECTION.
The input context is a list of presentation IDs ready for ACCEPT.")

(provide 'slime-clime)


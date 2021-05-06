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
         (slime-clime-insert-image svg-data presentations)
         (insert "\n")
         ;; Move the input-start marker after the REPL result.
         (set-marker marker (point))))
     t)
    ((:accept-for-clime thread tag input-context)
     (slime-clime-set-input-context (slime-connection) input-context)
     ;;(slime-dispatch-event `(:emacs-return ,thread ,tag 2565))
     t)
    (t nil)))

(defun slime-clime-insert-image (svg-data presentations)
  (let ((map (slime-clime-presentations-map presentations)))
    (insert-image (slime-clime-create-image svg-data map))
    (put-text-property (char-before) (point)
                       'slime-clime-connection (slime-connection))
    (put-text-property (char-before) (point)
                       'slime-clime-presentation-map map)))

(defun slime-clime-create-image (svg-data map)
  ;; Somehow create-image does not work properly when passed the image
  ;; data and map attributes directly. This is pure voodoo. Making a
  ;; fresh copy is the only reliable workaround I have been able to
  ;; find. -luke
  (eval (read (let (print-length print-depth)
                (prin1-to-string `(create-image ,svg-data 'svg t
                                                :map ',map))))))

(defun slime-clime-presentations-map (presentations)
  "Return an image 'map' property for PRESENTATIONS."
  (mapcar (lambda (presentation)
            (cl-destructuring-bind (id area tooltip) presentation
              `(,area ,(gensym) (pointer arrow help-echo ,tooltip))))
          presentations))


;;;; Input

(defun slime-clime-set-input-context (slime-connection input-context)
  "Set INPUT-CONTEXT for SLIME-CONNECTION.
The input context is a list of presentation IDs ready for ACCEPT."
  (slime-clime-map-images
   (lambda (image)
     (when (eq (get-text-property (point) 'slime-clime-connection)
               (slime-connection))
       ;; restrict presentations
       ))))

;;(defun slime-clime-map-images (fn)
;;  "Call FN with each CLIME image in all buffers in Emacs."


(provide 'slime-clime)


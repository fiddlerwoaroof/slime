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
    ((:write-clime svg-data)
     (with-current-buffer (slime-output-buffer)
       ;; Stolen mostly from slime-media.el, thanks Christophe!
       (let ((marker (slime-repl-output-target-marker :repl-result))
             (image (create-image svg-data 'svg t)))
         (goto-char marker)
         (insert-image image)
         (insert "\n")
         ;; Move the input-start marker after the REPL result.
         (set-marker marker (point))))
     t)
    (t nil)))

(provide 'slime-clime)


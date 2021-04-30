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
         (insert-image (slime-create-clime-image svg-data presentations))
         (insert "\n")
         ;; Move the input-start marker after the REPL result.
         (set-marker marker (point))))
     t)
    (t nil)))

(defun slime-create-clime-image (svg-data presentations)
  (apply 'create-image svg-data 'svg t
         :pointer 'hourglass
         (slime-clime-presentations-map presentations)))

(defun slime-clime-presentations-map (presentations)
  (let ((res
         (list :map
               (mapcar (lambda (presentation)
                         (cl-destructuring-bind (id area) presentation
                           `(,area ,(gensym) (pointer hand help-echo "presentation!"))))
                       presentations))))
    (message "res = %S" res)
    res))
  

(provide 'slime-clime)


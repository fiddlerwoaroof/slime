(eval-and-compile
  (require 'slime)
  (require 'text-property-search))

(define-slime-contrib slime-clime
  "Display CLIM presentations in Emacs."
  (:authors "Luke Gorrie <luke@nuddy.co>")
  (:license "GPL")
  (:slime-dependencies slime-repl)
  (:swank-dependencies swank-clime)
  (:on-load
   (add-hook 'slime-event-hooks 'slime-dispatch-clime-event)))

;;;; Keymap

(defvar slime-clime-image-keymap
  (let ((map (make-sparse-keymap)))
    ;; Tricky problem and solution:
    ;;
    ;; - We want to handle mouse clicks for all presentation IDs / areas.
    ;; - Emacs expects a separate keymap entry for each presentation ID.
    ;; - There can be lots presentation of IDs e.g. thousands/millions.
    ;; - We can use one catch-all [t] binding instead
    ;; - ... but then we also trap all keyboard events when point is on image.
    ;;
    ;; So on the one hand we don't really want to have
    ;; thousands/millions of separate key bindings to cover all
    ;; presentation IDs, and on the other hand we don't want the
    ;; keyboard to stop working when the point is on an image.
    ;;
    ;; What to do? Clever solution from wasamasa on #emacs:
    ;;
    ;; Define a catch-all [t] binding but tell Emacs that it is a menu
    ;; item. Why? Because Emacs keymaps support a special :filter
    ;; property that allows Elisp code to decide whether to process an
    ;; event or let it propagate up the keymap chain and,
    ;; mysteriously, this feature only works for menu items.
    ;;
    ;; (Perhaps, dear reader, you can see a simpler solution and/or
    ;; explanation...)
    ;;
    ;; FURTHERMORE,
    ;;
    ;; In CLIME it should be possible to kill/yank images as much as
    ;; you like, for example to paste your favorite CLIM images into a
    ;; special buffer for reference later. However, Emacs tends to
    ;; remove the 'keymap property when you yank text/images from the
    ;; kill ring (see `yank-excluded-properties'.)
    ;;
    ;; SO,
    ;;
    ;; CLIME includes a hack below such that we re-add this keymap to
    ;; all images in all Emacs buffers any time we have a convenient
    ;; opportunity to do so, which at the time of writing is when
    ;; visiting all the images to update the "input context" by
    ;; redefining which image areas are active.
    ;;
    ;; Maybe there is a simpler way to do that too, dear reader!
    ;;
    (define-key map [t] '(menu-item "" nil :filter slime-clime-event-filter))
    map))

(defun slime-clime-event-filter (map)
  "Process relevant mouse events and punt keyboard events."
  ;; NB: Return nil to punt "other" events.
  (when (mouse-event-p last-input-event)
    (when (eq (event-basic-type last-input-event) 'mouse-1)
      ;; Mouse event clicking on a presentation
      (slime-clime-mouse-1 last-input-event)
      ;; Mouse event with no action
      'ignore)))

(defun slime-clime-mouse-1 (&optional event)
  (interactive "e")
  (let* ((posn (cadr event))
         (image (posn-image posn))
         (area (posn-area posn)))
    (when (and area
               (slime-clime-input-context)
               (eq (slime-connection)
                   (image-property image 'slime-clime-connection)))
      (slime-clime-accept area)
      (slime-clime-reset-input-context))))

(defun slime-clime-accept (id)
  (let ((index (slime-clime-keyword-to-id id)))
    (cl-destructuring-bind (thread tag ctx) (slime-clime-input-context)
      (slime-dispatch-event `(:emacs-return ,thread ,tag ,index)))))

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
     (slime-clime-set-input-context (slime-connection) thread tag input-context)
     t)
    (t nil)))

(defun slime-clime-id-to-keyword (id)
  (intern (format ":%d" id)))

(defun slime-clime-keyword-to-id (id)
  (cl-parse-integer (substring (symbol-name id) 1))) ;

(defun slime-clime-insert-image (svg-data presentations)
  (let* ((map (slime-clime-presentations-map presentations))
         (props (list 'slime-clime-presentations presentations
                      :map map
                      'slime-clime-connection (slime-connection))))
    (insert-image (slime-clime-create-image svg-data map props))
    (add-text-properties (1- (point)) (point)
                         (list 'slime-clime-connection (slime-connection)
                               'keymap slime-clime-image-keymap))))

(defun slime-clime-create-image (svg-data map properties)
  ;; Somehow create-image does not work properly when passed the image
  ;; data and map attributes directly. This is pure voodoo. Making a
  ;; fresh copy is the only reliable workaround I have been able to
  ;; find. -luke
  (eval (append (slime-clime-reread `(create-image ,svg-data 'svg t :map ',map))
                (mapcar (lambda (prop) (list 'quote prop)) properties))))

(defun slime-clime-reread (form)
  "Reread FORM by prining readably and then reading.
Used as an awful workaround for voodoo object identity problems."
  (read (let (print-length print-depth) (prin1-to-string form))))

(defun slime-clime-presentations-map (presentations &optional pointer-shape)
  "Return an image 'map' property for PRESENTATIONS."
  (mapcar (lambda (presentation)
            (cl-destructuring-bind (number area tooltip) presentation
              (let ((id (slime-clime-id-to-keyword number)))
                `(,area ,id (pointer ,(or pointer-shape 'arrow)
                                     help-echo ,tooltip)))))
          presentations))


;;;; Input

(slime-def-connection-var slime-clime-input-context nil
  "Current CLIM input context for the connection.
List of (THREAD TAG INPUT-CONTEXT)")

(defun slime-clime-set-input-context (slime-connection thread tag input-context)
  "Set INPUT-CONTEXT for SLIME-CONNECTION.
The input context is a list of presentation IDs ready for ACCEPT."
  ;; If an input context already exists then abort it as stale.
  (when (slime-clime-input-context)
    (cl-destructuring-bind (thread tag ctx) (slime-clime-input-context)
      (slime-dispatch-event `(:emacs-return ,thread ,tag :abort)))
    (setf (slime-clime-input-context) nil))
  ;; Save input context
  (setf (slime-clime-input-context)
        (list thread tag input-context))
  ;; Update presentations
  (let ((enabled (slime-clime-enabled-presentations input-context)))
    (slime-clime-map-images
     (lambda (image)
       (slime-clime-restore-keymap-before-point)
       (when (eq (get-text-property (1- (point)) 'slime-clime-connection)
                 (slime-connection))
         (slime-clime-filter-presentations image enabled)))
     slime-connection)))

(defun slime-clime-enabled-presentations (input-context)
  "Return a hashtable of the enabled presentation set."
  (let ((enabled (make-hash-table)))
    (mapc (lambda (id) (puthash id t enabled)) input-context)
    enabled))

(defun slime-clime-filter-presentations (image enabled)
  "Filter active areas of IMAGE based on ENABLED hash-table set."
  (let* ((all (image-property image 'slime-clime-presentations))
         (filtered (cl-remove-if-not (lambda (area) (gethash (car area) enabled)) all)))
    (setf (image-property image :map) nil)
    (setf (image-property image :map)
          (slime-clime-reread (slime-clime-presentations-map filtered 'hand)))))

(defun slime-clime-reset-input-context (&optional connection)
  "Reset the current input context."
  (slime-clime-map-images
   (lambda (image)
     (slime-clime-restore-keymap-before-point)
     (setf (image-property image :map)
           (slime-clime-reread
            (slime-clime-presentations-map
             (image-property image 'slime-clime-presentations)))))
   (or connection (slime-connection))))

(defun slime-clime-map-images (fn connection)
  "Call FN with each CLIME image in all buffers in Emacs."
  (dolist (b (buffer-list))
    (with-current-buffer b
      (save-excursion
        (goto-char (point-min))
        (while (text-property-search-forward 'slime-clime-connection connection t)
          (let ((image (get-text-property (1- (point)) 'display)))
            (when image
              (funcall fn image))))))))

(defun slime-clime-restore-keymap-before-point ()
  "Restore the CLIME keymap to the image immediately before point.
This remedies the situation where the keymap is missing for some
reason, for example because the image was killed and yanked and
`yank-excluded-properties' caused the keymap to be stripped off
from the new copy."
  (put-text-property (1- (point)) (point)
                     'keymap slime-clime-image-keymap))


;;;; Image information

(defslimefun svg-image-size (svg-data)
  (let ((image (cons 'image (list :type 'svg :data svg-data))))
    (image-size image t)))

(defslimefun window-width-for-margin ()
  (window-body-width nil t))


(provide 'slime-clime)


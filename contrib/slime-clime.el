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
    ;; Currently using a catch-all keybinding because Emacs seems (?)
    ;; to insist that a specific [mouse-1] binding would be matched as
    ;; [area-id mouse-1] i.e. that we would need to define a
    ;; keybinding for each and every presentation. That's a bit much.
    ;; So for now we just catch all key/mouse events on the image.
    ;;
    ;; (If you put the Emacs point/cursor over the image and the
    ;; keyboard stops working then try clicking somewhere else so that
    ;; the point/cursor can escape...
    ;;(define-key map [mouse-1] 'slime-clime-mouse-1)
    (define-key map [t] 'slime-clime-input-event)
    map))

(defun slime-clime-input-event (&optional event)
  (interactive "e")
  (when (eq (event-basic-type event) 'mouse-1)
    (slime-clime-mouse-1 event)))

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
    (put-text-property (1- (point)) (point)
                       'slime-clime-connection (slime-connection))
    (put-text-property (1- (point)) (point)
                       'keymap slime-clime-image-keymap)))

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
  (slime-clime-map-images
   (lambda (image)
     (when (eq (get-text-property (1- (point)) 'slime-clime-connection)
               (slime-connection))
       (slime-clime-filter-presentations image input-context)))
   slime-connection))

(defun slime-clime-filter-presentations (image input-context)
  "Filter active areas of IMAGE based on INPUT-CONTEXT."
  (let* ((all (image-property image 'slime-clime-presentations))
         (filtered (cl-remove-if-not (lambda (area)
                                       (member (car area) input-context))
                                     all)))
    (setf (image-property image :map) nil)
    (setf (image-property image :map)
          (slime-clime-reread (slime-clime-presentations-map filtered 'hand)))))

(defun slime-clime-reset-input-context (&optional connection)
  "Reset the current input context."
  (slime-clime-map-images
   (lambda (image)
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


;;;; Image information

(defslimefun svg-image-size (svg-data)
  (let ((image (cons 'image (list :type 'svg :data svg-data))))
    (image-size image t)))

(defslimefun window-width-for-margin ()
  (window-body-width nil t))


(provide 'slime-clime)


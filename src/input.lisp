(in-package #:terminal-backend-ghostty)

;;; Input encoding. Each encoder is reconfigured from the terminal's live
;;; modes before every event (ghostty_*_encoder_set_from_terminal).

(defparameter +mod-bits+
  '((:shift . 0) (:ctrl . 1) (:alt . 2) (:super . 3) (:caps-lock . 4) (:num-lock . 5)))

(defun mods (modifiers)
  (loop for m in modifiers sum (ash 1 (cdr (assoc m +mod-bits+)))))

(defun key-enum (key)
  "Protocol key names are the ffi:key enum keywords."
  (if (cffi:foreign-enum-value 'ffi:key key :errorp nil)
      key
      (tp:unsupported (list :key key))))

(defmethod tp:encode-key ((terminal ghostty-terminal) key &key text modifiers
                                                            (action :press))
  (let ((encoder (native terminal 1))
        (event (native terminal 2))
        (handle (ghostty-terminal-handle terminal)))
    (check (ffi:key-encoder-set-from-terminal encoder handle) :key-encoder terminal)
    (check (ffi:key-event-set-key event (key-enum key)) :key terminal)
    (check (ffi:key-event-set-action event action) :action terminal)
    (check (ffi:key-event-set-mods event (mods modifiers)) :mods terminal)
    (check (ffi:key-event-set-consumed-mods event 0) :consumed-mods terminal)
    (check (ffi:key-event-set-unshifted-codepoint
            event (if (and text (= 1 (length text)))
                      (char-code (char-downcase (char text 0)))
                      0))
           :unshifted-codepoint terminal)
    (cffi:with-foreign-string ((bytes length) (or text "") :encoding :utf-8
                                                           :null-terminated-p nil)
      (gv:key-event-set-utf8 event bytes (if text length 0))
      (prog1 (gv:call-with-output-buffer
              (lambda (buf len written) (ffi:key-encoder-encode encoder event buf len written))
              :key-encode)
        ;; The event keeps the pointer; clear it before BYTES goes away.
        (gv:key-event-set-utf8 event (cffi:null-pointer) 0)))))

(defparameter +buttons+
  '((:left . :left) (:middle . :middle) (:right . :right)
    (:wheel-up . :four) (:wheel-down . :five) (:wheel-left . :six)
    (:wheel-right . :seven) (:back . :eight) (:forward . :nine)
    (:button-10 . :ten) (:button-11 . :eleven)))

(defmethod tp:encode-mouse ((terminal ghostty-terminal) button action x y
                            &key modifiers pixel-x pixel-y)
  (let ((encoder (native terminal 3))
        (event (native terminal 4))
        (handle (ghostty-terminal-handle terminal)))
    (check (ffi:mouse-encoder-set-from-terminal encoder handle) :mouse-encoder terminal)
    (multiple-value-bind (cols rows) (tp:terminal-size terminal)
      ;; Without pixel positions, encode in a 1×1-pixel-cell space so cell
      ;; coordinates are positions; +0.5 lands in the middle of the cell.
      (let ((cell-w (if pixel-x (max 1 (round (data terminal :width-px :uint32) cols)) 1))
            (cell-h (if pixel-y (max 1 (round (data terminal :height-px :uint32) rows)) 1)))
        (gv:with-sized (size (:struct ffi:mouse-encoder-size))
          (cffi:with-foreign-slots ((ffi:screen-width ffi:screen-height
                                     ffi:cell-width ffi:cell-height)
                                    size (:struct ffi:mouse-encoder-size))
            (setf ffi:screen-width (* cols cell-w) ffi:screen-height (* rows cell-h)
                  ffi:cell-width cell-w ffi:cell-height cell-h))
          (check (ffi:mouse-encoder-set encoder :size size) :mouse-size terminal))
        (check (ffi:mouse-event-set-action event action) :mouse-action terminal)
        (if button
            (check (ffi:mouse-event-set-button event (cdr (assoc button +buttons+)))
                   :mouse-button terminal)
            (check (ffi:mouse-event-clear-button event) :mouse-button terminal))
        (check (ffi:mouse-event-set-mods event (mods modifiers)) :mouse-mods terminal)
        (gv:mouse-event-set-position event
                                     (or pixel-x (+ x 0.5)) (or pixel-y (+ y 0.5)))
        (gv:call-with-output-buffer
         (lambda (buf len written) (ffi:mouse-encoder-encode encoder event buf len written))
         :mouse-encode)))))

(defmethod tp:encode-focus ((terminal ghostty-terminal) focused-p)
  (if (tp:terminal-mode terminal :focus-event)
      (gv:call-with-output-buffer
       (lambda (buf len written)
         (ffi:focus-encode (if focused-p :gained :lost) buf len written))
       :focus-encode)
      (make-array 0 :element-type '(unsigned-byte 8))))

(defmethod tp:encode-paste ((terminal ghostty-terminal) string)
  (let ((bracketed (tp:terminal-mode terminal :bracketed-paste)))
    (cffi:with-foreign-string ((data length) string :encoding :utf-8
                                                    :null-terminated-p nil)
      ;; paste_encode rewrites DATA in place; retrying with it is allowed.
      (gv:call-with-output-buffer
       (lambda (buf len written)
         (ffi:paste-encode data length bracketed buf len written))
       :paste-encode
       :initial-size (+ length 16)))))

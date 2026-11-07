(in-package #:terminal-backend-ghostty)

;;; A frame owns a GhosttyRenderState plus one row iterator and one row-cells
;;; handle, reused across updates. FRAME-UPDATE copies the viewport into
;;; fresh Lisp rows, so nothing a caller holds aliases native memory.

(defclass ghostty-frame (tp:frame)
  ((natives :initarg :natives :reader frame-natives
            :documentation "Cons cell (render-state row-iterator row-cells).")
   (cols :initform 0 :reader %frame-cols)
   (rows :initform #() :reader %frame-rows)
   (cursor :initform nil :reader tp:frame-cursor)
   (colors :initform nil :reader tp:frame-colors)))

(defun %free-frame (natives)
  (destructuring-bind (&optional state iterator cells) (car natives)
    (when cells (ffi:render-state-row-cells-free cells))
    (when iterator (ffi:render-state-row-iterator-free iterator))
    (when state (ffi:render-state-free state))
    (setf (car natives) nil)))

(defmethod tp:make-frame ((terminal ghostty-terminal))
  (let* ((null (cffi:null-pointer))
         (natives (list (list (%new-handle #'ffi:render-state-new null :out)
                              (%new-handle #'ffi:render-state-row-iterator-new null :out)
                              (%new-handle #'ffi:render-state-row-cells-new null :out))))
         (frame (make-instance 'ghostty-frame :terminal terminal :natives natives)))
    (trivial-garbage:finalize frame (lambda () (%free-frame natives)))
    frame))

(defmethod tp:frame-close ((frame ghostty-frame))
  (trivial-garbage:cancel-finalization frame)
  (%free-frame (frame-natives frame)))

(defun %frame-native (frame index)
  (or (nth index (car (frame-natives frame)))
      (error 'tp:terminal-closed-error :terminal (tp:frame-terminal frame))))

(defun rgb (pointer)
  (cffi:with-foreign-slots ((ffi:r ffi:g ffi:b) pointer (:struct ffi:color-rgb))
    (tp:make-rgb ffi:r ffi:g ffi:b)))

(defun read-colors (state)
  (gv:with-sized (colors (:struct ffi:render-state-colors))
    (check (ffi:render-state-get state :colors colors) :colors)
    (flet ((slot (name)
             (cffi:foreign-slot-pointer colors '(:struct ffi:render-state-colors) name)))
      (let ((palette (make-array 256)))
        (dotimes (i 256)
          (setf (aref palette i)
                (rgb (cffi:mem-aptr (slot 'ffi:palette) '(:struct ffi:color-rgb) i))))
        (tp:make-terminal-colors
         :foreground (rgb (slot 'ffi:foreground))
         :background (rgb (slot 'ffi:background))
         :cursor (when (cffi:foreign-slot-value colors '(:struct ffi:render-state-colors)
                                                'ffi:cursor-has-value)
                   (rgb (slot 'ffi:cursor)))
         :palette palette)))))

(defun read-cursor (state)
  (gv:with-sized (cursor (:struct ffi:render-state-cursor))
    (check (ffi:render-state-get state :cursor cursor) :cursor)
    (cffi:with-foreign-slots ((ffi:viewport-has-value ffi:viewport-x ffi:viewport-y
                               ffi:visible ffi:blinking ffi:wide-tail ffi:visual-style)
                              cursor (:struct ffi:render-state-cursor))
      (when ffi:viewport-has-value
        (tp:make-cursor :x ffi:viewport-x :y ffi:viewport-y
                        :visible-p ffi:visible :blinking-p ffi:blinking
                        :wide-tail-p ffi:wide-tail
                        :style (case ffi:visual-style
                                 (:bar :bar) (:underline :underline)
                                 (:block-hollow :block-hollow) (t :block)))))))

(defun style-color (pointer palette)
  "GhosttyStyleColor at POINTER → protocol colour, resolved through PALETTE."
  (let ((tag (cffi:foreign-slot-value pointer '(:struct ffi:style-color) 'ffi:tag))
        (value (cffi:foreign-slot-pointer pointer '(:struct ffi:style-color) 'ffi:value)))
    (case tag
      (:palette (aref palette (cffi:mem-ref value :uint8)))
      (:rgb (rgb value))
      (t :default))))

(defun cell-color (cells key)
  (cffi:with-foreign-object (out '(:struct ffi:color-rgb))
    (let ((result (ffi:render-state-row-cells-get cells key out)))
      (if (eq result :no-value)
          :default
          (progn (check result key) (rgb out))))))

(defun cell-text (cells)
  (cffi:with-foreign-object (buffer '(:struct ffi:buffer))
    (cffi:with-foreign-slots ((ffi:ptr ffi:cap ffi:len) buffer (:struct ffi:buffer))
      (setf ffi:ptr (cffi:null-pointer) ffi:cap 0 ffi:len 0)
      (let ((result (ffi:render-state-row-cells-get cells :graphemes-utf8 buffer)))
        (case result
          (:success "")
          (:out-of-space
           (let ((needed ffi:len))
             (cffi:with-foreign-object (bytes :uint8 needed)
               (setf ffi:ptr bytes ffi:cap needed)
               (check (ffi:render-state-row-cells-get cells :graphemes-utf8 buffer)
                      :graphemes-utf8)
               (gv:foreign-utf8 bytes ffi:len))))
          (t (check result :graphemes-utf8) ""))))))

(defun cell-raw (cells)
  "→ (values width hyperlink-p) from the cell's packed GhosttyCell."
  (cffi:with-foreign-object (raw 'ffi:cell)
    (check (ffi:render-state-row-cells-get cells :raw raw) :raw)
    (let ((value (cffi:mem-ref raw 'ffi:cell)))
      (values (case (ffi:cell-wide value)
                (:wide :wide)
                (:spacer-tail :spacer)
                (t :narrow))
              (ffi:cell-hyperlink value)))))

(defun cell-hyperlink (terminal x y)
  "OSC 8 URI of viewport cell X, Y, or NIL."
  (cffi:with-foreign-object (ref '(:struct ffi:grid-ref))
    (when (eq :success (gv:terminal-grid-ref (ghostty-terminal-handle terminal)
                                             :viewport x y ref))
      (let ((octets (gv:call-with-output-buffer
                     (lambda (buf len written)
                       (ffi:grid-ref-hyperlink-uri ref buf len written))
                     :hyperlink-uri)))
        (when (plusp (length octets))
          (cffi:with-pointer-to-vector-data (p octets)
            (gv:foreign-utf8 p (length octets))))))))

(defun cell-style (cells palette &optional hyperlink)
  (cffi:with-foreign-object (style '(:struct ffi:style))
    (check (ffi:render-state-row-cells-get cells :style style) :style)
    (let ((flags (cffi:foreign-slot-value style '(:struct ffi:style) 'ffi:flags)))
      (tp:make-style
       :foreground (cell-color cells :fg-color)
       :background (cell-color cells :bg-color)
       :underline-color (style-color (cffi:foreign-slot-pointer
                                      style '(:struct ffi:style) 'ffi:underline-color)
                                     palette)
       :bold (ffi:style-flags-bold flags)
       :italic (ffi:style-flags-italic flags)
       :faint (ffi:style-flags-faint flags)
       :blink (ffi:style-flags-blink flags)
       :inverse (ffi:style-flags-inverse flags)
       :invisible (ffi:style-flags-invisible flags)
       :strikethrough (ffi:style-flags-strikethrough flags)
       :overline (ffi:style-flags-overline flags)
       :underline (let ((u (ffi:style-flags-underline flags)))
                    (if (member u '(:single :double :curly :dotted :dashed)) u nil))
       :hyperlink hyperlink))))

(defun read-row (terminal iterator cells cols palette y)
  (let ((dirty (cffi:with-foreign-object (out 'ffi:bool)
                 (check (ffi:render-state-row-get iterator :dirty out) :row-dirty)
                 (cffi:mem-ref out 'ffi:bool)))
        (out (make-array cols)))
    (cffi:with-foreign-object (handle :pointer)
      (setf (cffi:mem-ref handle :pointer) cells)
      (check (ffi:render-state-row-get iterator :cells handle) :row-cells))
    (loop for x from 0 below cols
          while (ffi:render-state-row-cells-next cells)
          do (setf (aref out x)
                   (multiple-value-bind (width link-p) (cell-raw cells)
                     (tp:make-cell :text (cell-text cells)
                                   :width width
                                   :style (cell-style cells palette
                                                      (and link-p
                                                           (cell-hyperlink terminal x y)))))))
    (loop for x from 0 below cols
          unless (aref out x) do (setf (aref out x) (tp:make-cell)))
    (make-instance 'tp:row :y y :dirty-p dirty :cells out)))

(defmethod tp:frame-update ((frame ghostty-frame))
  (let* ((terminal (tp:frame-terminal frame))
         (state (%frame-native frame 0))
         (iterator (%frame-native frame 1))
         (cells (%frame-native frame 2)))
    (gv:without-float-traps
      (check (ffi:render-state-update state (ghostty-terminal-handle terminal)) :update))
    (let* ((cols (cffi:with-foreign-object (n :uint16)
                   (check (ffi:render-state-get state :cols n) :cols)
                   (cffi:mem-ref n :uint16)))
           (colors (read-colors state))
           (palette (tp:terminal-colors-palette colors))
           (rows '()))
      (cffi:with-foreign-object (handle :pointer)
        (setf (cffi:mem-ref handle :pointer) iterator)
        (check (ffi:render-state-get state :row-iterator handle) :row-iterator))
      (loop for y from 0
            while (ffi:render-state-row-iterator-next iterator)
            do (push (read-row terminal iterator cells cols palette y) rows))
      (with-slots ((c cols) (r rows) (cu cursor) (co colors)) frame
        (setf c cols
              r (coerce (nreverse rows) 'simple-vector)
              cu (read-cursor state)
              co colors)))
    (cffi:with-foreign-object (dirty 'ffi:render-state-dirty)
      (check (ffi:render-state-get state :dirty dirty) :dirty)
      (case (cffi:mem-ref dirty 'ffi:render-state-dirty)
        (:false :none) (:partial :partial) (t :full)))))

(defmethod tp:frame-size ((frame ghostty-frame))
  (values (%frame-cols frame) (length (%frame-rows frame))))

(defmethod tp:frame-row ((frame ghostty-frame) y)
  (aref (%frame-rows frame) y))

(defmethod tp:frame-mark-clean ((frame ghostty-frame))
  (check (ffi:render-state-clean (%frame-native frame 0)) :clean)
  (loop for row across (%frame-rows frame)
        do (setf (tp:row-dirty-p row) nil)))

(in-package #:terminal-backend-ghostty)

(defclass ghostty-terminal (tp:terminal)
  ((handle :initarg :handle :reader ghostty-terminal-handle)
   (id :initarg :id :reader terminal-id)
   (natives :initarg :natives :reader terminal-natives
            :documentation "Cons cell (handle key-encoder key-event mouse-encoder
mouse-event) shared with the finalizer; NILed out on close."))
  (:documentation "A terminal-protocol terminal backed by a GhosttyTerminal."))

(defun %new-handle (constructor &rest args)
  "Call a libghostty-vt *_new. The :OUT marker in ARGS is replaced by the
pointer the handle is written through. → the handle."
  (cffi:with-foreign-object (out :pointer)
    (check (apply constructor (substitute out :out args)) constructor)
    (cffi:mem-ref out :pointer)))

(defun %free-natives (natives)
  (destructuring-bind (&optional handle key-encoder key-event mouse-encoder mouse-event)
      (car natives)
    (when mouse-event (ffi:mouse-event-free mouse-event))
    (when mouse-encoder (ffi:mouse-encoder-free mouse-encoder))
    (when key-event (ffi:key-event-free key-event))
    (when key-encoder (ffi:key-encoder-free key-encoder))
    (when handle (ffi:terminal-free handle))
    (setf (car natives) nil)))

(defmethod tp:backend-make-terminal ((backend ghostty-backend) &key cols rows host
                                     scrollback-lines &allow-other-keys)
  (let* ((null (cffi:null-pointer))
         (handle (%new-handle #'ffi:terminal-new null :out cols rows))
         (natives (list (list handle
                              (%new-handle #'ffi:key-encoder-new null :out)
                              (%new-handle #'ffi:key-event-new null :out)
                              (%new-handle #'ffi:mouse-encoder-new null :out)
                              (%new-handle #'ffi:mouse-event-new null :out))))
         (terminal (make-instance 'ghostty-terminal :backend backend :host host
                                                    :handle handle :natives natives)))
    (setf (slot-value terminal 'id) (register terminal))
    (install-effects handle (terminal-id terminal))
    (when scrollback-lines
      (cffi:with-foreign-object (n :size)
        (setf (cffi:mem-ref n :size) scrollback-lines)
        (check (ffi:terminal-set handle :scrollback-max-lines n) :scrollback-max-lines)))
    (trivial-garbage:finalize terminal (lambda () (%free-natives natives)))
    terminal))

(defun native (terminal index)
  (nth index (car (terminal-natives terminal))))

(defmethod tp:terminal-close ((terminal ghostty-terminal))
  (unregister (terminal-id terminal))
  (trivial-garbage:cancel-finalization terminal)
  (%free-natives (terminal-natives terminal)))

;;; Program → terminal

(defmethod tp:terminal-feed ((terminal ghostty-terminal) octets &key (start 0) end)
  (gv:with-octets-pointer (pointer length octets :start start :end end)
    (gv:without-float-traps
      (ffi:terminal-vt-write (ghostty-terminal-handle terminal) pointer length))))

(defmethod tp:terminal-resize ((terminal ghostty-terminal) cols rows
                               &key (cell-width-px 0) (cell-height-px 0))
  (check (ffi:terminal-resize (ghostty-terminal-handle terminal) cols rows
                              (or cell-width-px 0) (or cell-height-px 0))
         :resize terminal))

(defmethod tp:terminal-reset ((terminal ghostty-terminal))
  (ffi:terminal-reset (ghostty-terminal-handle terminal)))

;;; State

(defun data (terminal key type)
  (cffi:with-foreign-object (out type)
    (check (ffi:terminal-get (ghostty-terminal-handle terminal) key out) key terminal)
    (cffi:mem-ref out type)))

(defun data-string (terminal key)
  (cffi:with-foreign-object (out '(:struct ffi:string))
    (check (ffi:terminal-get (ghostty-terminal-handle terminal) key out) key terminal)
    (let ((s (gstring out)))
      (if (string= s "") nil s))))

(defmethod tp:terminal-size ((terminal ghostty-terminal))
  (values (data terminal :cols :uint16) (data terminal :rows :uint16)))

(defmethod tp:terminal-cursor ((terminal ghostty-terminal))
  (tp:make-cursor :x (data terminal :cursor-x :uint16)
                  :y (data terminal :cursor-y :uint16)
                  :visible-p (data terminal :cursor-visible 'ffi:bool)))

(defmethod tp:terminal-title ((terminal ghostty-terminal))
  (data-string terminal :title))

(defmethod tp:terminal-pwd ((terminal ghostty-terminal))
  (data-string terminal :pwd))

(defmethod tp:terminal-active-screen ((terminal ghostty-terminal))
  (data terminal :active-screen 'ffi:terminal-screen))

(defmethod tp:backend-mode ((terminal ghostty-terminal) kind number)
  (cffi:with-foreign-object (config '(:struct ffi:terminal-mode-config))
    (setf (cffi:foreign-slot-value config '(:struct ffi:terminal-mode-config) 'ffi:mode)
          (gv:make-mode number (eq kind :ansi)))
    (check (ffi:terminal-get (ghostty-terminal-handle terminal) :mode config) :mode terminal)
    (cffi:foreign-slot-value config '(:struct ffi:terminal-mode-config) 'ffi:value)))

(defmethod (setf tp:backend-mode) (value (terminal ghostty-terminal) kind number)
  (cffi:with-foreign-object (config '(:struct ffi:terminal-mode-config))
    (cffi:with-foreign-slots ((ffi:mode ffi:value) config (:struct ffi:terminal-mode-config))
      (setf ffi:mode (gv:make-mode number (eq kind :ansi))
            ffi:value (and value t)))
    (check (ffi:terminal-set (ghostty-terminal-handle terminal) :mode config) :mode terminal))
  value)

(defmethod tp:terminal-mouse-tracking ((terminal ghostty-terminal))
  (cond ((tp:terminal-mode terminal :mouse-event-any) :any)
        ((tp:terminal-mode terminal :mouse-event-button) :button)
        ((tp:terminal-mode terminal :mouse-event-normal) :normal)
        ((tp:terminal-mode terminal :mouse-event-x10) :x10)
        (t :none)))

(defmethod tp:terminal-scroll-viewport ((terminal ghostty-terminal) &key to delta)
  (let ((handle (ghostty-terminal-handle terminal)))
    (cond (to (check (gv:terminal-scroll-viewport
                      handle (ecase to (:top :top) (:bottom :bottom)))
                     :scroll-viewport terminal))
          (delta (check (gv:terminal-scroll-viewport handle :delta delta)
                        :scroll-viewport terminal))))
  nil)

(defmethod tp:terminal-text ((terminal ghostty-terminal) &key (format :plain)
                                                            (region :viewport))
  (if (and (eq format :plain) (eq region :viewport))
      ;; The formatter covers the whole screen; the viewport comes from a frame.
      (let ((lines (getf (tp:terminal-snapshot terminal :cells nil) :lines)))
        (string-right-trim '(#\Newline)
                           (format nil "~{~A~%~}" lines)))
      (progn
        (when (eq region :viewport)
          (tp:unsupported (list :text format :viewport) terminal))
        (format-screen terminal format))))

(defun format-screen (terminal format)
  (let ((formatter (%new-handle #'ffi:formatter-terminal-new (cffi:null-pointer) :out
                                (ghostty-terminal-handle terminal))))
    (unwind-protect
         (progn
           (cffi:with-foreign-object (emit 'ffi:formatter-format)
             (setf (cffi:mem-ref emit 'ffi:formatter-format) format)
             (check (ffi:formatter-set formatter :emit emit) :formatter-emit terminal))
           (cffi:with-foreign-objects ((ptr :pointer) (len :size))
             (check (ffi:formatter-format-alloc formatter (cffi:null-pointer) ptr len)
                    :format terminal)
             (let ((p (cffi:mem-ref ptr :pointer)) (n (cffi:mem-ref len :size)))
               (prog1 (gv:foreign-utf8 p n)
                 (ffi:free (cffi:null-pointer) p n)))))
      (ffi:formatter-free formatter))))

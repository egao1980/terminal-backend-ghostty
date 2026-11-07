(in-package #:terminal-backend-ghostty)

;;; libghostty-vt calls these from inside ghostty_terminal_vt_write. Userdata
;;; is an integer id, never a Lisp object. Every effect goes through
;;; TP:CALL-HOST, which (during a feed) records a failing host method instead
;;; of unwinding through the native parser.

(defvar *terminals* (trivial-garbage:make-weak-hash-table :weakness :value
                                                           :test 'eql)
  "id → GHOSTTY-TERMINAL. Weak on the value so an unclosed terminal can still
be collected; its finalizer frees the native handles.")

(defvar *next-id* 0)

(defun register (terminal)
  (let ((id (incf *next-id*)))
    (setf (gethash id *terminals*) terminal)
    id))

(defun unregister (id)
  (remhash id *terminals*))

(defun lookup (userdata)
  (gethash (cffi:pointer-address userdata) *terminals*))

(defmacro define-effect (name (&rest args) return-type &body body)
  "DEFCALLBACK whose body runs with TERMINAL bound from USERDATA. If the
terminal is gone, the callback does nothing and returns DEFAULT."
  (let ((default (if (eq return-type :void) nil (getf (first body) :default))))
    (when (and (consp (first body)) (eq (first (first body)) :default))
      (setf body (rest body)))
    `(cffi:defcallback ,name ,return-type ((handle :pointer) (userdata :pointer) ,@args)
       (declare (ignore handle))
       (let ((terminal (lookup userdata)))
         (if terminal
             (progn ,@body)
             ,default)))))

(defun copy-octets (pointer length)
  (let ((out (make-array length :element-type '(unsigned-byte 8))))
    (dotimes (i length out)
      (setf (aref out i) (cffi:mem-aref pointer :uint8 i)))))

(defun gstring (pointer)
  "Lisp string from a GhosttyString struct at POINTER."
  (cffi:with-foreign-slots ((ffi:ptr ffi:len) pointer (:struct ffi:string))
    (gv:foreign-utf8 ffi:ptr ffi:len)))

(define-effect on-write-pty ((data :pointer) (len :size)) :void
  (tp:call-host terminal #'tp:host-write-pty (copy-octets data len)))

(define-effect on-bell () :void
  (tp:call-host terminal #'tp:host-bell))

(define-effect on-title-changed () :void
  (tp:call-host terminal #'tp:host-title-changed (tp:terminal-title terminal)))

(define-effect on-pwd-changed () :void
  (tp:call-host terminal #'tp:host-pwd-changed (tp:terminal-pwd terminal)))

(defun clipboard-keyword (location)
  (case location (:primary :primary) (:selection :primary) (t :clipboard)))

(define-effect on-clipboard-write ((write :pointer)) :void
  (cffi:with-foreign-slots ((ffi:location ffi:contents ffi:contents-len ffi:reply)
                            write (:struct ffi:clipboard-write))
    (let ((text (loop for i below ffi:contents-len
                      for content = (cffi:mem-aptr ffi:contents
                                                   '(:struct ffi:clipboard-content) i)
                      for mime = (gstring (cffi:foreign-slot-pointer
                                           content '(:struct ffi:clipboard-content) 'ffi:mime))
                      when (or (string= mime "") (eql 0 (search "text/" mime)))
                        return (gstring (cffi:foreign-slot-pointer
                                         content '(:struct ffi:clipboard-content) 'ffi:data)))))
      (tp:call-host terminal #'tp:host-clipboard-write
                    (clipboard-keyword ffi:location) (or text ""))
      (gv:with-sized (reply (:struct ffi:clipboard-write-reply))
        (setf (cffi:foreign-slot-value reply '(:struct ffi:clipboard-write-reply) 'ffi:result)
              :success)
        (cffi:foreign-funcall-pointer ffi:reply () :pointer write :pointer reply :void)))))

(define-effect on-clipboard-read ((read :pointer)) :void
  (cffi:with-foreign-slots ((ffi:location ffi:reply) read (:struct ffi:clipboard-read))
    (let ((text (tp:call-host terminal #'tp:host-clipboard-read
                              (clipboard-keyword ffi:location))))
      (gv:with-sized (reply (:struct ffi:clipboard-read-reply))
        (if (stringp text)
            (cffi:with-foreign-strings (((mime mime-len) "text/plain" :null-terminated-p nil)
                                        ((data data-len) text :encoding :utf-8
                                                              :null-terminated-p nil))
              (cffi:with-foreign-object (content '(:struct ffi:clipboard-content))
                (flet ((set-string (slot ptr len)
                         (let ((s (cffi:foreign-slot-pointer
                                   content '(:struct ffi:clipboard-content) slot)))
                           (setf (cffi:foreign-slot-value s '(:struct ffi:string) 'ffi:ptr) ptr
                                 (cffi:foreign-slot-value s '(:struct ffi:string) 'ffi:len) len))))
                  (set-string 'ffi:mime mime mime-len)
                  (set-string 'ffi:data data data-len))
                (cffi:with-foreign-slots ((ffi:result ffi:contents ffi:contents-len)
                                          reply (:struct ffi:clipboard-read-reply))
                  (setf ffi:result :success ffi:contents content ffi:contents-len 1))
                (cffi:foreign-funcall-pointer ffi:reply () :pointer read :pointer reply :void)))
            (progn
              (setf (cffi:foreign-slot-value reply '(:struct ffi:clipboard-read-reply) 'ffi:result)
                    :denied)
              (cffi:foreign-funcall-pointer ffi:reply () :pointer read :pointer reply :void)))))))

(define-effect on-desktop-notification ((notification :pointer)) :void
  (let ((title (gstring (cffi:foreign-slot-pointer
                         notification '(:struct ffi:terminal-desktop-notification) 'ffi:title)))
        (body (gstring (cffi:foreign-slot-pointer
                        notification '(:struct ffi:terminal-desktop-notification) 'ffi:body))))
    (tp:call-host terminal #'tp:host-notify (if (string= title "") nil title) body)))

(define-effect on-color-scheme ((out :pointer)) ffi:bool
  (:default nil)
  (let ((scheme (tp:call-host terminal #'tp:host-color-scheme)))
    (when (member scheme '(:light :dark))
      (setf (cffi:mem-ref out 'ffi:color-scheme) scheme)
      t)))

(define-effect on-semantic-prompt ((event :pointer)) :void
  (let ((kind (cffi:foreign-slot-value event '(:struct ffi:terminal-semantic-prompt)
                                       'ffi:kind)))
    (when (member kind '(:prompt-start :input-start :output-start :command-end))
      (tp:call-host terminal #'tp:host-prompt-mark kind))))

(defparameter +effects+
  '((:write-pty on-write-pty) (:bell on-bell) (:title-changed on-title-changed)
    (:pwd-changed on-pwd-changed) (:clipboard-write on-clipboard-write)
    (:clipboard-read on-clipboard-read)
    (:desktop-notification on-desktop-notification)
    (:color-scheme on-color-scheme) (:semantic-prompt on-semantic-prompt))
  "terminal option → callback.")

(defun install-effects (handle id)
  (check (ffi:terminal-set handle :userdata (cffi:make-pointer id)) :userdata)
  (loop for (option callback) in +effects+
        do (check (ffi:terminal-set handle option (cffi:get-callback callback)) option)))

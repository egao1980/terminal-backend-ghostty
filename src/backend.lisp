(in-package #:terminal-backend-ghostty)

(defclass ghostty-backend (tp:terminal-backend) ()
  (:documentation "terminal-protocol backend on libghostty-vt."))

(defparameter +features+
  '(:scrollback :alternate-screen :kitty-keyboard :kitty-graphics :kitty-zlib
    :osc-52 :osc-133
    :format-vt :format-html))

(defmethod tp:backend-supports-p ((backend ghostty-backend) feature)
  (and (member feature +features+) t))

(defun check (result function &optional terminal)
  "Map a GhosttyResult to protocol conditions. → RESULT on :SUCCESS."
  (case result
    (:success result)
    (:out-of-memory (error 'tp:terminal-out-of-memory :terminal terminal))
    (:unsupported (tp:unsupported function terminal))
    (:invalid-value (error 'tp:terminal-invalid-argument
                           :name function :value result :terminal terminal))
    (t (error 'gv:ghostty-vt-error :result result :function function))))

;;; Loading this system makes ghostty the default backend unless one is set.
(unless tp:*terminal-backend*
  (setf tp:*terminal-backend* (make-instance 'ghostty-backend)))

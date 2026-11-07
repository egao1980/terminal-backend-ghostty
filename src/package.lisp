(defpackage #:terminal-backend-ghostty
  (:use #:cl)
  (:local-nicknames (#:tp #:terminal-protocol)
                    (#:gv #:ghostty-vt)
                    (#:ffi #:ghostty-vt.ffi))
  (:export #:ghostty-backend
           #:ghostty-terminal
           #:ghostty-terminal-handle))

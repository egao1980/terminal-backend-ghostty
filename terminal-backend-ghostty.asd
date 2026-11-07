(defsystem "terminal-backend-ghostty"
  :version "0.1.0"
  :description "terminal-protocol backend on libghostty-vt 1.0 (via ghostty-vt)"
  :author "egao1980"
  :license "MIT"
  :depends-on ("terminal-protocol" "ghostty-vt" "cffi" "trivial-garbage")
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "backend")
               (:file "callbacks")
               (:file "terminal")
               (:file "frame")
               (:file "input"))
  :in-order-to ((test-op (test-op "terminal-backend-ghostty/tests"))))

(defsystem "terminal-backend-ghostty/tests"
  :depends-on ("terminal-backend-ghostty" "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "backend-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))

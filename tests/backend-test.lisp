(in-package #:terminal-backend-ghostty/tests)

(defun octets (string)
  (map '(simple-array (unsigned-byte 8) (*)) #'char-code string))

(defun esc (control)
  "CONTROL with ^ as ESC and ` as BEL, as octets."
  (octets (substitute (code-char 7) #\` (substitute #\Esc #\^ control))))

(defun babel-free-utf8 (string)
  (cffi:with-foreign-string ((p n) string :encoding :utf-8 :null-terminated-p nil)
    (let ((out (make-array n :element-type '(unsigned-byte 8))))
      (dotimes (i n out) (setf (aref out i) (cffi:mem-aref p :uint8 i))))))

(defmacro with-ghostty ((var &rest keys) &body body)
  `(let ((tp:*terminal-backend* (make-instance 'tg:ghostty-backend)))
     (tp:with-terminal (,var ,@keys) ,@body)))

;;; Test hosts

(defclass title-host (tp:terminal-host)
  ((titles :initform '() :accessor host-titles)))

(defmethod tp:host-title-changed ((h title-host) term title)
  (declare (ignore term))
  (push title (host-titles h)))

(defclass angry-host (tp:terminal-host) ())

(defmethod tp:host-bell ((h angry-host) term)
  (declare (ignore term))
  (error "no bells"))

(defclass echo-host (tp:terminal-host) ())

(defmethod tp:host-bell ((h echo-host) term)
  (tp:terminal-feed term (octets "x")))

(deftest loading-sets-default-backend
  (ok (typep tp:*terminal-backend* 'tp:terminal-backend))
  (ok (tp:backend-supports-p (make-instance 'tg:ghostty-backend) :kitty-keyboard)))

(deftest hello-world
  (with-ghostty (term :cols 20 :rows 3)
    (tp:terminal-feed term (esc "Hello, ^[1;32mworld^[0m!"))
    (ok (equal (tp:terminal-text term) "Hello, world!"))
    (let* ((snap (tp:terminal-snapshot term))
           (row (aref (getf snap :cells) 0)))
      (ok (equal (first (getf snap :lines)) "Hello, world!"))
      (ok (= 20 (length row)))
      (ok (tp:style-bold (tp:cell-style (aref row 7))))
      (ok (typep (tp:style-foreground (tp:cell-style (aref row 7))) 'tp:rgb))
      (ok (eq :default (tp:style-foreground (tp:cell-style (aref row 0)))))
      (ng (tp:style-bold (tp:cell-style (aref row 12)))))))

(deftest cursor-and-size
  (with-ghostty (term :cols 10 :rows 4)
    (tp:terminal-feed term (octets (format nil "ab~C~Ccd" #\Return #\Newline)))
    (let ((cursor (tp:terminal-cursor term)))
      (ok (= 2 (tp:cursor-x cursor)))
      (ok (= 1 (tp:cursor-y cursor))))
    (ok (equal (multiple-value-list (tp:terminal-size term)) '(10 4)))
    (tp:terminal-resize term 30 6)
    (ok (equal (multiple-value-list (tp:terminal-size term)) '(30 6)))))

(deftest split-sequences-resume
  (with-ghostty (term :cols 10 :rows 2)
    (let ((bytes (esc "^[1mX^[0mY")))
      (loop for b across bytes
            do (tp:terminal-feed term (make-array 1 :element-type '(unsigned-byte 8)
                                                    :initial-element b))))
    (ok (equal (tp:terminal-text term) "XY"))))

(deftest wide-characters
  (with-ghostty (term :cols 6 :rows 1)
    (tp:terminal-feed term (babel-free-utf8 "日x"))
    (let ((row (aref (getf (tp:terminal-snapshot term) :cells) 0)))
      (ok (eq :wide (tp:cell-width (aref row 0))))
      (ok (equal "日" (tp:cell-text (aref row 0))))
      (ok (eq :spacer (tp:cell-width (aref row 1))))
      (ok (equal "x" (tp:cell-text (aref row 2)))))))

(deftest title-and-pwd
  (let ((host (make-instance 'title-host)))
    (with-ghostty (term :host host)
      (tp:terminal-feed term (esc "^]2;hello`"))
      (ok (equal (tp:terminal-title term) "hello"))
      (ok (equal (host-titles host) '("hello")))
      (ng (tp:terminal-pwd term)))))

(deftest replies-reach-buffering-host
  (with-ghostty (term)
    (tp:terminal-feed term (esc "^[6n"))   ; DSR cursor position
    (ok (equalp (tp:terminal-take-replies term) (esc "^[1;1R")))))

(deftest bell-host-error-is-deferred
  (with-ghostty (term)
    (tp:with-terminal (t2 :host (make-instance 'angry-host))
      (ok (signals (tp:terminal-feed t2 (octets (string (code-char 7))))
                   'tp:terminal-host-error))
      ;; The parser survived and still works.
      (tp:terminal-feed t2 (octets "ok"))
      (ok (equal (tp:terminal-text t2) "ok")))))

(deftest reentrant-feed-refused
  (with-ghostty (term :host (make-instance 'echo-host))
    (let ((c (handler-case (tp:terminal-feed term (octets (format nil "a~Cb" (code-char 7))))
               (tp:terminal-host-error (c) c))))
      (ok (typep (tp:terminal-host-error-cause c) 'tp:terminal-reentrant-feed)))
    (ok (equal (tp:terminal-text term) "ab"))))

(deftest modes
  (with-ghostty (term)
    (ng (tp:terminal-mode term :bracketed-paste))
    (tp:terminal-feed term (esc "^[?2004h"))
    (ok (tp:terminal-mode term :bracketed-paste))
    (setf (tp:terminal-mode term :bracketed-paste) nil)
    (ng (tp:terminal-mode term '(:dec 2004)))
    (ok (tp:terminal-mode term :cursor-visible))
    (ok (eq :none (tp:terminal-mouse-tracking term)))
    (tp:terminal-feed term (esc "^[?1002h"))
    (ok (eq :button (tp:terminal-mouse-tracking term)))))

(deftest alternate-screen
  (with-ghostty (term :cols 10 :rows 2)
    (tp:terminal-feed term (octets "main"))
    (tp:terminal-feed term (esc "^[?1049h"))
    (ok (eq :alternate (tp:terminal-active-screen term)))
    (ok (equal (tp:terminal-text term) ""))
    (tp:terminal-feed term (esc "^[?1049l"))
    (ok (eq :primary (tp:terminal-active-screen term)))
    (ok (equal (tp:terminal-text term) "main"))))

(deftest key-encoding
  (with-ghostty (term)
    (ok (equalp (tp:encode-key term :a :text "a") (octets "a")))
    (ok (equalp (tp:encode-key term :enter) (octets (string #\Return))))
    (ok (equalp (tp:encode-key term :arrow-up) (esc "^[A")))
    (tp:terminal-feed term (esc "^[?1h"))          ; DECCKM
    (ok (equalp (tp:encode-key term :arrow-up) (esc "^OA")))
    (ok (equalp (tp:encode-key term :c :text "c" :modifiers '(:ctrl))
                (octets (string (code-char 3)))))))

(deftest mouse-encoding
  (with-ghostty (term)
    (ok (equalp (tp:encode-mouse term :left :press 0 0)
                (make-array 0 :element-type '(unsigned-byte 8))))
    (tp:terminal-feed term (esc "^[?1000h^[?1006h"))
    (ok (equalp (tp:encode-mouse term :left :press 4 2) (esc "^[<0;5;3M")))
    (ok (equalp (tp:encode-mouse term :left :release 4 2) (esc "^[<0;5;3m")))))

(deftest focus-and-paste
  (with-ghostty (term)
    (ok (zerop (length (tp:encode-focus term t))))
    (tp:terminal-feed term (esc "^[?1004h"))
    (ok (equalp (tp:encode-focus term t) (esc "^[I")))
    (ok (equalp (tp:encode-paste term "hi") (octets "hi")))
    (tp:terminal-feed term (esc "^[?2004h"))
    (ok (equalp (tp:encode-paste term "hi") (esc "^[200~hi^[201~")))))

(deftest frames-track-dirty-rows
  (with-ghostty (term :cols 10 :rows 3)
    (tp:with-frame (frame term)
      (tp:frame-mark-clean frame)
      (tp:terminal-feed term (esc "^[2;1Hx"))
      (ok (member (tp:frame-update frame) '(:partial :full)))
      (let ((dirty '()))
        (tp:do-frame-rows (row frame :dirty-only t) (push (tp:row-y row) dirty))
        (ok (member 1 dirty)))
      (ok (typep (tp:frame-colors frame) 'tp:terminal-colors))
      (ok (= 1 (tp:cursor-x (tp:frame-cursor frame)))))))

(deftest text-formats
  (with-ghostty (term :cols 20 :rows 2)
    (tp:terminal-feed term (esc "^[1mB^[0m"))
    (ok (search "B" (tp:terminal-text term :format :vt :region :screen)))
    (ok (search "<" (tp:terminal-text term :format :html :region :screen)))
    (ok (signals (tp:terminal-text term :format :html) 'tp:terminal-unsupported))))

(deftest closed-terminal-frees-and-guards
  (let* ((tp:*terminal-backend* (make-instance 'tg:ghostty-backend))
         (term (tp:make-terminal)))
    (tp:terminal-close term)
    (ok (signals (tp:terminal-feed term (octets "x")) 'tp:terminal-closed-error))
    (tp:terminal-close term)
    (ok (tp:terminal-closed-p term))))

(deftest finalizers-free-unclosed-terminals-and-frames
  (let ((tp:*terminal-backend* (make-instance 'tg:ghostty-backend)))
    (dotimes (i 200)
      (let ((term (tp:make-terminal :cols 20 :rows 5)))
        (tp:terminal-feed term (octets "garbage"))
        (when (evenp i) (tp:make-frame term))
        (when (zerop (mod i 3)) (tp:terminal-close term))))
    (trivial-garbage:gc :full t)
    (trivial-garbage:gc :full t)
    (tp:with-terminal (term)
      (tp:terminal-feed term (octets "still fine"))
      (ok (equal (tp:terminal-text term) "still fine")))))

;;; artilico.asd

(asdf:defsystem #:artilico
  :description "Artistic live-coding in Common Lisp"
  :author "Honix <ted888@ya.ru>"
  :license "cc nc"
  :serial t
  :depends-on (:swank
               :trivial-timers
               :cl-opengl
               :cl-glut
               ;; :cl-glu
               :uiop)
  :components ((:file "package")
               (:file "cffi-fix")
               (:file "utils")
               (:file "code")
               (:file "artilico")))

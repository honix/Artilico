;;; artilico.asd

(asdf:defsystem #:artilico
  :description "Artistic live-coding in Common Lisp"
  :author "Honix <techsnuffle@gmail.com>"
  :license "Unknown"
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

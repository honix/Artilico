(in-package :artilico)

;;
;; Artilico | code
;;

(defparameter *init-code*
  #("(defun draw ()"
    "    (gl:translate 0 0 -2)"
    "    (gl:rotate (art-time *window*) 1 1 1)"
    "    (let* ((time (art-time *window*))"
    "           (pulse (sin (* time 0.1))))"
    "        (loop for i from 1 to 20 do"
    "            (gl:color (/ i 20) 0 pulse)"
    "            (gl:rotate (* time i 0.2) 1.0 0.7 0.3)"
    "            (glut:wire-sphere (+ (/ 5 i) pulse) 12 12))))"))

(defparameter *code-carret-pixels*
  (make-array (* 6 9 4)
              :element-type 'unsigned-byte
              :initial-contents (loop for i below (* 6 9) append
                                     `(255 200 100 ,(random 255)))))

(defclass code ()
  (;; logic
   (code-strings :accessor code-strings
                 :initform *init-code*)
   (code-carret-x :accessor code-carret-x
                  :initform 0)
   (code-carret-y :accessor code-carret-y
                  :initform 0)
   (code-answer :accessor code-answer
                :initform nil)
   (code-answer-shift :accessor code-answer-shift
                      :initform 0)
   ;; timing
   (code-update-timer :accessor code-update-timer)
   (code-update? :accessor code-update?
                 :initform nil)
   ;; graphics
   (code-frame-buffer :accessor code-frame-buffer
                      :initform nil)
   (code-texture :accessor code-texture
                 :initform nil)
   (code-texture-res :accessor code-texture-res
                     :initform 512)))

(defmethod initialize-instance :after ((c code) &rest initargs)
  (declare (ignore initargs))
  (setf (code-update-timer c) (trivial-timers:make-timer
                               (lambda () (setf (code-update? c) t))))
  (reset-timer (code-update-timer c))
  (eval '(in-package :artilico))
  (code-evaluate c))

;;
;; logic
;;

(defmethod code-newline ((c code))
  (with-slots (code-strings (x code-carret-x)
                            (y code-carret-y)) c
    (let* ((str (aref code-strings y))
           (left (subseq str 0 x))
           (right (subseq str x)))
      (incf y)
      (setf x 0
            (aref code-strings (1- y)) left
            code-strings (insert-after code-strings 'vector
                                       y (list right))))))

(defmethod code-insert ((c code) char)
  (with-slots (code-strings (x code-carret-x)
                            (y code-carret-y)) c
    (symbol-macrolet ((str (aref code-strings y)))
      (setf str (insert-after str 'string x (string char)))
      (incf x))))

(defmethod code-backspace ((c code))
  (with-slots (code-strings (x code-carret-x)
                            (y code-carret-y)) c
    (if (> x 0)
        (symbol-macrolet ((str (aref code-strings y)))
          (decf x)
          (setf str (remove-nth str 'string x)))
        (when (> y 0)
          (macrolet ((c-s (s) `(aref (code-strings c) (+ y ,s))))
            (decf y)
            (setf x (length (c-s 0))
                  (c-s 0) (concatenate 'string (c-s 0) (c-s 1))
                  code-strings (remove-nth
                                code-strings 'vector (1+ y))))))))

(defmethod code-delete ((c code))
  (with-slots (code-strings (x code-carret-x)
                            (y code-carret-y)) c
    (symbol-macrolet ((str (aref code-strings y)))
      (if (< y (length str))
          (setf str (concatenate 'string
                                 (subseq str 0 x)
                                 (subseq str (1+ x))))
          (progn (setf x 0)
                 (incf y)
                 (code-backspace c))))))

(defmethod code-evaluate ((c code))
  (set-answer
   c
   (princ-to-string
    (handler-case (eval (read-from-string
                         (reduce (lambda (x y)
                                   (concatenate 'string x y))
                                 (code-strings c))))
      (error (er) (code-answer c) er)))))

(defmethod set-answer ((c code) answ)
  (setf (code-answer c)	answ
        (code-answer-shift c) (gap-by-pixels
                               c
                               (* (count #\Newline answ) 13)))
  (reset-timer (code-update-timer c)))

(defun reset-timer (timer)
  (trivial-timers:schedule-timer timer
                                 0 :repeat-interval 1))

(defmethod code-keyboard ((c code) key)
  (case key
    (#\Return (code-newline c))
    (#\Backspace (code-backspace c))
    (#\Rubout (code-delete c))
    (#\Tab (dotimes (i 4) (code-insert c " ")))
    (t (code-insert c key)))
  (reset-timer (code-update-timer c)))

(defmethod code-special ((c code) key)
  (with-slots (code-strings (x code-carret-x)
                            (y code-carret-y)
                            code-update-timer) c
    (case key
      (:key-home (setf x 0))
      (:key-end (setf x (length (aref code-strings y))))
      (:key-left (setf x (max 0 (1- x))))
      (:key-right (setf x (min (1+ x) (length (aref code-strings y)))))
      (:key-up (setf y (max 0 (1- y)))
               (setf x (min x (length (aref code-strings y)))))
      (:key-down (if (< y (1- (length code-strings)))
                     (incf y)
                     (setf x (length (aref code-strings y))))
                 (setf x (min x (length (aref code-strings y))))))
    (reset-timer code-update-timer)))

;;
;; graphics
;;

(defmethod code-update ((c code))
  (when (code-update? c)
    (setf (code-update? c) nil)
    (code-draw-buffer c)))

(defmethod code-gen-framebuffer ((c code))
  (when (code-frame-buffer c)
    (gl:delete-framebuffers-ext (list (code-frame-buffer c))))
  (when (code-texture c)
    (gl:delete-textures (list (code-texture c))))
  (let ((fb (first (gl:gen-framebuffers-ext 1)))
        (tx (first (gl:gen-textures 1))))
    (gl:bind-framebuffer-ext :framebuffer-ext fb)
    (gl:bind-texture :texture-2d tx)
    (gl:tex-parameter :texture-2d :texture-min-filter :nearest)
    (gl:tex-parameter :texture-2d :texture-mag-filter :nearest)
    (gl:tex-image-2d :texture-2d 0 :rgba
                     (code-texture-res c) (code-texture-res c) 0 :rgba
                     :unsigned-byte (cffi:null-pointer))
    (gl:bind-texture :texture-2d 0)
    (gl:framebuffer-texture-2d-ext :framebuffer-ext
                                   :color-attachment0-ext
                                   :texture-2d
                                   tx
                                   0)
    (setf (code-frame-buffer c) fb
          (code-texture c) tx)))

(defmethod gap-by-pixels ((c code) pixels)
  (* (/ pixels (code-texture-res c)) 2))

(defmethod draw-carret ((c code) row line)
  (with-slots ((x code-carret-x) (y code-carret-y)) c
    (gl:raster-pos (+ row (gap-by-pixels c (* x 8)))
                   (+ line (gap-by-pixels c (* y -13))))
    (gl:draw-pixels 6 9 :rgba :unsigned-byte
                    *code-carret-pixels*)))

(defmethod draw-chars ((c code) row line gap)
  (with-slots ((strs code-strings)
               (cx code-carret-x)
               (yy code-carret-y)) c
    (gl:raster-pos row line)
    (loop
       for y to (1- (length strs))
       as str = (aref strs y) do
         (loop
            for x to (1- (length str))
            as char = (aref str x) do
              (cond
                ((char= char #\Return) ())
                ((char= char #\Tab)
                 (bitmap-str glut:+bitmap-8-by-13+ "    "))
                (t (glut:bitmap-character
                    glut:+bitmap-8-by-13+ (char-code char)))))
         (gl:raster-pos -0.9 (decf line gap)))))

(defmethod code-draw-buffer ((c code))
  (with-slots (code-strings (x code-carret-x)
                            (y code-carret-y)) c
    ;; setup
    (gl:bind-framebuffer-ext :framebuffer-ext (code-frame-buffer c))
    (gl:disable :blend :texture-2d)
    (gl:viewport 0 0 (code-texture-res c) (code-texture-res c))
    (gl:matrix-mode :projection)
    (gl:load-identity)
    (gl:ortho -1 1 -1 1 0 1)
    (gl:matrix-mode :modelview)
    ;; draw
    (gl:clear-color 0 0 0 0)
    (gl:clear :color-buffer)
    (gl:load-identity)
    ;; status line
    (gl:color 0.5 0.5 1)
    (gl:raster-pos -0.9 (+ -0.95 (code-answer-shift c)))
    (bitmap-str glut:+bitmap-8-by-13+ (code-answer c))
    ;; fps
    (gl:color 1 1 1 0.5)
    (gl:raster-pos 0.7 -0.95)
    (bitmap-str glut:+bitmap-8-by-13+ (format nil "fps: ~d"
                                              (fps *window*)))
    (gl:color 1 1 1)
    (let ((row -0.9)
          (line 0.8)
          (gap (gap-by-pixels c 13)))
      (draw-carret c row line)
      (draw-chars c row line gap))))

(defun bitmap-str (font str)
  #-darwin
  (glut:bitmap-string font str)
  #+darwin
  (loop :for c :across str :do
     (glut:bitmap-character font (char-code c))))

(defmethod code-draw ((c code))
  (labels ((draw-quad ()
             (gl:with-primitive :quads
               (gl:tex-coord 0 0)
               (gl:vertex -1.0 -1.0)
               (gl:tex-coord 1 0)
               (gl:vertex 1.0 -1.0)
               (gl:tex-coord 1 1)
               (gl:vertex 1.0 1.0)
               (gl:tex-coord 0 1)
               (gl:vertex -1.0 1.0))))
    (gl:disable :depth-test :lighting)
    (gl:enable :blend :texture-2d)
    (gl:blend-func :src-alpha :one-minus-src-alpha)
    (gl:bind-texture :texture-2d (code-texture c))
    (gl:load-identity)
    (gl:translate 0 0 -1)
    (gl:color 0 0 0 0.9) ; text-shadow
    (draw-quad)
    (gl:translate 0 (gap-by-pixels c 1) 0)
    (gl:color 1 1 1 0.9) ; text
    (draw-quad)))

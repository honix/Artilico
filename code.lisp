(in-package :artilico)

;;
;; Artilico | code
;;

(defparameter *init-code*
  #(""
    "(defun draw ()"
    "    (gl:translate 0 0 -2)"
    "    (gl:rotate (art-time *window*) 1 1 1)"
    "    (let* ((time (art-time *window*))"
    "           (pulse (sin (* time 0.1))))"
    "        (loop for i from 1 to 10 do"
    "            (gl:color (/ i 20) 0 pulse)"
    "            (gl:rotate (* time i 0.2) 1.0 0.7 0.3)"
    "            (glut:wire-sphere (+ (/ 5 i) pulse) 12 12))))"))

(defparameter *code-carret-width* 8)
(defparameter *code-carret-height* 12)

(defparameter *code-carret-pixels*
  (make-array (* *code-carret-width* *code-carret-height* 4)
              :element-type
              'unsigned-byte
              :initial-contents
              (loop for i below (* *code-carret-width*
                                   *code-carret-height*)
                 append `(255 200 100 170))))

(defclass code ()
  (;; logic
   (code-strings      :accessor code-strings
                      :initform *init-code*)
   (code-carret-x     :accessor code-carret-x
                      :initform 0)
   (code-carret-y     :accessor code-carret-y
                      :initform 0)
   (code-selection    :accessor code-selection
                      :initform '((0 0) (0 0)))
   (code-answer       :accessor code-answer
                      :initform nil)
   (code-answer-shift :accessor code-answer-shift
                      :initform 0)
   ;; timing
   (code-update? :accessor code-update?
                 :initform t)
   ;; graphics
   (code-frame-buffer :accessor code-frame-buffer
                      :initform nil)
   (code-texture      :accessor code-texture
                      :initform nil)
   (code-texture-res  :accessor code-texture-res
                      :initform 512)))

(defmethod initialize-instance :after ((c code) &rest initargs)
  (declare (ignore initargs))
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
  (setf (code-update? c) t))

(defmethod code-set-end-selection ((c code) char-x char-y)
  (with-slots ((s code-selection)) c
    (if (and (<= char-y (second (first s)))
             (<= char-x (first (first s))))
        (setf (first (first s)) char-x
              (second (first s)) char-y)
        (setf (first (second s)) char-x
              (second (second s)) char-y))))

(defmethod code-mouse ((c code) button state x y)
  (with-slots (code-strings
	       code-carret-x code-carret-y
               code-selection) c
    (let* ((c-x (floor (/ x 8)))
	   (c-y (floor (/ y 13)))
	   (char-y
	    (max 0 (min c-y (1- (length code-strings)))))
	   (char-x
	    (max 0 (min c-x (length (aref code-strings char-y))))))
      (setf code-carret-x char-x
	    code-carret-y char-y)
      (cond ((eq state :down)
             (setf (first code-selection) (list char-x char-y)
                   (second code-selection) (list char-x char-y)))
            ((or (eq state :up))
             (code-set-end-selection c char-x char-y)))
      (setf (code-update? c) t))))

(defmethod code-drag ((c code) x y)
  (with-slots (code-strings code-selection) c
    (let* ((c-x (floor (/ x 8)))
           (c-y (floor (/ y 13)))
           (char-y
            (max 0 (min c-y (1- (length code-strings)))))
           (char-x
            (max 0 (min c-x (length (aref code-strings char-y))))))
      (code-set-end-selection c char-x char-y)
      (setf (code-update? c) t))))

(defmethod code-keyboard ((c code) key)
  (case key
    (#\Return (code-newline c))
    (#\Backspace (code-backspace c))
    (#\Rubout (code-delete c))
    (#\Tab (dotimes (i 4) (code-insert c " ")))
    (t (code-insert c key)))
  (setf (code-update? c) t))

(defmethod code-special ((c code) key)
  (with-slots (code-strings (x code-carret-x)
                            (y code-carret-y)) c
    (case key
      (:key-home
       (setf x 0))
      (:key-end
       (setf x (length (aref code-strings y))))
      (:key-left
       (setf x (max 0 (1- x))))
      (:key-right
       (setf x (min (1+ x) (length (aref code-strings y)))))
      (:key-up
       (setf y (max 0 (1- y)))
       (setf x (min x (length (aref code-strings y)))))
      (:key-down
       (if (< y (1- (length code-strings)))
	   (incf y)
	   (setf x (length (aref code-strings y))))
       (setf x (min x (length (aref code-strings y))))))
    (setf (code-update? c) t)))

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

(defmethod draw-mark ((c code) row line x y)
  (gl:raster-pos (+ row (gap-by-pixels c (* x 8)))
                 (+ line (gap-by-pixels c (* y -13))))
  (gl:draw-pixels *code-carret-width*
                  *code-carret-height*
                  :rgba :unsigned-byte
                  *code-carret-pixels*))

(defmethod draw-selection ((c code) row line gap)
  (destructuring-bind ((start-x start-y) (end-x end-y))
      (code-selection c)
    (if (not (= start-y end-y))
        (progn
          (loop for x from start-x to 64 do
               (draw-mark c row line x start-y))
          (loop for y from (1+ start-y) to (1- end-y) do
               (loop for x from 0 to 64 do
                    (draw-mark c row line x y)))
          (loop for x from 0 to end-x do
               (draw-mark c row line x end-y)))
        (progn
          (loop for x from start-x to end-x do
               (draw-mark c row line x start-y)))))

  (defmethod draw-carret ((c code) row line)
    (with-slots ((x code-carret-x) (y code-carret-y)) c
      (gl:raster-pos (+ row (gap-by-pixels c (* x 8)))
                     (+ line (gap-by-pixels c (* y -13))))
      (gl:draw-pixels *code-carret-width*
                      *code-carret-height*
                      :rgba :unsigned-byte
                      *code-carret-pixels*))))

(defmethod draw-chars ((c code) row line gap)
  (with-slots ((strs code-strings)
               (cx code-carret-x)
               (yy code-carret-y)) c
    (gl:raster-pos row line)
    (loop for str across strs do
         (loop for char across str do
              (cond
                ((char= char #\Return) ())
                (t (glut:bitmap-character
                    glut:+bitmap-8-by-13+ (char-code char)))))
         (gl:raster-pos -1 (decf line gap)))))

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
    ;; reset
    (gl:clear-color 0 0 0 0)
    (gl:clear :color-buffer)
    (gl:load-identity)
    ;; status line
    (gl:color 0.5 0.5 1)
    (gl:raster-pos -0.9 (+ -0.95 (code-answer-shift c)))
    (bitmap-str glut:+bitmap-8-by-13+ (code-answer c))
    ;; code
    (gl:color 1 1 1)
    (let ((row -1)
          (line (- 1 (gap-by-pixels c 13)))
          (gap (gap-by-pixels c 13)))
      (draw-selection c row line gap)
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
    (gl:translate (- 1 (aspect-ratio *window*)) 0 -1)
    (gl:color 0 0 0 0.9) ; text-shadow
    (draw-quad)
    (gl:translate 0 (gap-by-pixels c 1) 0)
    (gl:color 1 1 1 0.9) ; text
    (draw-quad)))


;;
;; Artilico | artilico-window
;;

;; There is some commented blocks,
;; gpu buffer and shadows are in plans
;; but little later.

(ql:quickload '(:swank :trivial-timers
		:cl-opengl :cl-glut)) ; :cl-glu

(load "cffi-fix.lisp")
(load "utils.lisp")
(load "code.lisp")

(in-package :cl-user)

;(defparameter *count* 65534)
;(defparameter *count* 4000)
(defparameter *window* nil)

;; (gl:define-gl-array-format position-color
;;   (gl:vertex :type :float :components (x y z))
;;   (gl:color :type :unsigned-char :components (r g b)))


(defclass artilico-window (glut:window)
  (;; time
   (art-time :accessor art-time :initform 0.0)
   (fps :accessor fps :initform 0)
   ;; ;; gpu-arrays
   ;; (vertex-array :accessor vertex-array
   ;;               :initform (gl:alloc-gl-array
   ;;                          'position-color *count*))
   ;; (indices-array :accessor indices-array
   ;;                :initform (gl:alloc-gl-array
   ;;                           :unsigned-short (* *count* 2)))
   ;; (shadow-buffer :accessor shadow-buffer)
   ;; (shadow-texture :accessor shadow-texture)
   ;; code
   (code :accessor code
	 :initform (make-instance 'code)))
  
  (:default-initargs :width 512 :height 512 :pos-x 100 :pos-y 100
                     :mode '(:double :depth :rgb :multisample)
		     :tick-interval 15 :title "artilico"))

;;
;; timing
;;

(let ((last-frame-time 0)
      (last-fps-time 0)
      (temp-fps 0))
  (defmethod update-times ((w artilico-window))
    (let* ((new-time (get-internal-real-time))
           (delta (- new-time last-frame-time)))
      (incf (art-time w) (* delta 0.01))
      (when (> (- new-time last-fps-time) 1000)
        (setf (fps w) temp-fps
              temp-fps 0
              last-fps-time new-time))
      (setf last-frame-time new-time)
      (incf temp-fps))))


;; (defmethod load-gpu-arrays ((w artilico-window))
;;   (dotimes (i *count*)
;;     ;; verticesx
;;     (setf (gl:glaref (vertex-array w) i 'x) (rand-from-to -1.0 1.0))
;;     (setf (gl:glaref (vertex-array w) i 'y) (rand-from-to -1.0 1.0))
;;     (setf (gl:glaref (vertex-array w) i 'z) (rand-from-to -1.0 1.0))
;;     ;; indices
;;     (setf (gl:glaref (indices-array w) (* 2 i)) i)
;;     (setf (gl:glaref (indices-array w) (1+ (* 2 i))) (- *count* i))
;;     ;; colors
;;     (setf (gl:glaref (vertex-array w) i 'r) (floor (random 255)))
;;     (setf (gl:glaref (vertex-array w) i 'g) (floor (random 255)))
;;     (setf (gl:glaref (vertex-array w) i 'b) (floor (random 255)))
;;     ;; (setf (gl:glaref (vertex-array w) i 'a) 255)
;;     ))


;;
;; glut events
;;

(defmethod glut:display-window :before ((w artilico-window))
  (gl:clear-color 0 0 0 1)

  ;; (setf (shadow-buffer w) (first (gl:gen-framebuffers-ext 1))
  ;; 	(shadow-texture w) (first (gl:gen-textures 1)))
  ;; (gl:bind-framebuffer-ext :framebuffer-ext (shadow-buffer w))
  ;; (gl:bind-texture :texture-2d (shadow-texture w))
  ;; (gl:tex-image-2d :texture-2d 0 :depth-component
  ;; 		   512 512 0 :depth-component :unsigned-byte
  ;; 		   (cffi:null-pointer))
  ;; (gl:framebuffer-texture-2d-ext :framebuffer-ext
  ;; 				 :depth-attachment-ext
  ;; 				 :texture-2d
  ;; 				 (shadow-texture w) 0)

  
  ;;(load-gpu-arrays w)
  )

(let ((ctrl nil))
  (defmethod glut:special ((w artilico-window) key x y)
    (when (eq key :key-left-ctrl) (setf ctrl t))
    (code-special (code w) key))

  (defmethod glut:special-up ((w artilico-window) key x y)
    (when (eq key :key-left-ctrl) (setf ctrl nil)))

  (defmethod glut:keyboard ((w artilico-window) key x y)
    (if (and ctrl (eq key #\Return))
	(code-evaluate (code w))
	(code-keyboard (code w) key))))

(defmethod glut:motion ((w artilico-window) x y)
  (format t "~d ~d~%" x y))

(defmethod glut:reshape ((w artilico-window) width height)
  (setf (glut:width w) width
        (glut:height w) height
	(code-texture-res (code w)) (+ (mod width 2)
				       (mod height 2)
				       (/ height 1)))
  (code-gen-framebuffer (code w))
  (reset-timer (code-update-timer (code w)))
  (gl:bind-framebuffer-ext :framebuffer-ext 0)
  ; turn clear off for videocard shit at resizing
  (gl:clear :color-buffer :depth-buffer)
  (gl:viewport 0 0 width height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (let* ((aspect (/ width height))
         (norma 0.1)
         (sized (* norma aspect)))
    (gl:frustum (- sized) sized (- norma) norma 0.1 100))
  (gl:matrix-mode :modelview)
  (gl:load-identity))

(defmethod glut:close ((w artilico-window))
  ;; (gl:free-gl-array (vertex-array w))
  ;; (gl:free-gl-array (indices-array w))
  )

;;
;; drawing
;;

(defun setup-draw-graphics (w)
  (gl:bind-framebuffer-ext :framebuffer-ext 0)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:viewport 0 0 (glut:width w) (glut:height w))
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (let* ((aspect (/ (glut:width w) (glut:height w)))
         (norma 0.1)
         (sized (* norma aspect)))
    (gl:frustum (- sized) sized (- norma) norma 0.1 100))
  (gl:matrix-mode :modelview))

(defun draw ())		; user function

(defun draw-graphics (w)
  (gl:enable :depth-test)  
  (gl:disable :texture-2d)
  (gl:clear-color 0 0 0 1)
  (gl:color 0.5 0.5 0.5)
  (gl:clear :color-buffer :depth-buffer)
  (gl:load-identity)

  (handler-case (draw)
    (error (er) (set-answer (code w) (princ-to-string er))))

  ;; (gl:translate 0.5 0 -3)
  ;; (gl:rotate (art-time w) 1 1 1)
  
  ;; (gl:enable-client-state :vertex-array)
  ;; (gl:enable-client-state :color-array)
  ;; (gl:bind-gl-vertex-array (vertex-array w))
  ;; (if (> (mod (art-time w) 60) 30)
  ;; (gl:draw-elements :lines (indices-array w))
  ;; (gl:draw-elements :triangles (indices-array w)))
  )

;;
;;
;;

(defmethod glut:display ((w artilico-window))
  (glut:tick w))

(defmethod update ((w artilico-window))
  (update-times w)

  (code-update (code w))
  
  (setup-draw-graphics w)
  (draw-graphics w)
  
  (code-draw (code w)))

(defmethod glut:tick ((w artilico-window))
  (update w)
  (glut:swap-buffers))

;;
;; gogogo
;; this thing doesnt runs on windows with slime-session

(sb-thread:make-thread
 (lambda () (glut:display-window (setf *window* (make-instance 'artilico-window))))
 :name "gl-thread")

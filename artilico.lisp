(in-package :artilico)

;;
;; Artilico | artilico-window
;;

(defparameter *window* nil)

(defclass artilico-window (glut:window)
  (;; time
   (art-time :accessor art-time :initform 0.0)
   (fps :accessor fps :initform 0)
   ;; code
   (code :accessor code
         :initform (make-instance 'code))
   ;; graphics
   (aspect-ratio :accessor aspect-ratio
		 :initform 1.0))
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

;;
;; glut events
;;

(defmethod glut:display-window :before ((w artilico-window))
  (gl:clear-color 0 0 0 1))

(let ((ctrl nil))
  (defmethod glut:special ((w artilico-window) key x y)
    (when (eq key :key-left-ctrl) (setf ctrl t))
    (code-special (code w) key))

  (defmethod glut:special-up ((w artilico-window) key x y)
    (when (eq key :key-left-ctrl) (setf ctrl nil)))

  (defmethod glut:keyboard ((w artilico-window) key x y)
    (if (and ctrl (or (eq key #\Return) (eq key #\Newline)))
        (code-evaluate (code w))
        (code-keyboard (code w) key)))

  (defmethod glut:mouse ((w artilico-window) button
			 state x y)
    (code-mouse (code w) button state x y))

  (defmethod glut:motion ((w artilico-window) x y)
    ;(write (list x y))
    ))

(defmethod glut:reshape ((w artilico-window) width height)
  (setf (glut:width w) width
        (glut:height w) height
        (code-texture-res (code w)) (+ (mod width 2)
                                       (mod height 2)
                                       (/ height 1)))
  (setf (aspect-ratio w) (/ width height))
  (code-gen-framebuffer (code w))
  (gl:bind-framebuffer-ext :framebuffer-ext 0)
  ;; turn clear off for videocard shit at resizing
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
  '())

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
    (error (er) (set-answer (code w) (princ-to-string er)))))

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

(defun gogogo ()
  (sb-thread:make-thread
   (lambda () (glut:display-window (setf *window* (make-instance 'artilico-window))))
   :name "gl-thread"))

(defun run-no-thread ()
  (glut:display-window (setf *window* (make-instance 'artilico-window))))

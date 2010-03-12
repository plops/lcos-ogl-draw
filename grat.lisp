#.(progn (require :asdf)
 (asdf:operate 'asdf:load-op :cl-opengl)
 (asdf:operate 'asdf:load-op :cl-glut)
 (asdf:operate 'asdf:load-op :cl-glu))

(declaim (optimize (speed 3) (safety 2) (debug 2)))

(defpackage :g
  (:shadowing-import-from :cl close get special)
  (:use :cl :gl :glut))

(in-package :g)

(defclass bild ()
  ((width  :accessor width :initarg :width :type fixnum :initform 1920)
   (height :accessor height :initarg :height :type fixnum :initform 1080)
   (texture-target :accessor texture-target :initform :texture-rectangle-nv :type fixnum)
   (texture-object :accessor texture-object :type fixnum :initform -1)
   (internal-format :accessor internal-format :initform :luminance16 :type fixnum)
   (data-format :accessor data-format :initform :luminance :type fixnum)
   (data-type :accessor data-type :initform :unsigned-byte :type fixnum)))

(defmethod initialize-instance :after ((img bild) &key)
  (with-slots ((w width)
               (h height)
               (dat data)
               (obj texture-object)
               (int-format internal-format)
               (format data-format)
               (type data-type)
               (target texture-target))
      img
    (format t "initializing bild object with size ~ax~a~&" w h)
    (setf obj (first (gen-textures 1)))
    (bind-texture target obj)
    (tex-parameter target :texture-mag-filter :linear)
    (tex-parameter target :texture-min-filter :linear)
    ;    (pixel-store :unpack-swap-bytes 1) ;; ACTIVATE when data is big-endian
    (tex-image-2d target 0 int-format w h 0 format
		  type (cffi:null-pointer)))) 

(defmethod draw ((self bild) &optional
		 (x 0f0) (y 0f0) 
		 (ww 1920f0) (hh 1080f0))
  (declare (single-float x y ww hh))
  (with-slots ((w width)
               (h height)
               (obj texture-object)
               (target texture-target))
      self
    (bind-texture target obj)
    (enable target)
    (color 1 1 1)
    (let ((q 1 #+nil(/ h w)))
     (with-primitive :quads
       (tex-coord 0 0)(vertex x y)
       (tex-coord w 0)(vertex ww y)
       (tex-coord w h)(vertex ww (* q hh))
       (tex-coord 0 h)(vertex x (* q hh))))
    (disable target)))

(defmethod destructor ((self bild))
  (with-slots ((obj texture-object))
      self
    (delete-textures (list obj))))

(defparameter *grating*
  (make-array 1920 :element-type '(unsigned-byte 8)))

(declaim (ftype (function ((simple-array (unsigned-byte 8) *)
			   double-float)
			  (values null &optional))
		fill-grating))

(defun fill-grating (a freq)
  (dotimes (i (length a))
    (let ((q (* i (/ 1d0 (length a)))))
      (setf (aref a i)
	    (truncate (+ 50 (* 55 (1+ (sin (* 2 pi q freq)))))))))
  nil)


(defmethod update ((self bild))
   (with-slots ((w width)
		(h height)
		(obj texture-object)
		(format data-format)
		(type data-type)
		(target texture-target))
       self
     (bind-texture target obj)
     (tex-sub-image-2d 
      target 0 0 0 w 1
      format type 
      (sb-sys:vector-sap
       (sb-ext:array-storage-vector
	*grating*)))))

(defparameter *bild* nil)

(defmethod reshape ((win window) w h)
  (viewport 0 0 w h)
  (matrix-mode :projection)
  (load-identity)
  (ortho 0 w 0 h -1 1)
  (matrix-mode :modelview)
  (post-redisplay))

(defmethod display-window :before ((w window))
  (setf *bild* (make-instance 'bild :height 1))
  (reshape w 1920 1080))

(defmethod keyboard ((w window) key x y)
  (case key
    (#\Esc (destroy-current-window)
	   (destructor *bild*))))

(declaim (ftype (function (double-float double-float double-float)
			  (values null &optional))
		draw-disk))
(defun draw-disk (radius center-x center-y)
  (with-primitive :triangle-fan
    (vertex center-x center-y 0d0)
    (let ((n 37))
      (dotimes (i (1+ n))
	(let* ((arg (* 2d0 pi i (/ 1d0 n)))
	       (c (cos arg))
	       (s (sin arg))
	       (x (+ (* radius c) center-x))
	       (y (+ (* radius s) center-y)))
	  (vertex x y 0d0))))))

(let ((cnt 0d0))
 (defmethod display ((w window))
   (let* ((w0 (* 540d0 (exp (complex 0d0 (/ pi 4d0)))))
	  (r 260d0)
	  (psi 270d0)
	  (w (* r (exp (complex 0d0 (* psi (/ pi 180d0))))))
	  (z (+ w w0)))
     (clear-stencil 0)
   (clear :color-buffer-bit :stencil-buffer-bit)
   (load-identity)
   ;; http://www.swiftless.com/tutorials/opengl/basic_reflection.html
   (color-mask :false :false :false :false)
   (depth-mask :false)
   (enable :stencil-test)
   (stencil-func :always 1 #xffffff)
   (stencil-op :replace :replace :replace)
   
   (draw-disk 100d0 (* .5d0 1920) (*  .5d0 1080))
;; center on camera 549,365
;; 400 pixels on lcos = 276 pixels on camera (with binning 2)
   (color-mask :true :true :true :true)
   (depth-mask :false)
   (stencil-func :equal 1 #xffffff)
   (stencil-op :keep :keep :keep)

   (disable :depth-test)
   (with-pushed-matrix 
     (translate (* .5 1920) (*  .5 1080) 0)
     (rotate (* (phase z) 180d0 (/ pi)) 0 0 1)
     (translate (* -.5 1920) (* -.5 1080) 0)
     (draw *bild*))
   (disable :stencil-test)
   (enable :depth-test)

   (fill-grating *grating* (abs z))
   (format t "~a~%" cnt)
   (if (< cnt 360d0)
       (incf cnt 30d0)
       (setf cnt 0d0))
   (update *bild*)
   (swap-buffers)
   (sleep (/ 1d0) #+nil (* (/ 1000d0) 2 186.53d0))
   (post-redisplay))))

(defun run ()
 (display-window
  (make-instance 'window :width 1920 :height 1080 :pos-y -23 :pos-x 1280 :mode '(:double :rgb :stencil))))

#+nil
(run)

;; this is only run when 'sbcl --load ...' is used. not if file is
;; compiled in emacs/slime with C-c C-k

(eval-when (:execute)
  (sb-thread:make-thread #'run))

;;;; drawing.lisp

(in-package #:sketch)

;;;  ____  ____      ___        _____ _   _  ____
;;; |  _ \|  _ \    / \ \      / /_ _| \ | |/ ___|
;;; | | | | |_) |  / _ \ \ /\ / / | ||  \| | |  _
;;; | |_| |  _ <  / ___ \ V  V /  | || |\  | |_| |
;;; |____/|_| \_\/_/   \_\_/\_/  |___|_| \_|\____|
;;;
;;;  http://onrendering.blogspot.com/2011/10/buffer-object-streaming-in-opengl.html
;;;  http://www.java-gaming.org/index.php?topic=32169.0

(kit.gl.vao:defvao sketch-vao ()
  (:interleave ()
               (vertex :float 2)
               (texcoord :float 2)
               (color :unsigned-byte 4 :out-type :float)))

(defparameter *buffer-size* (expt 2 17))
(defparameter *vertex-attributes* 5)
(defparameter *bytes-per-vertex* (+ (* 4 *vertex-attributes*)))
(defparameter +access-mode+
  (cffi:foreign-bitfield-value '%gl:BufferAccessMask
                               '(:map-write :map-unsynchronized)))

(defparameter *draw-mode* :gpu)
(defparameter *draw-sequence* nil)

(defparameter *uv-rect* nil)

(defmacro with-uv-rect (rect &body body)
  `(let ((*uv-rect* ,rect))
     ,@body))

(defun orphane-vbo ()
  (kit.gl.vao:vao-buffer-data (env-vao *env*) 0 *buffer-size* (cffi:null-pointer) :stream-draw)
  (setf (env-buffer-position *env*) 0))

(defun start-draw ()
  (orphane-vbo)
  (kit.gl.vao:vao-bind (env-vao *env*)))

(defun end-draw ()
  (%gl:bind-buffer :array-buffer 0)
  (kit.gl.vao:vao-unbind))

(defun map-vbo-range (length)
  (symbol-macrolet ((position (env-buffer-position *env*)))
    (when (> (* *bytes-per-vertex* (+ position length)) *buffer-size*)
      (orphane-vbo))
    (%gl:map-buffer-range :array-buffer
                          (* position *bytes-per-vertex*)
                          (* length *bytes-per-vertex*)
                          +access-mode+)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro with-vbo-range ((buffer-pointer-var length) &body body)
    (let ((g!length (gensym)))
      `(let* ((,g!length ,length)
              (,buffer-pointer-var (map-vbo-range ,g!length)))
         (prog1 (progn ,@body)
           (incf (env-buffer-position *env*) ,g!length))))))

(defun shader-color-texture-values (res)
  (typecase res
    (color (values (or (color-vector-255 res) (env-white-color-vector *env*))
                   (env-white-pixel-texture *env*)))
    (cropped-image (values (env-white-color-vector *env*)
                           (or (image-texture res) (env-white-pixel-texture *env*))
                           (cropped-image-uv-rect res)))
    (image (values (env-white-color-vector *env*)
                   (or (image-texture res) (env-white-pixel-texture *env*))))))

(defun draw-shape (primitive fill-vertices stroke-vertices)
  (when (and fill-vertices (pen-fill (env-pen *env*)))
    (multiple-value-bind (shader-color shader-texture uv-rect)
        (shader-color-texture-values (pen-fill (env-pen *env*)))
      (with-uv-rect uv-rect
        (push-vertices fill-vertices
                       shader-color
                       shader-texture
                       primitive
                       *draw-mode*))))
  (when (and stroke-vertices (pen-stroke (env-pen *env*)))
    (multiple-value-bind (shader-color shader-texture uv-rect)
        (shader-color-texture-values (pen-stroke (env-pen *env*)))
      (with-uv-rect uv-rect
        (let* ((weight (or (pen-weight (env-pen *env*)) 1))
               (mixed (mix-lists stroke-vertices
                                 (grow-polygon stroke-vertices weight))))
          (push-vertices (append mixed (list (first mixed) (second mixed)))
                         shader-color
                         shader-texture
                         :triangle-strip
                         *draw-mode*))))))

(defmethod push-vertices (vertices color texture primitive (draw-mode (eql :gpu)))
  (kit.gl.shader:uniform-matrix (env-programs *env*) :model-m 4
                                (vector (env-model-matrix *env*)))
  (gl:bind-texture :texture-2d texture)
  (with-vbo-range (buffer-pointer (length vertices))
    (fill-buffer buffer-pointer vertices color)
    (%gl:unmap-buffer :array-buffer)
    (%gl:draw-arrays primitive (env-buffer-position *env*) (length vertices))))

(defmethod push-vertices (vertices color texture primitive (draw-mode (eql :figure)))
  (let* ((vertices (mapcar (lambda (v) (transform-vertex v (env-model-matrix *env*)))
                           vertices))
         (buffer (static-vectors:make-static-vector
                  (* *bytes-per-vertex* (length vertices))
                  :element-type '(unsigned-byte 8)))
         (buffer-pointer (static-vectors:static-vector-pointer buffer)))
    (fill-buffer buffer-pointer vertices color)
    (push (list :primitive primitive
                :pointer buffer-pointer
                :length (length vertices))
          *draw-sequence*)))

(defun transform-vertex (vertex matrix)
  (let* ((vector (sb-cga:vec
                  (coerce (car vertex) 'single-float)
                  (coerce (cadr vertex) 'single-float)
                  0.0))
         (transformed (sb-cga:transform-point vector matrix)))
    ;; TODO: This is painfully inelegant.
    ;; No consing should happen at this point.
    (list (elt transformed 0) (elt transformed 1))))

(defun fit-uv-to-rect (uv)
  (if *uv-rect*
      (destructuring-bind (u-in v-in) uv
        (destructuring-bind (u1 v1 u-range v-range) *uv-rect*
            (list (+ u1 (* u-range u-in))
                  (+ v1 (* v-range v-in)))))
      uv))

(defun fill-buffer (buffer-pointer vertices color)
  (loop
     for idx from 0 by *vertex-attributes*
     for (x y) in vertices
     for (tx ty) in (mapcar #'fit-uv-to-rect (normalize-to-bounding-box vertices))
     do (setf (cffi:mem-aref buffer-pointer :float idx) (coerce-float x)
              (cffi:mem-aref buffer-pointer :float (+ idx 1)) (coerce-float y)
              (cffi:mem-aref buffer-pointer :float (+ idx 2)) (coerce-float tx)
              (cffi:mem-aref buffer-pointer :float (+ idx 3)) (coerce-float (* ty (env-y-axis-sgn *env*)))
              (cffi:mem-aref buffer-pointer :uint8 (* 4 (+ idx 4))) (aref color 0)
              (cffi:mem-aref buffer-pointer :uint8 (+ (* 4 (+ idx 4)) 1)) (aref color 1)
              (cffi:mem-aref buffer-pointer :uint8 (+ (* 4 (+ idx 4)) 2)) (aref color 2)
              (cffi:mem-aref buffer-pointer :uint8 (+ (* 4 (+ idx 4)) 3)) (aref color 3))))

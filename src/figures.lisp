;;;; figures.lisp

(in-package #:sketch)

;;;  _____ ___ ____ _   _ ____  _____ ____
;;; |  ___|_ _/ ___| | | |  _ \| ____/ ___|
;;; | |_   | | |  _| | | | |_) |  _| \___ \
;;; |  _|  | | |_| | |_| |  _ <| |___ ___) |
;;; |_|   |___\____|\___/|_| \_\_____|____/

(defclass figure ()
  ((draws :initarg :draws)))

(defmethod draw ((figure figure) &key &allow-other-keys)
  (with-slots (draws) figure
    (kit.gl.shader:uniform-matrix (env-programs *env*) :model-m 4
                                  (vector (env-model-matrix *env*)))
    (gl:bind-texture :texture-2d (env-white-pixel-texture *env*))
    (dolist (draw draws)
      (let ((primitive (getf draw :primitive))
            (pointer (getf draw :pointer))
            (length (getf draw :length)))
        (with-vbo-range (buffer-pointer length)
          (copy-buffer pointer buffer-pointer (* length *bytes-per-vertex*))
          (%gl:unmap-buffer :array-buffer)
          (%gl:draw-arrays primitive (env-buffer-position *env*) length))))))

(defmacro deffigure (name &body body)
  `(let ((*draw-sequence* nil))
     (let ((*env* (make-env))
           (*draw-mode* :figure))
       (with-pen (make-default-pen)
         ,@body))
     (setf *draw-sequence* (nreverse *draw-sequence*))
     (let ((figure (make-instance 'figure :draws *draw-sequence*)))
       (defun ,name (x y)
         (translate x y)
         (draw figure)
         (translate (- x) (- y))))))

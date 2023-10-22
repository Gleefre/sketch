;;;; sketch.lisp

(in-package #:sketch)

;;; "sketch" goes here. Hacks and glory await!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                  ;;;
;;;     _|_|_|  _|    _|  _|_|_|_|  _|_|_|_|_|    _|_|_|  _|    _|   ;;;
;;;   _|        _|  _|    _|            _|      _|        _|    _|   ;;;
;;;     _|_|    _|_|      _|_|_|        _|      _|        _|_|_|_|   ;;;
;;;         _|  _|  _|    _|            _|      _|        _|    _|   ;;;
;;;   _|_|_|    _|    _|  _|_|_|_|      _|        _|_|_|  _|    _|   ;;;
;;;                                                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Sketch class

(defparameter *sketch* nil
  "The current sketch instance.")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *default-slots*
    '((title :initform "Sketch" :accessor sketch-title :initarg :title)
      (width :initform 400 :accessor sketch-width :initarg :width)
      (height :initform 400 :accessor sketch-height :initarg :height)
      (fullscreen :initform nil :accessor sketch-fullscreen :initarg :fullscreen)
      (copy-pixels :initform nil :accessor sketch-copy-pixels :initarg :copy-pixels)
      (y-axis :initform :down :accessor sketch-y-axis :initarg :y-axis))))

(defmacro define-sketch-class ()
  `(defclass sketch (kit.sdl2:gl-window)
     ((%env :initform (make-env))
      (%restart :initform t)
      ,@*default-slots*)))

(define-sketch-class)

;;; Non trivial sketch writers

(defmacro define-sketch-writer (slot &body body)
  `(defmethod (setf ,(alexandria:symbolicate 'sketch- slot)) :after (value (instance sketch))
     (let ((win (kit.sdl2:sdl-window instance)))
       ,@body)))

(define-sketch-writer title
  (sdl2:set-window-title win value))

(define-sketch-writer width
  (sdl2:set-window-size win value (sketch-height instance)))

(define-sketch-writer height
  (sdl2:set-window-size win (sketch-width instance) value))

(define-sketch-writer fullscreen
  (sdl2:set-window-fullscreen win value))

(define-sketch-writer y-axis
  (declare (ignore win))
  (with-slots ((env %env) width height y-axis) instance
    (setf (env-view-matrix env)
          (if (eq y-axis :down)
              (kit.glm:ortho-matrix 0 width height 0 -1 1)
              (kit.glm:ortho-matrix 0 width 0 height -1 1)))
    (kit.gl.shader:uniform-matrix
     (env-programs env) :view-m 4 (vector (env-view-matrix env)))))

;;; Generic functions

(defgeneric prepare (instance &key &allow-other-keys)
  (:documentation "Generated by DEFSKETCH.")
  (:method-combination progn :most-specific-last))

(defgeneric setup (instance &key &allow-other-keys)
  (:documentation "Called before creating the sketch window.")
  (:method ((instance sketch) &key &allow-other-keys) ()))

(defgeneric draw (instance &key &allow-other-keys)
  (:documentation "Called repeatedly after creating the sketch window,
used for drawing, 60fps.")
  (:method ((instance sketch) &key &allow-other-keys) ()))

;;; Initialization

(defparameter *initialized* nil)

(defun initialize-sketch ()
  (unless *initialized*
    (setf *initialized* t)
    (kit.sdl2:init)
    (sdl2-ttf:init)
    (sdl2:in-main-thread ()
      (sdl2:gl-set-attr :multisamplebuffers 1)
      (sdl2:gl-set-attr :multisamplesamples 4)

      (sdl2:gl-set-attr :context-major-version 3)
      (sdl2:gl-set-attr :context-minor-version 3)
      (sdl2:gl-set-attr :context-profile-mask 1))))

(defmethod initialize-instance :around ((instance sketch) &key &allow-other-keys)
  (initialize-sketch)
  (call-next-method)
  (kit.sdl2:start))

(defmethod initialize-instance :after ((instance sketch) &rest initargs &key &allow-other-keys)
  (initialize-environment instance)
  (apply #'prepare (list* instance initargs))
  (initialize-gl instance))

(defmethod update-instance-for-redefined-class :after
    ((instance sketch) added-slots discarded-slots property-list &rest initargs)
  (declare (ignore added-slots discarded-slots property-list))
  (apply #'prepare (list* instance initargs)))

;;; Rendering

(defmacro gl-catch (error-color &body body)
  `(handler-case
       (progn
         ,@body)
     (error (e)
       (progn
         (background ,error-color)
         (with-font (make-error-font)
           (with-identity-matrix
             (text (format nil "ERROR~%---~%~a~%---~%Click for restarts." e) 20 20)))
         (setf %restart t
               (env-red-screen *env*) t)))))

(defun draw-window (window)
  (start-draw)
  (draw window)
  (end-draw))

(defmethod kit.sdl2:render ((instance sketch))
  (with-slots (%env %restart width height copy-pixels) instance
    (with-environment %env
      (with-pen (make-default-pen)
        (with-font (make-default-font)
          (with-identity-matrix
            (unless copy-pixels
              (background (gray 0.4)))
            ;; Restart sketch on setup and when recovering from an error.
            (when %restart
              (gl-catch (rgb 1 1 0.3)
                (setup instance))
              (setf (slot-value instance '%restart) nil))
            ;; If we're in the debug mode, we exit from it immediately,
            ;; so that the restarts are shown only once. Afterwards, we
            ;; continue presenting the user with the red screen, waiting for
            ;; the error to be fixed, or for the debug key to be pressed again.
            (if (debug-mode-p)
                (progn
                  (exit-debug-mode)
                  (draw-window instance))
                (gl-catch (rgb 0.7 0 0)
                  (draw-window instance)))))))))

;;; Support for resizable windows

(defmethod kit.sdl2:window-event :before ((instance sketch) (type (eql :size-changed)) timestamp data1 data2)
  (with-slots ((env %env) width height y-axis) instance
    (setf width data1
          height data2
          (env-view-matrix env)
          (if (eq y-axis :down)
              (kit.glm:ortho-matrix 0 width height 0 -1 1)
              (kit.glm:ortho-matrix 0 width 0 height -1 1)))
    (gl:viewport 0 0 width height)
    (kit.gl.shader:uniform-matrix
     (env-programs env) :view-m 4 (vector (env-view-matrix env)))))

;;; Default events

(defmethod kit.sdl2:keyboard-event :before ((instance sketch) state timestamp repeatp keysym)
  (declare (ignorable timestamp repeatp))
  (when (and (eql state :keyup)
             (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape))
    (kit.sdl2:close-window instance)))

(defmethod close-window :before ((instance sketch))
  (with-environment (slot-value instance '%env)
    (loop for resource being the hash-values of (env-resources *env*)
       do (free-resource resource))))

(defmethod close-window :after ((instance sketch))
  (when (and *build* (not (kit.sdl2:all-windows)))
    (sdl2-ttf:quit)
    (kit.sdl2:quit)))

;;; DEFSKETCH helpers

(defun first-two (list)
  (list (first list) (second list)))

(defun default-slot-p (slot-or-binding)
  (let ((defaults (mapcar #'car *default-slots*)))
    (typecase slot-or-binding
      (list (member (car slot-or-binding) defaults))
      (t (member slot-or-binding defaults)))))

(defun custom-bindings (&optional bindings)
  (remove-if (lambda (binding)
               (member (car binding) (mapcar #'car *default-slots*)))
             bindings))

(defun intern-accessor (name)
  (intern (string (alexandria:symbolicate 'sketch- name)) :sketch))

(defun binding-accessor (sketch binding)
  (if (default-slot-p binding)
      (intern-accessor (car binding))
      (or (cadr (member :accessor (cddr binding)))
          (alexandria:symbolicate sketch '- (car binding)))))

(defun make-slot-form (sketch binding)
  `(,(car binding)
     :initarg ,(alexandria:make-keyword (car binding))
     :accessor ,(binding-accessor sketch binding)))

;;; DEFSKETCH channels

(defun channel-binding-p (binding)
  (and (consp (cadr binding)) (eql 'in (caadr binding))))

(defun make-channel-observer (sketch binding)
  `(define-channel-observer
     (let ((win (kit.sdl2:last-window)))
       (when win
         (setf (,(binding-accessor sketch binding) win) ,(cadr binding))))))

(defun make-channel-observers (sketch bindings)
  (mapcar (lambda (binding)
            (when (channel-binding-p binding)
              (make-channel-observer sketch binding)))
          bindings))

(defun replace-channels-with-values (bindings)
  (loop for binding in bindings
     collect (list (car binding)
                   (if (channel-binding-p binding)
                       (caddr (cadr binding))
                       (cadr binding)))))

;;; DEFSKETCH bindings

(defun sketch-bindings-to-slots (sketch bindings)
  (mapcar (lambda (x) (make-slot-form sketch x))
          (remove-if (lambda (x)
                       (member (car x) (mapcar #'car *default-slots*)))
                     bindings)))

;;; DEFSKETCH setf instructions

(defun make-window-parameter-setf ()
  `(setf ,@(mapcan (lambda (slot)
                     `((,(intern-accessor (car slot)) instance) ,(car slot)))
                   *default-slots*)))

(defun make-custom-slots-setf (sketch bindings)
  `(setf ,@(mapcan (lambda (binding)
                     `((slot-value instance ',(car binding)) ,(car binding)))
                   bindings)))

(defun make-reinitialize-setf ()
  `(setf ,@(mapcan (lambda (slot)
                     `((,(intern-accessor (car slot)) instance)
                       (,(intern-accessor (car slot)) instance)))
                   *default-slots*)))

(defun custom-slots (bindings)
  (loop
     for b in (mapcar #'car bindings)
     if (not (member b *default-slots*))
     collect b))

;;; DEFSKETCH macro

(defmacro defsketch (sketch-name bindings &body body)
  (let ((default-not-overridden
          (remove-if (lambda (x) (find x bindings :key #'car))
                     (mapcar #'car *default-slots*))))
    `(progn
       (defclass ,sketch-name (sketch)
         ,(sketch-bindings-to-slots `,sketch-name bindings))

       ,@(remove-if-not #'identity (make-channel-observers sketch-name bindings))

       (defmethod prepare progn ((instance ,sketch-name) &rest initargs &key &allow-other-keys)
                  (declare (ignorable initargs))
                  (let* (,@(loop for slot in default-not-overridden
                              collect `(,slot (slot-value instance ',slot)))
                         ,@(mapcar (lambda (binding)
                                     (destructuring-bind (name value)
                                         (first-two binding)
                                       (list name (if (default-slot-p name)
                                                      `(if (getf initargs ,(alexandria:make-keyword name))
                                                           (slot-value instance ',name)
                                                           ,value)
                                                      `(or (getf initargs ,(alexandria:make-keyword name)) ,value)))))
                                   (replace-channels-with-values bindings)))
                    (declare (ignorable ,@(mapcar #'car *default-slots*) ,@(custom-slots bindings)))
                    ,(make-window-parameter-setf)
                    ,(make-custom-slots-setf sketch-name (custom-bindings bindings)))
                  (setf (env-y-axis-sgn (slot-value instance '%env))
                        (if (eq (slot-value instance 'y-axis) :down) +1 -1)))

       (defmethod draw ((instance ,sketch-name) &key &allow-other-keys)
         (let ((*sketch* instance))
           (with-accessors ,(mapcar (lambda (x) (list (car x) (intern-accessor (car x))))
                             *default-slots*) instance
             (with-slots ,(mapcar #'car bindings) instance
               ,@body))))

       (make-instances-obsolete ',sketch-name)

       (find-class ',sketch-name))))

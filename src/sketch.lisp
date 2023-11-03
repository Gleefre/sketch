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

(defparameter *default-width* 400
  "The default width of sketch window")
(defparameter *default-height* 400
  "The default height of sketch window")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *default-slots*
    '((title :initform "Sketch" :accessor sketch-title :initarg :title)
      (width :initform *default-width* :accessor sketch-width :initarg :width)
      (height :initform *default-height* :accessor sketch-height :initarg :height)
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
    ;; FIXME: Code duplicated (with initialize-environment)
    (setf (env-view-matrix env) (if (eq y-axis :down)
                                    (kit.glm:ortho-matrix 0 width height 0 -1 1)
                                    (kit.glm:ortho-matrix 0 width 0 height -1 1))
          (env-y-axis-sgn env) (if (eq y-axis :down) +1 -1))
    (kit.gl.shader:uniform-matrix
     (env-programs env) :view-m 4 (vector (env-view-matrix env)))))

;;; Generic functions

(defgeneric prepare (instance &key &allow-other-keys)
  (:documentation "Generated by DEFSKETCH."))

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
  (with-environment (slot-value instance '%env)
    (set-var :sketch instance))
  (apply #'prepare instance initargs)
  (initialize-gl instance))

(defmethod update-instance-for-redefined-class :after
    ((instance sketch) added-slots discarded-slots property-list &rest initargs)
  (declare (ignore added-slots discarded-slots property-list))
  (apply #'prepare instance initargs))

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

;;; DEFSKETCH bindings

(defclass binding ()
  ((name :initarg :name :accessor binding-name)
   (sketch-name :initarg :sketch-name :accessor binding-sketch-name)
   (initform :initarg :initform :accessor binding-initform)
   (defaultp :initarg :defaultp :accessor binding-defaultp)
   (initarg :initarg :initarg :accessor binding-initarg)
   (accessor :initarg :accessor :accessor binding-accessor)
   (hookp :initarg :hookp :accessor binding-hookp)
   (hook-name :initarg :hook-name :accessor binding-hook-name)
   (hook-vars :initarg :hook-vars :accessor binding-hook-vars)))

(defun make-binding (name &key (sketch-name 'sketch)
                               (defaultp nil)
                               (initform nil)
                               (initarg (alexandria:make-keyword name))
                               (accessor (alexandria:symbolicate sketch-name '#:- name))
                               (hook-vars nil hook-vars-p)
                               (hook-name (when hook-vars-p
                                            (alexandria:symbolicate sketch-name '#:- name '#:-hook))))
  (make-instance 'binding :name name
                          :sketch-name sketch-name
                          :defaultp defaultp
                          :initform initform
                          :initarg initarg
                          :accessor accessor
                          :hook-vars hook-vars
                          :hookp hook-vars-p
                          :hook-name hook-name))

(defun add-default-bindings (parsed-bindings)
  (loop for (name . args) in (reverse *default-slots*)
        unless (member name parsed-bindings :key #'binding-name)
        do (push (apply #'make-binding name :defaultp t args) parsed-bindings))
  parsed-bindings)

(defun parse-bindings (sketch-name bindings)
  (add-default-bindings
   (loop for (name value . args) in (alexandria:ensure-list bindings)
         for default-slot-p = (assoc name *default-slots*)
         ;; If a VALUE is of form (VAR VAR-NAME DEFAULT-VALUE) it is
         ;; recognized as a variable in sketch's environment. We
         ;; should pass additional :hook-vars parameter to
         ;; MAKE-BINDING and set the VALUE to the DEFAULT-VALUE.
         when (and (consp value)
                   (eq 'var (car value)))
           do (setf args (list* :hook-vars (list (second value)) args)
                    value (third value))
         collect (apply #'make-binding
                        name
                        :initform value
                        (if default-slot-p
                            (cdddr default-slot-p)
                            (list* :sketch-name sketch-name args))))))

;;; DEFSKETCH hooks

(defun define-sketch-hooks (sketch-name bindings)
  `(progn
     ,@(loop for b in bindings
             when (binding-hookp b)
             collect `(defun ,(binding-hook-name b) ()
                        (setf (,(binding-accessor b) (var :sketch))
                              (var ,(binding-hook-name b)))))
     (defmethod initialize-instance :after ((sketch ,sketch-name) &key &allow-other-keys)
       (with-environment (slot-value sketch '%env)
         ,@(loop for b in bindings
                 when (binding-hookp b)
                 collect `(add-hook '(,@(binding-hook-vars b))
                                    ',(binding-hook-name b)))))))

;;; DEFSKETCH macro

(defun define-sketch-defclass (name bindings)
  `(defclass ,name (sketch)
     (,@(loop for b in bindings
              unless (eq 'sketch (binding-sketch-name b))
              collect `(,(binding-name b)
                        :initarg ,(binding-initarg b)
                        :accessor ,(binding-accessor b))))))

(defun define-draw-method (name bindings body)
  `(defmethod draw ((*sketch* ,name) &key &allow-other-keys)
     (with-accessors (,@(loop for b in bindings
                              collect `(,(binding-name b) ,(binding-accessor b))))
         *sketch*
       ,@body)))

(defun define-prepare-method (name bindings)
  `(defmethod prepare ((*sketch* ,name)
                       &key ,@(loop for b in bindings
                                    collect `((,(binding-initarg b) ,(binding-name b))
                                              ,(if (binding-defaultp b)
                                                   `(,(binding-accessor b) *sketch*)
                                                   (binding-initform b))))
                       &allow-other-keys)
     (setf ,@(loop for b in bindings
                   collect `(,(binding-accessor b) *sketch*)
                   collect (binding-name b)))))

(defmacro defsketch (sketch-name bindings &body body)
  (let ((bindings (parse-bindings sketch-name bindings)))
    `(progn
       ,(define-sketch-defclass sketch-name bindings)
       ,(define-sketch-hooks sketch-name bindings)
       ,(define-prepare-method sketch-name bindings)
       ,(define-draw-method sketch-name bindings body)

       (make-instances-obsolete ',sketch-name)
       (find-class ',sketch-name))))

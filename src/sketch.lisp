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
      (resizable :initform nil :accessor sketch-resizable :initarg :resizable)
      (copy-pixels :initform nil :accessor sketch-copy-pixels :initarg :copy-pixels)
      (y-axis :initform :down :accessor sketch-y-axis :initarg :y-axis))))

(defmacro define-sketch-class ()
  `(defclass sketch ()
     ((%env :initform (make-env))
      (%restart :initform t)
      (%window :initform nil)
      ,@*default-slots*)))

(define-sketch-class)

;;; sketch-window

(defclass sketch-window (kit.sdl2:gl-window)
  ((%sketch :initarg :sketch :type sketch)))

(macrolet ((define-forward (name (&rest args) &optional (empty? t))
             `(progn
                (defmethod ,name ((w sketch-window) ,@args)
                  (,name (slot-value w '%sketch) ,@args))
                ,@(when empty?
                    `((defmethod ,name ((w sketch) ,@args)))))))
  (define-forward kit.sdl2:mousebutton-event (state timestamp button x y))
  (define-forward kit.sdl2:mousemotion-event (timestamp button-mask x y xrel yrel))
  (define-forward kit.sdl2:mousewheel-event (timestamp x y))
  (define-forward kit.sdl2:textinput-event (timestamp text))
  (define-forward kit.sdl2:keyboard-event (state timestamp repeatp keysym))
  (define-forward kit.sdl2:window-event (type timestamp data1 data2))
  (define-forward kit.sdl2:controller-added-event (c))
  (define-forward kit.sdl2:controller-removed-event (c))
  (define-forward kit.sdl2:controller-axis-motion-event (controller timestamp axis value))
  (define-forward kit.sdl2:controller-button-event (controller state timestamp button))
  (define-forward kit.sdl2:other-event (event)))

;;; Non trivial sketch writers

(defmacro define-sketch-writer (slot &body body)
  `(defmethod (setf ,(alexandria:symbolicate 'sketch- slot)) :after (value (instance sketch))
     (let ((win (when (slot-value instance '%window)
                  (kit.sdl2:sdl-window (slot-value instance '%window)))))
       (declare (ignorable win))
       ,@body)))

(define-sketch-writer title
  (when win (sdl2:set-window-title win value)))

(define-sketch-writer width
  (when win (sdl2:set-window-size win value (sketch-height instance))))

(define-sketch-writer height
  (when win (sdl2:set-window-size win (sketch-width instance) value)))

(define-sketch-writer fullscreen
  (when win (sdl2:set-window-fullscreen win value)))

(define-sketch-writer y-axis
  (with-slots ((env %env) width height y-axis) instance
    (when (env-initialized-p env)
      ;; FIXME: Code duplicated (with initialize-environment)
      (setf (env-view-matrix env) (if (eq y-axis :down)
                                      (kit.glm:ortho-matrix 0 width height 0 -1 1)
                                      (kit.glm:ortho-matrix 0 width 0 height -1 1))
            (env-y-axis-sgn env) (if (eq y-axis :down) +1 -1))
      (kit.gl.shader:uniform-matrix
       (env-programs env) :view-m 4 (vector (env-view-matrix env))))))

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

(defmethod initialize-instance :after ((instance sketch) &rest initargs &key no-window &allow-other-keys)
  (initialize-sketch)
  (apply #'prepare instance initargs)
  (unless no-window
    (setf (slot-value instance '%window)
          (apply #'make-instance 'sketch-window
                 :sketch instance
                 :title (sketch-title instance)
                 :w (sketch-width instance)
                 :h (sketch-height instance)
                 :resizable (sketch-resizable instance)
                 :fullscreen (sketch-fullscreen instance)
                 (remove-from-plist-if (lambda (key)
                                         (not (member key '(:x :y :shown :flags))))
                                       initargs)))
    (kit.sdl2:start)
    (setf (kit.sdl2:idle-render (slot-value instance '%window)) t)))

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
  (kit.sdl2:render (slot-value instance '%window)))

(defmethod kit.sdl2:render ((window sketch-window))
  (render (slot-value window '%sketch)))

(defun render (instance)
  (with-slots (%env %restart width height copy-pixels) instance
    (unless (env-initialized-p %env)
      (initialize-environment instance)
      (initialize-gl instance))
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
    (close-sketch instance)))

(defmethod close-sketch ((instance sketch))
  (when (slot-value instance '%window)
    (setf (kit.sdl2:render-enabled (slot-value instance '%window)) nil))
  (with-environment (slot-value instance '%env)
    (loop for resource being the hash-values of (env-resources *env*)
          do (free-resource resource)))
  (when (slot-value instance '%window)
    (kit.sdl2:close-window (slot-value instance '%window))
    (when (and *build* (not (kit.sdl2:all-windows)))
      (sdl2-ttf:quit)
      (kit.sdl2:quit))))

;;; DEFSKETCH bindings

(defclass binding ()
  ((name :initarg :name :accessor binding-name)
   (sketch-name :initarg :sketch-name :accessor binding-sketch-name)
   (initform :initarg :initform :accessor binding-initform)
   (defaultp :initarg :defaultp :accessor binding-defaultp)
   (initarg :initarg :initarg :accessor binding-initarg)
   (accessor :initarg :accessor :accessor binding-accessor)
   (channelp :initarg :channelp :accessor binding-channelp)
   (channel-name :initarg :channel-name :accessor binding-channel-name)))

(defun make-binding (name &key (sketch-name 'sketch)
                               (defaultp nil)
                               (initform nil)
                               (initarg (alexandria:make-keyword name))
                               (accessor (alexandria:symbolicate sketch-name '#:- name))
                               (channel-name nil channel-name-p))
  (make-instance 'binding :name name
                          :sketch-name sketch-name
                          :defaultp defaultp
                          :initform initform
                          :initarg initarg
                          :accessor accessor
                          :channel-name channel-name
                          :channelp channel-name-p))

(defun add-default-bindings (parsed-bindings)
  (loop for (name . args) in (reverse *default-slots*)
        unless (member name parsed-bindings :key #'binding-name)
        do (push (apply #'make-binding name :defaultp t args) parsed-bindings))
  parsed-bindings)

(defun parse-bindings (sketch-name bindings)
  (add-default-bindings
   (loop for (name value . args) in (alexandria:ensure-list bindings)
         for default-slot-p = (assoc name *default-slots*)
         ;; If a VALUE is of form (IN CHANNEL-NAME DEFAULT-VALUE) it
         ;; is recognized as a channel. We should pass additional
         ;; :channel-name parameter to MAKE-BINDING and set the VALUE
         ;; to the DEFAULT-VALUE.
         when (and (consp value)
                   (eq 'in (car value)))
           do (setf args (list* :channel-name (second value) args)
                    value (third value))
         collect (apply #'make-binding
                        name
                        :initform value
                        (if default-slot-p
                            (cdddr default-slot-p)
                            (list* :sketch-name sketch-name args))))))

;;; DEFSKETCH channels

(defun define-channel-observers (bindings)
  (loop for b in bindings
        when (binding-channelp b)
        collect `(define-channel-observer
                   (let ((win (kit.sdl2:last-window)))
                     (when win
                       (setf (,(binding-accessor b) win)
                             (in ,(binding-channel-name b)
                                 ,(binding-initform b))))))))

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
       ,@(define-channel-observers bindings)
       ,(define-prepare-method sketch-name bindings)
       ,(define-draw-method sketch-name bindings body)

       (make-instances-obsolete ',sketch-name)
       (find-class ',sketch-name))))

;;;; controllers.lisp

(in-package #:sketch)

;;;   ____ ___  _   _ _____ ____   ___  _     _     _____ ____  ____
;;;  / ___/ _ \| \ | |_   _|  _ \ / _ \| |   | |   | ____|  _ \/ ___|
;;; | |  | | | |  \| | | | | |_) | | | | |   | |   |  _| | |_) \___ \
;;; | |__| |_| | |\  | | | |  _ <| |_| | |___| |___| |___|  _ < ___) |
;;;  \____\___/|_| \_| |_| |_| \_\\___/|_____|_____|_____|_| \_\____/

;;; Mouse

(set-var :mouse (cons 0 0)
         :mouse-x 0
         :mouse-y 0
         :mouse-rel (cons 0 0)
         :mouse-xrel 0
         :mouse-yrel 0
         :mouse-wheel (cons 0 0)
         :mouse-wheel-x 0
         :mouse-wheel-y 0)

(defmethod kit.sdl2:mousemotion-event :after ((instance sketch)
                                              timestamp button-mask x y xrel yrel)
  (set-var :mouse (cons x y)
           :mouse-x x
           :mouse-y y
           :mouse-rel (cons xrel yrel)
           :mouse-xrel xrel
           :mouse-yrel yrel))

(defmethod kit.sdl2:mousewheel-event :after ((instance sketch)
                                             timestamp x y)
  (set-var :mouse-wheel (cons x y)
           :mouse-wheel-x x
           :mouse-wheel-y y))

(defmethod kit.sdl2:mousebutton-event :after ((instance sketch)
                                              state timestamp button x y)
  (with-slots (%env) instance
    (when (env-red-screen %env)
      (setf (env-debug-key-pressed %env) t))))

;;; Keyboard

(defmethod keyboard-event :after ((instance sketch)
                                  state timestamp repeatp keysym))

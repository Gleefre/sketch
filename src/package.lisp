;;;; package.lisp

(defpackage #:sketch
  (:use #:cl)

  ;; Math
  (:export #:clamp
           #:clamp-1
           #:lerp
           #:normalize
           #:rescale

           #:+pi+
           #:+two-pi+
           #:+tau+
           #:+half-pi+
           #:+quarter-pi+
           #:+epsilon+
           #:+phi+
           #:+golden-ratio+
           #:+e+

           #:radians
           #:degrees)

  ;; Utils
  (:export #:relative-path)

  ;; Resources
  (:export #:load-resource
           #:image
           #:image-width
           #:image-height
           #:crop
           #:with-uv-rect
           #:save-png)

  ;; Colors
  (:export #:color
           #:make-color
           #:color-red
           #:color-green
           #:color-blue
           #:color-hue
           #:color-saturation
           #:color-brightness
           #:color-alpha
           #:rgb-to-hsb
           #:hsb-to-rgb
           #:rgb
           #:hsb
           #:gray
           #:rgb-255
           #:hsb-360
           #:gray-255
           #:hex-to-color
           #:color-rgb
           #:color-rgba
           #:color-hsba
           #:color-vector
           #:color-vector-255
           #:lerp-color
           #:random-color
           #:hash-color
           #:color-filter-grayscale
           #:color-filter-invert
           #:color-filter-rotate
           #:color-filter-hsb
           #:+red+
           #:+green+
           #:+blue+
           #:+yellow+
           #:+magenta+
           #:+cyan+
           #:+orange+
           #:+white+
           #:+black+
           #:+gray+
           #:+indigo+)

  ;; Channels
  (:export #:register-input
           #:in
           #:out
           #:define-channel-observer
           #:define-named-channel-observer
           #:reset-all-channels)

  ;; Pen
  (:export #:pen
           #:pen-stroke
           #:pen-fill
           #:pen-weight
           #:pen-curve-steps
           #:pen-winding-rule
           #:make-pen
           #:set-pen
           #:copy-pen
           #:flip-pen
           #:with-pen
           #:background)

  ;; Font
  (:export #:make-font
           #:with-font
           #:set-font
           #:text
           #:text-line-image)

  ;; Shapes
  (:export #:point
           #:line
           #:polyline
           #:rect
           #:ngon
           #:star
           #:ellipse
           #:circle
           #:polygon
           #:bezier)

  ;; Transforms
  (:export #:set-matrix
           #:push-matrix
           #:pop-matrix
           #:translate
           #:rotate
           #:scale
           #:with-matrix
           #:with-identity-matrix
           #:with-current-matrix
           #:with-translate
           #:with-rotate
           #:with-scale)

  ;; Complex transforms
  (:export #:fit
           #:with-fit)

  ;; Sketch
  (:export #:sketch
           #:setup
           #:draw

           #:defsketch

           #:sketch-title
           #:sketch-width
           #:sketch-height
           #:sketch-fullscreen
           #:sketch-resizable
           #:sketch-copy-pixels
           #:sketch-y-axis
           #:sketch-close-on

           #:title
           #:width
           #:height
           #:fullscreen
           #:resizable
           #:copy-pixels
           #:y-axis
           #:close-on

           #:*default-width*
           #:*default-height*)

  ;; Control flow
  (:export #:start-loop
           #:stop-loop)

  ;; Entities
  (:export #:defentity
           #:entity-width
           #:entity-height)

  ;; Figures
  (:export #:deffigure)

  ;; Controllers
  (:export #:on-click
           #:on-mouse-button
           #:on-mouse-left
           #:on-mouse-middle
           #:on-mouse-right
           #:on-mouse-left-up
           #:on-mouse-left-down
           #:on-mouse-middle-up
           #:on-mouse-middle-down
           #:on-mouse-right-up
           #:on-mouse-right-down
           #:on-hover
           #:on-enter
           #:on-leave
           #:on-text
           #:on-key)

  ;; Canvas
  (:export #:make-canvas
           #:canvas-reset
           #:canvas-paint
           #:canvas-image
           #:canvas-lock
           #:canvas-unlock
           #:canvas-width
           #:canvas-height))

;;;; math.lisp

(in-package #:sketch)

;;;  __  __    _  _____ _   _
;;; |  \/  |  / \|_   _| | | |
;;; | |\/| | / _ \ | | | |_| |
;;; | |  | |/ ___ \| | |  _  |
;;; |_|  |_/_/   \_\_| |_| |_|

;; Calculation

(declaim (inline clamp))
(defun clamp (x low high)
  (cond ((< x low) low)
        ((> x high) high)
        (t x)))

(declaim (inline clamp-1))
(defun clamp-1 (x)
  (clamp x 0.0 1.0))

(declaim (inline lerp))
(defun lerp (x low high)
  (+ (* (- 1 x) low) (* x high)))

(declaim (inline normalize))
(defun normalize (x low high)
  (/ (- x low) (- high low)))

(declaim (inline rescale))
(defun rescale (x low high out-low out-high)
  (lerp (normalize x low high) out-low out-high))

;; Trigonometry

(defconstant +pi+ PI)
(defconstant +two-pi+ (* +pi+ 2))
(defconstant +tau+ +two-pi+)
(defconstant +half-pi+ (/ +pi+ 2))
(defconstant +quarter-pi+ (/ +pi+ 4))
(defconstant +epsilon+ single-float-epsilon)
(defconstant +phi+ (/ (1+ (sqrt 5d0)) 2))
(defconstant +golden-ratio+ +phi+)
(defconstant +e+ (exp 1d0))

(declaim (inline radians))
(defun radians (deg)
  (* +pi+ (/ deg 180)))

(declaim (inline degrees))
(defun degrees (rad)
  (* 180 (/ rad +pi+)))

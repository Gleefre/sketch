;; Let's support the following:
;; RGB -- most known format
;; HSB (hue saturation brightness)
;; hex
;; 0.0-1.0 or 0-255 ?
;; alpha ?
;; strings/string designators as colors? (+ compile macros and all that stuff)
;; X11 color names?
;; just gray?
;; printer, load-form ??

;; string designators like "ffaaba" or "#fad" or "#0010f044" or #:#aaaa or #:aabbccd0 e.t.c.
;; -> hex with or without # in the beginning, designating RGB or RGBA, either from 0 to 15, or from 0 to 255

;; hex as a string designator (symbol or string)
;; "#fab" or #:ffaaba88

;; rgb

;; hsb (hsv)

;; cmyk ?

;; cie ?

;; 0.0-1.0, 0d0-1d0, 0-15, 0-16, 0-255, 0-256 -- ???

;; X11 color names (as string designators, I guess? or as (x11-color <string-designator>) ?..)
;; same for svg basic/extended ?
;; maybe for string designators -- default is x11, but you can specify list by doing "x11/red" and "svg/blue" ?
;; should be case insensitive, of course

;; -> named colors, (define-color-group ... (yeah, named by strings, not symbols...) ...)
;; -> "my-game/red", "my-game/portal", ... or #:my-game/portal, ...
;; #:/portal or "/portal" --> uses *color-group*
;; #:portal --> uses *default-color-group* = X11

;; conversions, autoconversion on demand only?

;; color generators/filters

;;; Utils

(deftype string-designator ()
  `(or symbol string character))

(defun string-designator-p (object)
  (typep object 'string-designator))

(defun quotep (form)
  (and (listp form)
       (eql (car form) 'quote)
       (not (null (cdr form)))
       (null (cddr form))))

(defun trim1 (item seq &key (test 'eql))
  (if (and (plusp (length seq))
           (funcall test item (elt seq 0)))
      (subseq seq 1)
      seq))

;;; Base color (alpha?..)

(defclass base-color ()
  ((alpha :initform 1.0 :accessor color-alpha :initarg :alpha)))

;;; Types

(deftype hex-color () `(and string-designator (satisfies hex-color-p)))
(deftype named-color () `(and string-designator (satisfies named-color-p)))
(deftype color () `(or base-color hex-color named-color))

(declaim (inline colorp))
(defun colorp (object)
  (typep object 'color))

;;; Coersion

(defgeneric coerce-color (color type)
  (:method (color type)
    (unless (typep color type)
      (error "Color ~S can't be converted to type ~A." color type))))

(define-compiler-macro coerce-color (&whole form color type)
  (or (and (colorp color)
           (quotep type)
           (symbolp (cadr type))
           (ignore-errors `',(coerce-color color type)))
      form))

(defmacro define-main-coerce-color (((var from-type) to-type) &body body)
  `(progn
     (defmethod coerce-color ((color T) (eql ',to-type))
       (coerce-color (coerce-color color ',from-type) ',to-type))
     (defmethod coerce-color ((,var ,from-type) (eql ',to-type))
       ,@body)))

(defmacro define-coerce-color (((var from-type) to-type) &body body)
  `(defmethod coerce-color ((,var ,from-type) (eql ',to-type))
     ,@body))

;;; Properties

(defgeneric color-property (color property)
  (:method (color property)
    (error "Unknown property ~A for color ~S." property color)))

;; SETF's (??)
(defgeneric (setf color-property) (value color property)
  (:method (value color property)
    (error "Unknown property ~A for color ~S." property color)))

(define-compiler-macro color-property (&whole form color property)
  (or (and (colorp color)
           (quotep property)
           (symbolp (cadr property))
           (ignore-errors `',(color-property color property)))
      form))

(defmacro define-color-property ((name &key setf accessor) ((var class)) &body body)
  (let ((property (gensym "property"))
        (value (gensym "value")))
    `(progn
       (defmethod color-property ((color T) (,property (eql ',name)))
         (declare (ignore ,property))
         (color-property (coerce-color color ',class) ',name))
       (defmethod (setf color-property) (value (color T) (,property (eql ',name)))
         (declare (ignore ,property))
         ;; FIXME: this is wrong... We need to coerce it back to its
         ;; own type, but we don't know how to do that...
         (setf (color-property (coerce-color color ',class) ',name) value))
       (defmethod color-property ((,var ,class) (,property (eql ',name)))
         (declare (ignore ,property))
         ,@body)
       ,@(when setf
           `((defmethod (setf color-property) (,value (,var ,class) (,property (eql ',name)))
               (declare (ignore ,property))
               (setf ,@body ,value))))
       ,@(when accessor
           `((defun ,accessor (color)
               (color-property color ',name))
             (defun (setf ,accessor) (value color)
               (setf (color-property color ',name) value)))))))

;;; Hex colors from strings

(declaim (inline hex-color-p))
(defun hex-color-p (name)
  (and (string-designator-p name)
       (let* ((s (string name))
              (len (length s))
              (start (if (or (zerop len) (char/= (char s 0) #\#)) 0 1)))
         (and (member (- len start) '(3 4 6 8))
              (loop for i from start below len
                    always (digit-char-p (char s i) 16))))))

;;; Color groups and named colors

(defvar *color-groups* (make-hash-table :test 'equal))

(defun ensure-color-group (name)
  (or (gethash (string name) *color-groups*)
      (setf (gethash (string name) *color-groups*)
            (make-hash-table :test 'equal))))

(defun find-color-group (name)
  (prog1 (gethash (string name) *color-groups*)))

(defvar *default-color-group* "X11")
(defvar *shorthand-color-group* "")

(defun %parse-named-color (name &aux (s (string name)))
  (let ((pos (position #\/ s)))
    (cond
      ((null pos) (values *default-color-group* s))
      ((zerop pos) (values *shorthand-color-group* (subseq s 1)))
      (t (values (subseq s 0 pos) (subseq s pos))))))

(defun named-color-p (name)
  (and (string-designator-p name)
       (multiple-value-bind (group name) (%parse-named-color name)
         (let ((group (find-color-group group)))
           (when group
             (gethash group name))))))

;;; RGB

(defclass rgb-color (base-color)
  ((red :initform 0.0 :accessor rgb-color-red :initarg :red)
   (green :initform 0.0 :accessor rgb-color-green :initarg :green)
   (blue :initform 0.0 :accessor rgb-color-blue :initarg :blue)))

(define-color-property (:red :setf t :accessor color-red) ((color rgb-color))
  (rgb-color-red color))

(define-color-property (:green :setf t :accessor color-green) ((color rgb-color))
  (rgb-color-green color))

(define-color-property (:blue :setf t :accessor color-blue) ((color rgb-color))
  (rgb-color-blue color))

;;; HSB (HSV)

(defclass hsb-color (base-color)
  ((hue :initform 0.0 :accessor hsb-color-hue :initarg :hue)
   (saturation :initform 0.0 :accessor hsb-color-saturation :initarg :saturation)
   (brightness :initform 0.0 :accessor hsb-color-brightness :initarg :brightness)))

(define-color-property (:hue :setf t :accessor color-hue) ((color hsb-color))
  (hsb-color-hue color))

(define-color-property (:saturation :setf t :accessor color-saturation) ((color hsb-color))
  (hsb-color-saturation color))

(define-color-property (:brightness :setf t :accessor color-brightness) ((color hsb-color))
  (hsb-color-brightness color))

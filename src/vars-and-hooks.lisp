;;;; vars-and-hooks.lisp

(in-package #:sketch)

(in-nomine:define-namespace var
  :binding-table-var *vars*
  :default-arg-in-accessor-p t
  :error-when-not-found-p nil
  :condition-name nil)

(in-nomine:define-namespace hook
  :binding-table-var *hooks*
  :default-arg-in-accessor-p t
  :error-when-not-found-p nil
  :condition-name nil
  :accessor var-hooks)

(defun var (name &optional (default nil))
  (symbol-var name default))

(defun (setf var) (value name)
  (prog1 (setf (symbol-var name) value)
    (mapcar #'funcall (var-hooks name))))

(defun add-hook (name function)
  (pushnew function (var-hooks name)))

(defun remove-hook (name function)
  (alexandria:removef (var-hooks name) function))

(defmacro define-hook (name (&rest vars) &body body)
  `(progn
     (defun ,name () ,@body)
     ,@(loop for var in vars
             collect `(add-hook ',name ',var))))

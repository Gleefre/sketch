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

(defmacro set-var (&rest var-value)
  `(setf ,@(loop for (var value) on var-value by #'cddr
                 collect `(var ,var)
                 collect value)))

(defun add-hook (names function)
  (loop for name in (alexandria:ensure-list names)
        do (pushnew function (var-hooks name))))

(defun remove-hook (names function)
  (loop for name in (alexandria:ensure-list names)
        do (alexandria:removef (var-hooks name) function)))

(defmacro define-hook (name (&rest vars) &body body)
  (unless name
    (setf name (gensym)))
  `(progn
     (defun ,name () ,@body)
     ,@(loop for var in vars
             collect `(add-hook ',name ',var))))

(defpackage shot
  (:use :cl)
  (:import-from :shot.parser
                :parse)
  (:import-from :shot.transpile
                :*symbol-table*
                :*bindings*
                :statements)
  (:import-from :shot.condition
                :shot-error
                :failed)
  (:export :make-env
           :bindings
           :env-bindings
           :shot
           :shot-error
           :failed))
(in-package :shot)

(defstruct env
  (symbol-table (make-hash-table :test 'equal))
  (bindings ()))

(defun shot (source &optional (env (make-env)))
  (let ((*symbol-table* (env-symbol-table env))
        (*bindings* (env-bindings env)))
    (prog1
        (eval (statements (parse source)))
      (setf (env-bindings env) *bindings*))))

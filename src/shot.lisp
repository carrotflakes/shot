(defpackage shot
  (:use :cl)
  (:import-from :shot.parser
                :parse)
  (:import-from :shot.transpile
                :statements)
  (:import-from :shot.condition
                :shot-error
                :failed)
  (:export :shot
           :shot-error
           :failed))
(in-package :shot)

(defun shot (source)
  (eval (statements (parse source))))

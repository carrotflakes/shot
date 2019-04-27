(defpackage shot.condition
  (:use :cl)
  (:export :shot-error))
(in-package :shot.condition)

(define-condition shot-error (simple-error)
  ())

(define-condition failed (shot-error)
  ())

(defpackage shot-test
  (:use :cl
        :shot
        :prove))
(in-package :shot-test)

;; NOTE: To run this test file, execute `(asdf:test-system :shot)' in your Lisp.

(plan nil)

(defun f (source)
  (print source)
  (print (shot:shot source)))

(f "1")
(f "-123")
(f "a = 1")
(f "b = 2 c = 3 {a::b c \"d\": 1 ...{}}")
(f "{}")
(f "{a: b c: d ..._} = {a: 1 c: 2 e: true}")
(f "f x = x + 1")
(f "[1=true 2=false](1)")
(f "[x=x<0?:minus|0<x?:plus|:zero](1)")
(f "f x=x<0?:minus|0<x?:plus|:zero [f(1) f(0) f(-1)]")

(finalize)

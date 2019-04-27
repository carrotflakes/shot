(defpackage shot
  (:use :cl)
  (:import-from :shot.parser
                :parse)
  (:import-from :shot.transpile
                :statements
                :expression))
(in-package :shot)

(defun f (source)
  (print source)
  (let ((ast (parse source)))
    (print ast)
    (let ((code (statements ast)))
      (print code)
      (print (eval code)))))

(f "1")
(f "a = 1")
(f "b = 2 c = 3 {a::b c \"d\": 1 ...{}}")
(f "{}")
(f "{a: b c: d ..._} = {a: 1 c: 2 e: true}")
(f "f x = x + 1")
(f "[1=true 2=false](1)")

#|
  This file is a part of shot project.
  Copyright (c) 2019 carrotflakes (carrotflakes@gmail.com)
|#

#|
  Author: carrotflakes (carrotflakes@gmail.com)
|#

(defsystem "shot"
  :version "0.1.0"
  :author "carrotflakes"
  :license "LLGPL"
  :depends-on ("trivia"
               "snaky")
  :components ((:module "src"
                :components
                ((:file "shot" :depends-on ("parser" "transpile"))
                 (:file "parser")
                 (:file "transpile" :depends-on ("condition"))
                 (:file "condition"))))
  :description ""
  :long-description
  #.(read-file-string
     (subpathname *load-pathname* "README.markdown"))
  :in-order-to ((test-op (test-op "shot-test"))))

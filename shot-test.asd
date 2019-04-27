#|
  This file is a part of shot project.
  Copyright (c) 2019 carrotflakes (carrotflakes@gmail.com)
|#

(defsystem "shot-test"
  :defsystem-depends-on ("prove-asdf")
  :author "carrotflakes"
  :license "LLGPL"
  :depends-on ("shot"
               "prove")
  :components ((:module "tests"
                :components
                ((:test-file "shot"))))
  :description "Test system for shot"

  :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c)))

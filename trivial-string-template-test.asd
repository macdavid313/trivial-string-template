#|
  This file is a part of trivial-string-template project.
  Copyright (c) 2016 David Gu (macdavid313@gmail.com)
|#

(in-package :cl-user)
(defpackage trivial-string-template-test-asd
  (:use :cl :asdf))
(in-package :trivial-string-template-test-asd)

(defsystem trivial-string-template-test
  :author "David Gu"
  :license "MIT"
  :depends-on (:alexandria
               :trivial-string-template
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "trivial-string-template"))))
  :description "Test system for trivial-string-template"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))

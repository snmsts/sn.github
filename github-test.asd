#|
  This file is a part of sn.github project.
  Copyright (c) 2015 SANO Masatoshi (snmsts@gmail.com)
|#

(in-package :cl-user)
(defpackage sn.github-test-asd
  (:use :cl :asdf))
(in-package :sn.github-test-asd)

(defsystem sn.github-test
  :author "SANO Masatoshi"
  :license "MIT"
  :depends-on (:sn.github
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "sn.github"))))
  :description "Test system for sn.github"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))

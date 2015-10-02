#|
  This file is a part of sn.github project.
  Copyright (c) 2015 SANO Masatoshi (snmsts@gmail.com)
|#

#|
  Author: SANO Masatoshi (snmsts@gmail.com)
|#

(in-package :cl-user)
(defpackage sn.github-asd
  (:use :cl :asdf))
(in-package :sn.github-asd)

(defsystem sn.github
  :version "0.1"
  :author "SANO Masatoshi"
  :license "MIT"
  :depends-on (:cl-gists :jonathan :quri)
  :components ((:module "src"
                :components
                ((:file "release"))))
  :description ""
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.md"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op sn.github-test))))

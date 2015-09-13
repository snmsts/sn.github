(in-package :cl-user)
(defpackage github-test
  (:use :cl
        :github
        :prove))
(in-package :github-test)

;; NOTE: To run this test file, execute `(asdf:test-system :github)' in your Lisp.

(plan nil)

;; blah blah blah.

(finalize)

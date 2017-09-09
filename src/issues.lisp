(in-package :cl-user)
(defpackage sn.github.issues
  (:use :cl))

(in-package :sn.github.issues)

(defun issues (&optional (as :plist))
    (jonathan:parse (cl-gists.util:get-request
                     (quri:uri (format nil "~A/issues"
                                       cl-gists.api::+api-base-uri+)))
                    :as as))

(defun user/issues (&optional (as :plist))
  (jonathan:parse (cl-gists.util:get-request
                   (quri:uri (format nil "~A/user/issues"
                                     cl-gists.api::+api-base-uri+)))
                  :as as))

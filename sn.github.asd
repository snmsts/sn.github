(defsystem sn.github
  :version "0.1"
  :author "SANO Masatoshi"
  :license "MIT"
  :depends-on (:cl-gists :jonathan :quri)
  :components ((:module "src"
                :components
                ((:file "repos.releases")
                 (:file "issues"))))
  :description ""
  :long-description
  #.(uiop:read-file-string (merge-pathnames
                            #p"README.md" (or *load-pathname* *compile-file-pathname*)))
  :in-order-to ((test-op (test-op :sn.github/test))))

(asdf:defsystem sn.github/test
  :class :package-inferred-system
  :defsystem-depends-on (:rove)
  :depends-on (:sn.github)
  :components ((:file "test/github"))
  :perform (test-op :after (o c)
                    (uiop:symbol-call :rove :run :sn.github/test)))

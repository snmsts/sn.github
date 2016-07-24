(in-package :cl-user)
(defpackage sn.github.repos.release
  (:use :cl)
  (:export :releases-list
           :release-by-id
           :release-latest
           :release-by-tag
           :release-create
           :release-edit
           :release-delete
           :release-assets-list
           :release-asset-upload
           :release-asset-get
           :release-asset-edit
           :release-asset-delete))

(in-package :sn.github.repos.release)

(defun releases-list (owner repo &optional (as :plist))
  (jonathan:parse (cl-gists.util:get-request (quri:uri (format nil "~A/repos/~A/~A/releases"
                                                               cl-gists.api::+api-base-uri+   
                                                               owner
                                                               repo)))
                  :as as))

(defun release-by-id (owner repo id &optional (as :plist))
  (jonathan:parse (cl-gists.util:get-request (quri:uri (format nil "~A/repos/~A/~A/releases/~A"
                                                               cl-gists.api::+api-base-uri+
                                                               owner
                                                               repo id)))
                  :as as))

(defun release-latest (owner repo &optional (as :plist))
  (jonathan:parse (cl-gists.util:get-request
                   (quri:uri (format nil "~A/repos/~A/~A/releases/latest"
                                     cl-gists.api::+api-base-uri+
                                     owner
                                     repo)))
                  :as as))

(defun release-by-tag (owner repo tag &optional (as :plist))
  (jonathan:parse (cl-gists.util:get-request
                   (quri:uri (format nil "~A/repos/~A/~A/releases/tags/~A"
                                     cl-gists.api::+api-base-uri+
                                     owner
                                     repo
                                     tag)))
                  :as as))

(defun release-create (owner repo tagname &key commitish (name "") (body "") draft prerelease (as :plist))
  (let* ((uri (quri:uri (format nil "~A/repos/~A/~A/releases"
                                cl-gists.api::+api-base-uri+
                                owner
                                repo)))
         (to-json `(("tag_name" . ,tagname)
                    ,@(when commitish `(("target_commitish" . ,commitish)))
                    ("name" . ,name)
                    ("body" . ,body)
                    ("draft" . ,(if draft t :false))
                    ("prerelease" . ,(if prerelease t :false))))
         (content (jonathan:to-json to-json :from :alist))
         (cl-gists:*credentials* t))
    (jonathan:parse (cl-gists.util:post-request uri :content content)
                    :as as)))

(defun release-edit (owner repo id tagname &key commitish (name "") (body "") draft prerelease (as :plist))
  (let* ((uri (quri:uri (format nil "~A/repos/~A/~A/releases/~A"
                                cl-gists.api::+api-base-uri+
                                owner
                                repo
                                id)))
         (to-json `(("tag_name" . ,tagname)
                    ,@(when commitish `(("target_commitish" . ,commitish)))
                    ("name" . ,name)
                    ("body" . ,body)
                    ("draft" . ,(if draft t :false))
                    ("prerelease" . ,(if prerelease t :false))))
         (content (jonathan:to-json to-json :from :alist))
         (cl-gists:*credentials* t))
    (jonathan:parse (cl-gists.util:patch-request uri :content content) :as as)))

(defun release-delete (owner repo id &optional (as :plist))
  (let ((uri (quri:uri (format nil "~A/repos/~A/~A/releases/~A"
                               cl-gists.api::+api-base-uri+
                               owner
                               repo
                               id)))
        (cl-gists:*credentials* t))
    (jonathan:parse (cl-gists.util:delete-request uri) :as as)))

(defun release-assets-list (owner repo id &optional (as :plist))
  (let ((uri (quri:uri (format nil "~A/repos/~A/~A/releases/~A/assets"
                               cl-gists.api::+api-base-uri+
                               owner
                               repo
                               id)))
        (cl-gists:*credentials* t))
    (jonathan:parse (cl-gists.util:get-request uri) :as as)))

(defun release-asset-upload (owner repo id file &key name label (as :plist))
  (let ((upload-uri (getf (release-by-id owner repo id) :|upload_url|)))
    (setf upload-uri (quri:uri (subseq upload-uri 0 (position #\{ upload-uri))))
    (setf (quri:uri-query-params upload-uri)
          `(("name" . ,(or name (file-namestring file)))
            ,@(when label `(("label" . ,label)))))
    (jonathan:parse (cl-gists.util:post-request upload-uri :content file) :as as)))

(defun release-asset-get (owner repo id &optional (as :plist))
  (let ((uri (quri:uri (format nil "~A/repos/~A/~A/releases/assets/~A"
                               cl-gists.api::+api-base-uri+
                               owner
                               repo
                               id))))
    (jonathan:parse (cl-gists.util:get-request uri) :as as)))

(defun release-asset-edit (owner repo id name &key label (as :plist))
  (let* ((uri (quri:uri (format nil "~A/repos/~A/~A/releases/assets/~A"
                                cl-gists.api::+api-base-uri+
                                owner
                                repo
                                id)))
         (to-json `(("name" . ,name)
                    ,@(when label `(("label" . ,label)))))
         (content (jonathan:to-json to-json :from :alist))
         (cl-gists:*credentials* t))
    (jonathan:parse (cl-gists.util:patch-request uri :content content) :as as)))

(defun release-asset-delete (owner repo id &optional (as :plist))
  (let ((uri (quri:uri (format nil "~A/repos/~A/~A/releases/assets/~A"
                               cl-gists.api::+api-base-uri+
                               owner
                               repo
                               id)))
        (cl-gists:*credentials* t))
    (jonathan:parse (cl-gists.util:delete-request uri) :as as)))

(in-package :cl-user)
(defpackage sn.github.repos.releases
  (:use :cl)
  (:export :releases-list
           :releases-by-id
           :releases-latest
           :releases-by-tag
           :releases-create
           :releases-edit
           :releases-delete
           :releases-assets-list
           :releases-asset-upload
           :releases-asset-get
           :releases-asset-edit
           :releases-asset-delete))

(in-package :sn.github.repos.releases)

(defun releases-list (owner repo &optional (as :plist))
  (jonathan:parse (cl-gists.util:get-request (quri:uri (format nil "~A/repos/~A/~A/releases"
                                                               cl-gists.api::+api-base-uri+   
                                                               owner
                                                               repo)))
                  :as as))

(defun releases-by-id (owner repo id &optional (as :plist))
  (jonathan:parse (cl-gists.util:get-request (quri:uri (format nil "~A/repos/~A/~A/releases/~A"
                                                               cl-gists.api::+api-base-uri+
                                                               owner
                                                               repo id)))
                  :as as))

(defun releases-latest (owner repo &optional (as :plist))
  (jonathan:parse (cl-gists.util:get-request
                   (quri:uri (format nil "~A/repos/~A/~A/releases/latest"
                                     cl-gists.api::+api-base-uri+
                                     owner
                                     repo)))
                  :as as))

(defun releases-by-tag (owner repo tag &optional (as :plist))
  (jonathan:parse (cl-gists.util:get-request
                   (quri:uri (format nil "~A/repos/~A/~A/releases/tags/~A"
                                     cl-gists.api::+api-base-uri+
                                     owner
                                     repo
                                     tag)))
                  :as as))

(defun releases-create (owner repo tagname &key commitish (name "") (body "") draft prerelease (as :plist))
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

(defun releases-edit (owner repo id tagname &key commitish (name "") (body "") draft prerelease (as :plist))
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

(defun releases-delete (owner repo id &optional (as :plist))
  (let ((uri (quri:uri (format nil "~A/repos/~A/~A/releases/~A"
                               cl-gists.api::+api-base-uri+
                               owner
                               repo
                               id)))
        (cl-gists:*credentials* t))
    (jonathan:parse (cl-gists.util:delete-request uri) :as as)))

(defun releases-assets-list (owner repo id &optional (as :plist))
  (let ((uri (quri:uri (format nil "~A/repos/~A/~A/releases/~A/assets"
                               cl-gists.api::+api-base-uri+
                               owner
                               repo
                               id)))
        (cl-gists:*credentials* t))
    (loop with json
          with status
          with headers
          do (multiple-value-setq (json status headers) (cl-gists.util:get-request uri))
             (setf status
                   (ignore-errors
                    (second (assoc "rel=\"next" (mapcar (lambda (x) (reverse (mapcar (lambda (x) (string-trim " <>\"" x)) (split-sequence:split-sequence #\; x)))) (split-sequence:split-sequence #\, (gethash "link" headers))) :test 'equal))))
             (when status
               (setf uri (quri:uri status)))
          append (jonathan:parse json :as as)
          while status)))

(defun releases-asset-upload (owner repo id file &key name label (as :plist))
  (let ((upload-uri (getf (releases-by-id owner repo id) :|upload_url|)))
    (setf upload-uri (quri:uri (subseq upload-uri 0 (position #\{ upload-uri))))
    (setf (quri:uri-query-params upload-uri)
          `(("name" . ,(or name (file-namestring file)))
            ,@(when label `(("label" . ,label)))))
    (jonathan:parse (cl-gists.util:post-request upload-uri :content file) :as as)))

(defun releases-asset-get (owner repo id &optional (as :plist))
  (let ((uri (quri:uri (format nil "~A/repos/~A/~A/releases/assets/~A"
                               cl-gists.api::+api-base-uri+
                               owner
                               repo
                               id))))
    (jonathan:parse (cl-gists.util:get-request uri) :as as)))

(defun releases-asset-edit (owner repo id name &key label (as :plist))
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

(defun releases-asset-delete (owner repo id &optional (as :plist))
  (let ((uri (quri:uri (format nil "~A/repos/~A/~A/releases/assets/~A"
                               cl-gists.api::+api-base-uri+
                               owner
                               repo
                               id)))
        (cl-gists:*credentials* t))
    (jonathan:parse (cl-gists.util:delete-request uri) :as as)))

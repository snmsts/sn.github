(in-package :cl-user)
(defpackage sn.github.release
  (:use :cl))
(in-package :sn.github.release)

(defun releases-list (owner repo)
  (jonathan:parse(cl-gists.util:get-request (quri:uri (format nil "~A/repos/~A/~A/releases"
                                               cl-gists.api::+api-base-uri+   
                                               owner
                                               repo)))))

(defun release-by-id (owner repo id)
  (jonathan:parse (cl-gists.util:get-request (quri:uri (format nil "~A/repos/~A/~A/releases/~A"
                                               cl-gists.api::+api-base-uri+
                                               owner
                                               repo id)))))

(defun release-latest (owner repo)
  (jonathan:parse (cl-gists.util:get-request
                   (quri:uri (format nil "~A/repos/~A/~A/releases/latest"
                                     cl-gists.api::+api-base-uri+
                                     owner
                                     repo)))))

(defun release-by-tag (owner repo tag)
  (jonathan:parse (cl-gists.util:get-request
                   (quri:uri (format nil "~A/repos/~A/~A/releases/tags/~A"
                                     cl-gists.api::+api-base-uri+
                                     owner
                                     repo
                                     tag)))))

(defun release-create (owner repo tagname &key commitish (name "") (body "") draft prerelease)
  (let ((uri (quri:uri (format nil "~A/repos/~A/~A/releases"
                               cl-gists.api::+api-base-uri+
                               owner
                               repo)))
        (content (jonathan:to-json `(("tag_name" . ,tagname)
                                     ,@(when commitish `(("target_commitish" . ,commitish)))
                                     ("name" . ,name)
                                     ("body" . ,body)
                                     ("draft" . ,(if draft t :false))
                                     ("prerelease" . ,(if prerelease t :false)))
                                   :from :alist))
        (cl-gists:*credentials* t))
    (jonathan:parse (cl-gists.util:post-request uri :content content))))

(defun release-edit (owner repo id tagname &key commitish (name "") (body "") draft prerelease)
  (let ((uri (quri:uri (format nil "~A/repos/~A/~A/releases/~A"
                               cl-gists.api::+api-base-uri+
                               owner
                               repo
                               id)))
        (content (jonathan:to-json `(("tag_name" . ,tagname)
                                     ,@(when commitish `(("target_commitish" . ,commitish)))
                                     ("name" . ,name)
                                     ("body" . ,body)
                                     ("draft" . ,(if draft t :false))
                                     ("prerelease" . ,(if prerelease t :false)))
                                   :from :alist))
        (cl-gists:*credentials* t))
    (jonathan:parse (cl-gists.util:patch-request uri :content content))))

(defun release-delete (owner repo id)
  (let ((uri (quri:uri (format nil "~A/repos/~A/~A/releases/~A"
                               cl-gists.api::+api-base-uri+
                               owner
                               repo
                               id)))
        (cl-gists:*credentials* t))
    (jonathan:parse (cl-gists.util:delete-request uri))))

(defun release-assets-list (owner repo id)
  (let ((uri (quri:uri (format nil "~A/repos/~A/~A/releases/~A/assets"
                               cl-gists.api::+api-base-uri+
                               owner
                               repo
                               id)))
        (cl-gists:*credentials* t))
    (jonathan:parse (cl-gists.util:get-request uri))))

(defun release-asset-upload (owner repo id file &key name label)
  (let ((upload-uri (getf (release-by-id owner repo id) :|upload_url|)))
    (setf upload-uri (quri:uri (subseq upload-uri 0 (position #\{ upload-uri))))
    (setf (quri:uri-query-params upload-uri)
          `(("name" . ,(or name (file-namestring file)))
            ,@(when label `(("label" . ,label)))))
    upload-uri
    (cl-gists.util:post-request upload-uri :content file)))

(defun release-asset-get (owner repo id)
  (let ((uri (quri:uri (format nil "~A/repos/~A/~A/releases/assets/~A"
                               cl-gists.api::+api-base-uri+
                               owner
                               repo
                               id))))
    (jonathan:parse (cl-gists.util:get-request uri))))

(defun release-asset-edit (owner repo id name &key label)
  (let ((uri (quri:uri (format nil "~A/repos/~A/~A/releases/assets/~A"
                               cl-gists.api::+api-base-uri+
                               owner
                               repo
                               id)))
        (content (jonathan:to-json `(("name" . ,name)
                                     ,@(when label `(("label" . ,label))))
                                   :from :alist))
        (cl-gists:*credentials* t))
    (jonathan:parse (cl-gists.util:patch-request uri :content content))))

(defun release-asset-delete (owner repo id)
  (let ((uri (quri:uri (format nil "~A/repos/~A/~A/releases/assets/~A"
                               cl-gists.api::+api-base-uri+
                               owner
                               repo
                               id)))
        (cl-gists:*credentials* t))
    (jonathan:parse (cl-gists.util:delete-request uri))))

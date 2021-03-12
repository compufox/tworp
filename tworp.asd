;;;; tworp.asd

(asdf:defsystem #:tworp
  :description "mastodon bot that mirrors a twitter account"
  :author "ava fox"
  :license  "NPLv1+"
  :version "0.1"
  :serial t
  :depends-on (#:glacier #:chirp #:simple-config #:with-user-abort #:unix-opts #:cl-ppcre)
  :components ((:file "tworp"))
  :entry-point "tworp:main"
  :build-operation "program-op"
  :build-pathname "bin/tworp")

;; dont compress on windows, so we dont have to deal with zlib DLLs
#+(and sb-core-compression (not win32))
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c) :executable t :compression t))

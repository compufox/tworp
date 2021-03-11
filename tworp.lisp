;;;; tworp.lisp

(defpackage #:tworp
  (:use #:cl #:with-user-abort)
  (:export :main))

(in-package #:tworp)

(declaim (inline post-to-mastodon))

(defvar *last-id* nil)

(unix-opts:define-opts
  (:name :help
   :description "print this help text"
   :short #\h
   :long "help")
  (:name :config-file
   :description "config file FILE to load"
   :short #\c
   :long "config"
   :arg-parser #'identity
   :meta-var "FILE"))

(defmacro when-option ((options opt) &body body)
  `(let ((it (getf ,options ,opt)))
     (declare (ignorable it))
     (when it
       ,@body)))

(defun new-tweets ()
  (let ((tweets (chirp:statuses/user-timeline :screen-name (conf:config :twitter-user) :since-id *last-id*)))
    (when tweets
      (setf *last-id* (chirp:id (first tweets)))
      (write-last-id))
    tweets))

(defun write-last-id ()
  (with-open-file (out "last.id" :direction :output
                                 :if-does-not-exist :create
                                 :if-exists :supersede)
    (princ *last-id* out)))

(defun post-to-mastodon (tweet)
  (when tweet
    (glacier:post (ppcre:regex-replace-all "@(\\w*)" (chirp:text tweet) "@\\1@twitter.com")
                  :cw (conf:config :content-warning "twitter crosspost")
                  :visibility (conf:config :visibility :unlisted)
                  :media (mapcar #'download-tweet-media (cdr (assoc :media (chirp:entities tweet)))))))

(defun download-tweet-media (media)
  (when media
    (let* ((url (chirp:media-url-https media))
           (filename (concatenate 'string "media/" (pathname-name url) "." (pathname-type url))))
      (with-open-file (out filename :direction :output
                                    :if-does-not-exist :create
                                    :if-exists :supersede
                                    :element-type '(unsigned-byte 8))
        (loop :with input := (drakma:http-request (chirp:media-url-https media) :want-stream t)
              :for byte := (read-byte input nil nil)
              :while byte
              :do (write-byte byte out)))
      (merge-pathnames filename (uiop:getcwd)))))

(defun main ()
  (when (uiop:file-exists-p "last.id")
    (with-open-file (in "last.id")
      (setf *last-id* (read-line in))))
      
  (multiple-value-bind (options free-args)
      (handler-case (opts:get-opts)
        (opts:missing-arg (condition)
          (format t "fatal: option ~s needs an argument!~%"
                  (opts:option condition))
          (opts:exit 1))
        (opts:arg-parser-failed (condition)
          (format t "fatal: cannot parse ~s as argument of ~s~%"
                  (opts:raw-arg condition)
                  (opts:option condition))
          (opts:exit 1))
        (opts:missing-required-option (con)
          (format t "fatal: ~a~%" con)
          (opts:exit 1)))
    (declare (ignorable free-args))

    (when-option (options :help)
      (unix-opts:describe :prefix "repost tweets from twitter to mastodon"
                          :usage-of "tworp")
      (opts:exit 0))
    
    (handler-case
        (with-user-abort
             (glacier:run-bot ((make-instance 'glacier:mastodon-bot :config-file
                                              (getf options :config-file))
                               :with-websocket nil)
               (unless (or chirp:*oauth-api-key* chirp:*oauth-api-secret*)
                 (setf chirp:*oauth-api-key* (conf:config :twitter-api-key)
                       chirp:*oauth-api-secret* (conf:config :twitter-api-secret)))
               (ensure-directories-exist #P"./media/")
               (glacier:after-every ((conf:config :interval 5) :minutes :run-immediately t)
                 (mapcar #'post-to-mastodon (new-tweets)))))
      (user-abort ()
        (unix-opts:exit 0))
      (error (e)
        (format t "encountered uncrecoverable error: ~A~%" e)))))

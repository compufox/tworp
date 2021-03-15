;;;; tworp.lisp

(defpackage #:tworp
  (:use #:cl #:with-user-abort)
  (:export :main))

(in-package #:tworp)

(declaim (inline post-to-mastodon generate-link write-last-id
                 new-tweets cache-id))

(defvar *last-id* nil)
(defvar *tweet-buffer* nil)

(opts:define-opts
  (:name :help
   :description "print this help text"
   :short #\h
   :long "help")
  (:name :config-file
   :description "config file FILE to load"
   :short #\c
   :long "config"
   :arg-parser #'identity
   :meta-var "FILE")
  (:name :version
   :description "prints the application version"
   :long "version"))

(defun generate-link (tweet)
  "generates a twitter link to TWEET"
  (format nil "https://twitter.com/~A/status/~A"
          (chirp:screen-name (chirp:user tweet))
          (chirp:id tweet)))
          
(defun new-tweets ()
  "fetches new tweets that have appeared since *last-id*"
  (chirp:statuses/user-timeline :screen-name (conf:config :twitter-user) :since-id *last-id* :tweet-mode "extended"))

(defun cache-id (tweet)
  "cache and save ID for TWEET"
  (setf *last-id* (chirp:id tweet))
  (write-last-id))

(defun write-last-id ()
  "saves *last-id* to a last.id file"
  (with-open-file (out "last.id" :direction :output
                                 :if-does-not-exist :create
                                 :if-exists :supersede)
    (princ *last-id* out)))

(defun post-to-mastodon (tweet)
  "posts TWEET to mastodon"
  (when tweet
    ;; the regex-replace-all turns all twitter @mentions into @mention@twitter.com
    ;;  this is to avoid name collision with anyone on masto
    (glacier:post (format nil "~A~%~%Source: ~A"
                          (ppcre:regex-replace-all "@(\\w*)" (chirp:xml-decode (chirp:full-text tweet))
                                                   "@\\1@twitter.com")
                          (generate-link tweet))
                  :cw (conf:config :content-warning "twitter crosspost")
                  :visibility (conf:config :visibility :unlisted)
                  :media (mapcar #'download-tweet-media (cdr (assoc :media (chirp:entities tweet)))))
    ;; once we post the tweet, we cache the ID so we dont post it again
    (cache-id tweet)))

(defun download-tweet-media (media)
  "downloads MEDIA and saves it to the filesystem for crossposting"
  (when media
    (let* ((url (chirp:media-url-https media))
           (filename (concatenate 'string "media/" (pathname-name url) "." (pathname-type url))))
      (with-open-file (out filename :direction :output
                                    :if-does-not-exist :create
                                    :if-exists :supersede
                                    :element-type '(unsigned-byte 8))
        ;; fetch and save our media to the hard drive
        (loop :with input := (drakma:http-request (chirp:media-url-https media) :want-stream t)
              :for byte := (read-byte input nil nil)
              :while byte
              :do (write-byte byte out)))
      ;; return the full file path to the newly downloaded media
      (merge-pathnames filename (uiop:getcwd)))))

(defun main ()
  "binary entry point"
  (macrolet ((when-option ((options opt) &body body)
              `(let ((it (getf ,options ,opt)))
                 (declare (ignorable it))
                 (when it
                   ,@body)))
             (append! (place &rest lists)
               `(setf ,place (append ,place ,@lists))))
    ;; when a "last.id" file exists load the contents
    ;;  (last.id file only contains the ID of the last
    ;;   tweet we saw. this allows for persistency
    ;;   across reboots of the application)
    (when (uiop:file-exists-p "last.id")
      (with-open-file (in "last.id")
        (setf *last-id* (read-line in))))

    ;; parse all of the options we got passed 
    (multiple-value-bind (options free-args)
        (handler-case (opts:get-opts)
          (opts:missing-arg (condition)
            (format t "fatal: option ~s needs an argument!"
                    (opts:option condition))
            (opts:exit 1))
          (opts:arg-parser-failed (condition)
            (format t "fatal: cannot parse ~s as argument of ~s"
                    (opts:raw-arg condition)
                    (opts:option condition))
            (opts:exit 1))
          (opts:missing-required-option (con)
            (format t "fatal: ~a" con)
            (opts:exit 1))
          (opts:unknown-option (con)
            (format t "fatal: ~A" con)
            (opts:exit 1)))
      (declare (ignorable free-args))

      ;; when the user passes -h/--help
      (when-option (options :help)
        (opts:describe :prefix "repost tweets from twitter to mastodon"
                       :usage-of "tworp")
        (opts:exit 0))

      ;; when the user passed --version
      (when-option (options :version)
        (format t "tworp v~A" 
                ;; the #. allows this value to be calculated at compile time
                ;;  instead of run time (making this WAY faster lmao)
                #.(asdf:component-version (asdf:find-system :tworp)))
        (opts:exit 0))

      
      (handler-case
          (with-user-abort
               (glacier:run-bot ((make-instance 'glacier:mastodon-bot :config-file
                                                (getf options :config-file))
                                 :with-websocket nil)
                 
                 ;; if we dont have our twitter api keys set then we load them from our config
                 ;;  (these get populated by the dev on a release build)
                 (unless (or chirp:*oauth-api-key* chirp:*oauth-api-secret*)
                   (setf chirp:*oauth-api-key* (conf:config :twitter-api-key)
                         chirp:*oauth-api-secret* (conf:config :twitter-api-secret)))

                 ;; make sure our media directory exists
                 (ensure-directories-exist #P"./media/")

                 ;; in a separate thread fetch new tweets as dictated by our interval 
                 ;;  set up in the config
                 (glacier:after-every ((conf:config :interval 5) :minutes :run-immediately t :async t)
                   (append! *tweet-buffer* (new-tweets)))

                 ;; post to mastodon after each timeout, as dictated by our config
                 (glacier:after-every ((conf:config :timeout 1) :minutes)
                   (post-to-mastodon (pop *tweet-buffer*)))))

        ;; if a user stops us, exit gracefully
        (user-abort ()
          (opts:exit 0))

        ;; if we hit an issue, tell the user and exit gracefully
        (error (e)
          (format t "encountered uncrecoverable error: ~A" e))))))
